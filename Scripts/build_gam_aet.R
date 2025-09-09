# Load necessary libraries
library(here)
library(vroom)
library(caret)
library(igraph)
library(mgcv)
library(parallel)
library(dplyr)
library(readr)
# library(corrplot)
library(utils)

# sink(here("build_gam_aet_log.txt"))

# Load the dataset
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
tamarack_df <- vroom(here("data", "tamarack_fire", "tamarack_fire_model_df_outliers_removed.csv"))
knp_complex_df <- vroom(here("data", "knp_complex_fire", "knp_complex_fire_model_df_outliers_removed.csv"))

num_cores <- detectCores() - 1

# Define predictor variables
# I'm going to omit lat and lon right off the bat since I don't want to
# accidentally represent climate variation by location that doesn't
# actually depend on lat and lon
predictor_vars <- c("prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
                    "tmax", "precip",  "vpd", "pdsi", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and valid sets
set.seed(2024)
train_index <- createDataPartition(model_df$aet, p = 0.7, list = FALSE)
train_df <- model_df[train_index, ]
valid_df <- model_df[-train_index, ]

# Preprocess the data
preproc_values <- preProcess(train_df[, predictor_vars], method = c("center", "scale"))
train_df_scaled <- train_df
train_df_scaled[, predictor_vars] <- predict(preproc_values, train_df[, predictor_vars])
valid_df_scaled <- valid_df
valid_df_scaled[, predictor_vars] <- predict(preproc_values, valid_df[, predictor_vars])

tamarack_df_scaled <- tamarack_df
tamarack_df_scaled[, predictor_vars] <- predict(preproc_values, tamarack_df[, predictor_vars])
knp_complex_df_scaled <- knp_complex_df
knp_complex_df_scaled[, predictor_vars] <- predict(preproc_values, knp_complex_df[, predictor_vars])

############################# Variable Selection ###############################

# Check for multicollinearity in the training set
train_predictors <- train_df_scaled[, predictor_vars]
train_cor_matrix <- cor(train_predictors, use = "complete.obs")
# corrplot(train_cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Identify highly correlated predictors (correlation > 0.7)
high_corr <- findCorrelation(train_cor_matrix, cutoff = 0.7)
corr_removed_vars <- predictor_vars[high_corr]
print(paste("Variables to consider removing due to high correlation:", paste(corr_removed_vars, collapse = ", ")))
# corrplot(abs(train_cor_matrix) > 0.7)
# Define conflict pairs based on high correlations
conflict_pairs <- list(
    c("prefire_ndvi", "prefire_nbr"),
    c("dndvi", "dnbr"),
    c("dndvi", "rdnbr"),
    c("dnbr", "rdnbr"),
    c("tmax", "vpd"),
    c("tmax", "elev"),
    c("precip", "soil_moisture"),
    c("vpd", "elev")
)

# Create a conflict graph
g <- make_empty_graph(n = 0, directed = FALSE)
g <- add_vertices(g, length(predictor_vars), name = predictor_vars)
for (pair in conflict_pairs) {
    g <- add_edges(g, pair)
}
# Visualize the conflict graph
plot(g, vertex.label.cex = 0.8, main = "Conflict Graph for Predictors")

# Find maximal independent sets
g_complement <- complementer(g)
max_indep_sets <- max_cliques(g_complement)
indep_sets <- lapply(max_indep_sets, function(x) V(g_complement)[x]$name)

# Fit models for each independent set
model_performance <- list()
for (i in seq_along(indep_sets)) {
  current_set <- indep_sets[[i]]

  # Print
  print(paste("Fitting model", i, "with predictors:", current_set))

  # Create a formula for the current model
  formula_str <- paste("aet ~", paste(paste0("s(", current_set, ", k=10, bs='cs')"), collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Fit the linear regression model on the training set
  model <- bam(model_formula, data = train_df_scaled,
               method="fREML", nthreads = num_cores)

  # Predict on the validation set
  valid_predictions <- predict(model, newdata = valid_df_scaled)
  knp_complex_predictions <- predict(model, newdata = knp_complex_df_scaled)
  tamarack_predictions <- predict(model, newdata = tamarack_df_scaled)

  # Calculate performance metrics for validation set
  valid_rmse <- sqrt(mean((valid_df_scaled$aet - valid_predictions)^2))
  valid_r2 <- cor(valid_df_scaled$aet, valid_predictions)^2

  knp_complex_rmse <- sqrt(mean((knp_complex_df_scaled$aet - knp_complex_predictions)^2))
  knp_complex_r2 <- cor(knp_complex_df_scaled$aet, knp_complex_predictions)^2

  tamarack_rmse <- sqrt(mean((tamarack_df_scaled$aet - tamarack_predictions)^2))
  tamarack_r2 <- cor(tamarack_df_scaled$aet, tamarack_predictions)^2

  # Store the performance metrics along with the predictors
  model_performance[[i]] <- list(
    model_id = i,
    predictors = current_set,
    valid_rmse = valid_rmse,
    valid_r2 = valid_r2,
    knp_complex_rmse = knp_complex_rmse,
    knp_complex_r2 = knp_complex_r2,
    tamarack_rmse = tamarack_rmse,
    tamarack_r2 = tamarack_r2
  )

  # Print
  print(paste("Validation R2:", valid_r2))
}

# Convert the list to a data frame
performance_df <- bind_rows(model_performance)

# Save the df as csv
write_csv(performance_df, here("data", "creek_fire", "creek_fire_aet_gam_model_performance.csv"))
# 
# # Read it
performance_df <- read_csv(here("data", "creek_fire", "creek_fire_aet_gam_model_performance.csv"))

# Find the best model based on validation R2
best_valid_r2 <- performance_df %>% arrange(desc(valid_r2)) %>% slice(1)
best_model_predictors <- performance_df[performance_df$model_id == as.numeric(best_valid_r2$model_id),]$predictors

# Recreate and fit the best base model
best_model_formula <- paste("aet ~", paste(paste0("s(", best_model_predictors, ", k=10, bs='ts')"), collapse = " + "))

creek_fire_aet_gam_base_k10 <- bam(as.formula(best_model_formula), data = train_df_scaled,
                  method="fREML", nthreads = num_cores, discrete = TRUE)

# Save the model
save(creek_fire_aet_gam_base_k10, file = here("models", "creek_fire", "creek_fire_aet_gam_base_k10.RData"))

print("creek_fire_aet_gam_base_k10 info")
print(creek_fire_aet_gam_base_k10)

summary(creek_fire_aet_gam_base_k10)

# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   ret ~ s(prefire_nbr, k = 30, bs = "ts") + s(aspect, k = 30, bs = "ts") + 
#   s(northness, k = 30, bs = "ts") + s(slope, k = 30, bs = "ts") + 
#   s(precip, k = 30, bs = "ts") + s(elev, k = 30, bs = "ts") + 
#   s(dnbr, k = 30, bs = "ts")
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 47.22577    0.01249    3780   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df       F p-value    
# s(prefire_nbr) 21.21     29 15309.4  <2e-16 ***
#   s(aspect)      20.90     29  1243.3  <2e-16 ***
#   s(northness)   17.83     29   241.6  <2e-16 ***
#   s(slope)       17.12     29   411.7  <2e-16 ***
#   s(precip)      28.92     29  4673.3  <2e-16 ***
#   s(elev)        28.66     29  2222.4  <2e-16 ***
#   s(dnbr)        28.14     29 79519.8  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.741   Deviance explained = 74.1%
# fREML = 4.7155e+06  Scale est. = 180.03    n = 1174220

# # Try increasing k for precip, elev, and dnbr
# best_model_formula <- aet ~ s(prefire_nbr, k=25, bs='ts') + s(aspect, k=20, bs='ts') +
#   s(northness, k=20, bs='ts') + s(slope, k=20, bs='ts') +
#   s(precip, k=100, bs='ts') + s(elev, k=100, bs='ts') +
#   s(dnbr, k=50, bs='ts')
# 
# # Try fitting the model again
# best_model <- bam(best_model_formula, data = train_df_scaled,
#                   method="fREML", nthreads=num_cores)
# 
# gam.check(best_model)
# # Save it again
# save(best_model, file = here("models", "aet_gam_base_adj_k.RData"))

########################### Try Adding Interactions ############################

# Generate two-way interactions for the best base model
two_way_interactions <- combn(best_model_predictors, 2, function(x) paste(x, collapse = ","))
all_terms <- c(best_model_predictors, two_way_interactions)
interaction_formula <- as.formula(paste("aet ~",
                                        paste(c(paste0("s(", best_model_predictors, ", k=10, bs='ts')"),
                                          paste0("te(", two_way_interactions, ", k=10, bs='ts')")),
                                        collapse = " + ")))


# Fit the full interaction model
creek_fire_aet_gam_interaction_k10 <- bam(interaction_formula, data = train_df_scaled,
                              method="fREML", nthreads=num_cores, discrete = TRUE)
# Save the model
save(creek_fire_aet_gam_interaction_k10, file = here("models", "creek_fire", "creek_fire_aet_gam_interaction_k10.RData"))

# Print so I have a record of which terms were significant
print("creek_fire_aet_gam_interaction_k10 info")
print(creek_fire_aet_gam_interaction_k10)

summary(creek_fire_aet_gam_interaction_k10)

# Reduced interaction gam: same as full, but remove non-significant terms;
# aspect, northness, precip, and northness:slope
# best_model_predictors_reduced <- setdiff(best_model_predictors, c("aspect", "northness", "precip"))
best_model_predictors_reduced <- best_model_predictors
two_way_interactions_reduced <- setdiff(two_way_interactions, c("precip,pdsi","precip,elev"))

# Formula
reduced_interaction_formula <- as.formula(paste("aet ~",
                                                paste(c(paste0("s(", best_model_predictors_reduced, ", k=10, bs='ts')"),
                                                        paste0("te(", two_way_interactions_reduced, ", k=10, bs='ts')")),
                                                      collapse = " + ")))
# Fit reduced gam
reduced_interaction_model <- bam(reduced_interaction_formula, data = train_df_scaled,
                                 method="fREML", nthreads=num_cores, discrete = TRUE)

# Save it
save(reduced_interaction_model, file = here("models", "creek_fire", "creek_fire_gam_interaction_reduced_k10.RData"))



###################### output ##################################################

# Family: gaussian
# Link function: identity
# 
# Formula:
#   aet ~ s(prefire_nbr, k = 10, bs = "ts") + s(precip, k = 10, bs = "ts") +
#   s(northness, k = 10, bs = "ts") + s(slope, k = 10, bs = "ts") +
#   s(aspect, k = 10, bs = "ts") + s(pdsi, k = 10, bs = "ts") +
#   s(elev, k = 10, bs = "ts") + s(dnbr, k = 10, bs = "ts")
# 
# Estimated degrees of freedom:
#   8.30 9.00 8.88 8.64 8.76 8.97 8.99
# 8.94  total = 71.48
# 
# fREML score: 5738294
# 
# Family: gaussian
# Link function: identity
# 
# Formula:
#   aet ~ s(prefire_nbr, k = 10, bs = "ts") + s(precip, k = 10, bs = "ts") +
#   s(northness, k = 10, bs = "ts") + s(slope, k = 10, bs = "ts") +
#   s(aspect, k = 10, bs = "ts") + s(pdsi, k = 10, bs = "ts") +
#   s(elev, k = 10, bs = "ts") + s(dnbr, k = 10, bs = "ts")
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  59.7229     0.1094   546.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F p-value
# s(prefire_nbr) 8.303      9   4917.5  <2e-16 ***
#   s(precip)      8.998      9  21733.2  <2e-16 ***
#   s(northness)   8.881      9    560.0  <2e-16 ***
#   s(slope)       8.637      9    349.7  <2e-16 ***
#   s(aspect)      8.763      9    936.7  <2e-16 ***
#   s(pdsi)        8.971      9   2839.2  <2e-16 ***
#   s(elev)        8.989      9   5386.8  <2e-16 ***
#   s(dnbr)        8.943      9 342081.8  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.809   Deviance explained = 80.9%
# fREML = 5.7383e+06  Scale est. = 212.43    n = 1400119
# [1] "creek_fire_aet_gam_interaction_k10 info"
# 
# Family: gaussian
# Link function: identity
# 
# Formula:
#   aet ~ s(prefire_nbr, k = 10, bs = "ts") + s(precip, k = 10, bs = "ts") +
#   s(northness, k = 10, bs = "ts") + s(slope, k = 10, bs = "ts") +
#   s(aspect, k = 10, bs = "ts") + s(pdsi, k = 10, bs = "ts") +
#   s(elev, k = 10, bs = "ts") + s(dnbr, k = 10, bs = "ts") +
#   te(prefire_nbr, precip, k = 10, bs = "ts") + te(prefire_nbr,
#                                                   northness, k = 10, bs = "ts") + te(prefire_nbr, slope, k = 10,
#                                                                                      bs = "ts") + te(prefire_nbr, aspect, k = 10, bs = "ts") +
#   te(prefire_nbr, pdsi, k = 10, bs = "ts") + te(prefire_nbr,
#                                                 elev, k = 10, bs = "ts") + te(prefire_nbr, dnbr, k = 10,
#                                                                               bs = "ts") + te(precip, northness, k = 10, bs = "ts") + te(precip,
#                                                                                                                                          slope, k = 10, bs = "ts") + te(precip, aspect, k = 10, bs = "ts") +
#   te(precip, pdsi, k = 10, bs = "ts") + te(precip, elev, k = 10,
#                                            bs = "ts") + te(precip, dnbr, k = 10, bs = "ts") + te(northness,
#                                                                                                  slope, k = 10, bs = "ts") + te(northness, aspect, k = 10,
#                                                                                                                                 bs = "ts") + te(northness, pdsi, k = 10, bs = "ts") + te(northness,
#                                                                                                                                                                                          elev, k = 10, bs = "ts") + te(northness, dnbr, k = 10, bs = "ts") +
#   te(slope, aspect, k = 10, bs = "ts") + te(slope, pdsi, k = 10,
#                                             bs = "ts") + te(slope, elev, k = 10, bs = "ts") + te(slope,
#                                                                                                  dnbr, k = 10, bs = "ts") + te(aspect, pdsi, k = 10, bs = "ts") +
#   te(aspect, elev, k = 10, bs = "ts") + te(aspect, dnbr, k = 10,
#                                            bs = "ts") + te(pdsi, elev, k = 10, bs = "ts") + te(pdsi,
#                                                                                                dnbr, k = 10, bs = "ts") + te(elev, dnbr, k = 10, bs = "ts")
# 
# Estimated degrees of freedom:
#   0.0003  0.0000  0.0033  0.0067  0.0017  0.0000  0.0000
# 0.0002 72.5638 57.5710 55.0282 51.4174 63.8191 67.6253
# 61.1901 73.5196 73.5509 73.7824 98.8932 86.9695 67.7157
# 33.1339 37.2972 63.8667 73.3431 54.8266 55.1768 58.0128
# 56.4748 58.8221 68.7843 71.1054 57.1525 75.9531 62.7232
# 69.3906  total = 1800.72
# 
# fREML score: 5481502
# 
# Family: gaussian
# Link function: identity
# 
# Formula:
#   aet ~ s(prefire_nbr, k = 10, bs = "ts") + s(precip, k = 10, bs = "ts") +
#   s(northness, k = 10, bs = "ts") + s(slope, k = 10, bs = "ts") +
#   s(aspect, k = 10, bs = "ts") + s(pdsi, k = 10, bs = "ts") +
#   s(elev, k = 10, bs = "ts") + s(dnbr, k = 10, bs = "ts") +
#   te(prefire_nbr, precip, k = 10, bs = "ts") + te(prefire_nbr,
#                                                   northness, k = 10, bs = "ts") + te(prefire_nbr, slope, k = 10,
#                                                                                      bs = "ts") + te(prefire_nbr, aspect, k = 10, bs = "ts") +
#   te(prefire_nbr, pdsi, k = 10, bs = "ts") + te(prefire_nbr,
#                                                 elev, k = 10, bs = "ts") + te(prefire_nbr, dnbr, k = 10,
#                                                                               bs = "ts") + te(precip, northness, k = 10, bs = "ts") + te(precip,
#                                                                                                                                          slope, k = 10, bs = "ts") + te(precip, aspect, k = 10, bs = "ts") +
#   te(precip, pdsi, k = 10, bs = "ts") + te(precip, elev, k = 10,
#                                            bs = "ts") + te(precip, dnbr, k = 10, bs = "ts") + te(northness,
#                                                                                                  slope, k = 10, bs = "ts") + te(northness, aspect, k = 10,
#                                                                                                                                 bs = "ts") + te(northness, pdsi, k = 10, bs = "ts") + te(northness,
#                                                                                                                                                                                          elev, k = 10, bs = "ts") + te(northness, dnbr, k = 10, bs = "ts") +
#   te(slope, aspect, k = 10, bs = "ts") + te(slope, pdsi, k = 10,
#                                             bs = "ts") + te(slope, elev, k = 10, bs = "ts") + te(slope,
#                                                                                                  dnbr, k = 10, bs = "ts") + te(aspect, pdsi, k = 10, bs = "ts") +
#   te(aspect, elev, k = 10, bs = "ts") + te(aspect, dnbr, k = 10,
#                                            bs = "ts") + te(pdsi, elev, k = 10, bs = "ts") + te(pdsi,
#                                                                                                dnbr, k = 10, bs = "ts") + te(elev, dnbr, k = 10, bs = "ts")
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  10078.6      284.5   35.42   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F  p-value
# s(prefire_nbr)            3.112e-04      9    0.000  0.00123 **
#   s(precip)                 1.643e-11      2    0.000  0.00891 **
#   s(northness)              3.259e-03      9    0.000  < 2e-16 ***
#   s(slope)                  6.663e-03      9    0.000  < 2e-16 ***
#   s(aspect)                 1.717e-03      9    0.000 7.97e-05 ***
#   s(pdsi)                   9.313e-11      3    0.000  < 2e-16 ***
#   s(elev)                   3.518e-08      8    0.000  < 2e-16 ***
#   s(dnbr)                   1.779e-04      9    0.000  < 2e-16 ***
#   te(prefire_nbr,precip)    7.256e+01     92   69.598  < 2e-16 ***
#   te(prefire_nbr,northness) 5.757e+01     99    9.079  < 2e-16 ***
#   te(prefire_nbr,slope)     5.503e+01     99   18.915  < 2e-16 ***
#   te(prefire_nbr,aspect)    5.142e+01     99  190.054  < 2e-16 ***
#   te(prefire_nbr,pdsi)      6.382e+01     94   55.282  < 2e-16 ***
#   te(prefire_nbr,elev)      6.763e+01     98   85.874  < 2e-16 ***
#   te(prefire_nbr,dnbr)      6.119e+01     99  155.991  < 2e-16 ***
#   te(precip,northness)      7.352e+01     93   51.286  < 2e-16 ***
#   te(precip,slope)          7.355e+01     92   35.571  < 2e-16 ***
#   te(precip,aspect)         7.378e+01     91   43.850  < 2e-16 ***
#   te(precip,pdsi)           9.889e+01     25 2397.277  0.92977
# te(precip,elev)           8.697e+01     12 6718.583  0.88237
# te(precip,dnbr)           6.772e+01     92  211.361  < 2e-16 ***
#   te(northness,slope)       3.313e+01     99    2.059  < 2e-16 ***
#   te(northness,aspect)      3.730e+01     99    4.822  < 2e-16 ***
#   te(northness,pdsi)        6.387e+01     92   18.877  < 2e-16 ***
#   te(northness,elev)        7.334e+01     99   46.511  < 2e-16 ***
#   te(northness,dnbr)        5.483e+01     99   20.414  < 2e-16 ***
#   te(slope,aspect)          5.518e+01     99    7.295  < 2e-16 ***
#   te(slope,pdsi)            5.801e+01     93   22.291  < 2e-16 ***
#   te(slope,elev)            5.647e+01     98   19.087  < 2e-16 ***
#   te(slope,dnbr)            5.882e+01     99   20.282  < 2e-16 ***
#   te(aspect,pdsi)           6.878e+01     91   21.469  < 2e-16 ***
#   te(aspect,elev)           7.111e+01     99  162.321  < 2e-16 ***
#   te(aspect,dnbr)           5.715e+01     99   69.450  < 2e-16 ***
#   te(pdsi,elev)             7.595e+01     36  780.536  0.00326 **
#   te(pdsi,dnbr)             6.272e+01     93   70.651  < 2e-16 ***
#   te(elev,dnbr)             6.939e+01     99   70.360  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.869   Deviance explained = 86.9%
# fREML = 5.4815e+06  Scale est. = 146.14    n = 1400119


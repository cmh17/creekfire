# Load necessary libraries
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(vroom)
library(leaps)
library(corrplot)
library(caret)
library(igraph)

# Load the dataset
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
tamarack_df <- vroom(here("data", "tamarack_fire", "tamarack_fire_model_df_outliers_removed.csv"))
knp_complex_df <- vroom(here("data", "knp_complex_fire", "knp_complex_fire_model_df_outliers_removed.csv"))

# Define predictor variables
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
corrplot(train_cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Identify highly correlated predictors (correlation > 0.7)
high_corr <- findCorrelation(train_cor_matrix, cutoff = 0.7)
corr_removed_vars <- predictor_vars[high_corr]
print(paste("Variables to consider removing due to high correlation:", paste(corr_removed_vars, collapse = ", ")))
corrplot(abs(train_cor_matrix) > 0.7)
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

  # Create a formula for the current model
  formula_str <- paste("aet ~", paste(current_set, collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Fit the linear regression model on the training set
  model <- lm(model_formula, data = train_df_scaled)

  # Predict on the validation set
  valid_predictions <- predict(model, newdata = valid_df_scaled)
  tamarack_predictions <- predict(model, newdata = tamarack_df_scaled)
  knp_complex_predictions <- predict(model, newdata = knp_complex_df_scaled)

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
    knp_complex_r2 = knp_complex_r2,
    knp_complex_rmse = knp_complex_rmse,
    tamarack_r2 = tamarack_r2,
    tamarack_rmse = tamarack_rmse
  )
}

# Convert the list to a data frame
performance_df <- bind_rows(model_performance)

# Save the performance_df 
# write_csv(performance_df, here("results", "creek_fire", "creek_fire_aet_lm_model_performance.csv"))

# Find the best model based on validation R2
best_valid_r2 <- performance_df %>% arrange(desc(valid_r2)) %>% slice(1)
best_model_predictors <- model_performance[[as.numeric(best_valid_r2$model_id)]]$predictors

# Recreate and fit the best base model
best_model_formula <- paste("aet ~", paste(best_model_predictors, collapse = " + "))
best_model <- lm(as.formula(best_model_formula), data = train_df_scaled)

# Save the model
# save(best_model, file = here("models", "creek_fire", "creek_fire_aet_lm_base.RData"))

################################### Interaction Models #####################################

# Generate two-way interactions for the best base model
two_way_interactions <- combn(best_model_predictors, 2, function(x) paste(x, collapse = ":"))
all_terms <- c(best_model_predictors, two_way_interactions)
interaction_formula <- as.formula(paste("aet ~", paste(all_terms, collapse = " + ")))

# Fit the full interaction model
creek_fire_aet_lm_interaction <- lm(interaction_formula, data = train_df_scaled)
# Save the model
save(creek_fire_aet_lm_interaction, file = here("models", "creek_fire", "creek_fire_aet_lm_interaction.RData"))

# # Load it
# load(here("models", "creek_fire", "creek_fire_aet_lm_interaction.RData"))
# 
# Perform best subset selection on the interaction model
interaction_subsets <- regsubsets(interaction_formula,train_df_scaled,
  method = "exhaustive",
  nvmax = length(all_terms),
  force.in = seq_along(best_model_predictors)
  )
interaction_subset_summary <- summary(interaction_subsets)
creek_fire_lm_interaction_subset_size <- which.min(interaction_subset_summary$bic)
best_interaction_coefs <- coef(interaction_subsets, creek_fire_lm_interaction_subset_size - length(best_model_predictors)) # subtract number of forced in vars
best_interaction_predictors <- names(best_interaction_coefs)[-1] # Increases to 37 because it forces in all non-interaction terms and includes intercept
best_interaction_formula <- as.formula(paste("aet ~", paste(best_interaction_predictors, collapse = " + ")))
creek_fire_lm_interaction_subset <- lm(best_interaction_formula, data = train_df_scaled)
# Save the model
# save(creek_fire_lm_interaction_subset, file = here("models", "creek_fire", "creek_fire_lm_interaction_subset.RData"))

######################## Models Excluding Lat and Lon ##########################

# Section to exclude lat and lon, but I did this from the start by ommitting them as possible variables
# So don't need it right now

# # Identify models without lat and lon
# model_ids <- unique(performance_df %>% 
#                               filter(map_lgl(predictors, ~ any(.x %in% c("lat", "lon")))) %>% 
#                               pull(model_id))
# models <- performance_df %>% filter(!model_id %in% model_ids)
# 
# # Select the best model without lat and lon
# reduced_model_info <- models %>% arrange(desc(valid_r2)) %>% slice(1)
# reduced_model_predictors <- model_performance[[as.numeric(reduced_model_info$model_id)]]$predictors
# 
# # Recreate and fit the best base model without lat and lon
# reduced_model_formula <- paste("aet ~", paste(reduced_model_predictors, collapse = " + "))
# reduced_model <- lm(as.formula(reduced_model_formula), data = train_df_scaled)
# # Save the model
# save(reduced_model, file = here("models", "aet_lm_base.RData"))
# 
# # Generate two-way interactions 
# two_way_interactions <- combn(reduced_model_predictors, 2, function(x) paste(x, collapse = ":"))
# all_terms <- c(reduced_model_predictors, two_way_interactions)
# interaction_formula <- as.formula(paste("aet ~", paste(all_terms, collapse = " + ")))
# 
# # Fit the full interaction model 
# creek_fire_aet_lm_interaction <- lm(interaction_formula, data = train_df_scaled)
# # Save the model
# save(creek_fire_aet_lm_interaction, file = here("models", "aet_lm_interaction.RData"))
# 
# # Perform best subset selection on the interaction model again
# interaction_subsets <- regsubsets(interaction_formula,train_df_scaled,
#   method = "exhaustive",
#   nvmax = length(all_terms),
#   force.in = seq_along(reduced_model_predictors)
#   )
# interaction_subset_summary <- summary(interaction_subsets)
# reduced_interaction_model_size <- which.min(interaction_subset_summary$bic)
# reduced_interaction_coefs <- coef(interaction_subsets, reduced_interaction_model_size)
# reduced_interaction_predictors <- names(reduced_interaction_coefs)[-1]
# reduced_interaction_formula <- as.formula(paste("aet ~", paste(reduced_interaction_predictors, collapse = " + ")))
# reduced_interaction_model <- lm(reduced_interaction_formula, data = train_df_scaled)
# # Save the model
# save(reduced_interaction_model, file = here("models", "aet_lm_interaction_reduced.RData"))

######################### Evaluate All Models on Validation and valid Data #########################

# List of models to evaluate
models_list <- list(
  "aet_lm_base" = best_model,
  "aet_lm_interaction" = creek_fire_aet_lm_interaction,
  "aet_lm_interaction_reduced" = creek_fire_lm_interaction_subset
)

# Initialize a list to store model information
model_info_list <- list()

# Loop over the models
for (model_name in names(models_list)) {
  rm(model)
  model <- models_list[[model_name]]
  
  # Get adjusted R2, AIC, BIC
  adj_r2 <- summary(model)$adj.r.squared
  model_aic <- AIC(model)
  model_bic <- BIC(model)
  
  # Get predictors and their p-values
  predictors <- names(coef(model))[-1]  # Exclude intercept
  p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]  # Exclude intercept
  
  # Get significance levels
  significance_levels <- symnum(p_values, corr = FALSE, na = FALSE, 
                                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                symbols = c("***", "**", "*", ".", " "))
  
  # Create a data frame of predictors and their significance levels
  predictors_info <- data.frame(
    predictors = predictors,
    p_value = p_values,
    significance_levels = significance_levels,
    stringsAsFactors = FALSE
  )
  
  # Predict on the valid data
  valid_predictions <- predict(model, newdata = valid_df_scaled)
  
  # Calculate RMSE and R2 on the valid data
  valid_rmse <- sqrt(mean((valid_df_scaled$aet - valid_predictions)^2))
  valid_r2 <- cor(valid_df_scaled$aet, valid_predictions)^2
  
  # Predict on test data
  tamarack_predictions <- predict(model, newdata = tamarack_df_scaled)
  knp_complex_predictions <- predict(model, newdata = knp_complex_df_scaled)
  
  tamarack_rmse <- sqrt(mean((tamarack_df_scaled$aet - tamarack_predictions)^2))
  tamarack_r2 <- cor(tamarack_df_scaled$aet, tamarack_predictions)^2
  
  knp_complex_rmse <- sqrt(mean((knp_complex_df_scaled$aet - knp_complex_predictions)^2))
  knp_complex_r2 <- cor(knp_complex_df_scaled$aet, knp_complex_predictions)^2

  # Store model information
  model_info_list[[model_name]] <- list(
    model_name = model_name,
    adj_r2 = adj_r2,
    aic = model_aic,
    bic = model_bic,
    valid_rmse = valid_rmse,
    valid_r2 = valid_r2,
    test_rmse_tamarack = tamarack_rmse,
    test_r2_tamarack = tamarack_r2,
    test_rmse_knp_complex = knp_complex_rmse,
    test_r2_knp_complex = knp_complex_r2,
    predictors_info = predictors_info
  )
}

# Create a dataframe summarizing the models
model_summary_df <- data.frame(
  model_name = character(),
  adj_r2 = numeric(),
  aic = numeric(),
  bic = numeric(),
  valid_rmse = numeric(),
  valid_r2 = numeric(),
  test_rmse_tamarack = numeric(),
  test_r2_tamarack = numeric(),
  test_rmse_knp_complex = numeric(),
  test_r2_knp_complex = numeric(),
  stringsAsFactors = FALSE
)

# Data frame with predictors and significance levels for each model
predictors_df <- data.frame(
  model_name = character(),
  predictor = character(),
  p_value = numeric(),
  significance = character(),
  stringsAsFactors = FALSE
)

for (model_name in names(model_info_list)) {
  info <- model_info_list[[model_name]]
  
  # Append to model_summary_df
  model_summary_df <- rbind(model_summary_df, data.frame(
    model_name = model_name,
    adj_r2 = info$adj_r2,
    aic = info$aic,
    bic = info$bic,
    valid_rmse = info$valid_rmse,
    valid_r2 = info$valid_r2,
    test_rmse_tamarack = info$test_rmse_tamarack,
    test_r2_tamarack = info$test_r2_tamarack,
    test_rmse_knp_complex = info$test_rmse_knp_complex,
    test_r2_knp_complex = info$test_r2_knp_complex,
    stringsAsFactors = FALSE
  ))
  
  # Append to predictors_df
  model_predictors_info <- info$predictors
  model_predictors_info$Model_Name <- model_name
  predictors_df <- rbind(predictors_df,model_predictors_info)
}

# Save the model summary data frame
# write_csv(model_summary_df, here("results", "creek_fire", "creek_fire_aet_lm_model_summary.csv"))

# Save the predictors data frame
# write_csv(predictors_df, here("results", "creek_fire", "creek_fire_aet_lm_model_predictors_significance.csv"))

############################ Model Diagonostics ################################
summary_interaction <- summary(creek_fire_aet_lm_interaction)
summary_interaction_reduced <- summary(creek_fire_lm_interaction_subset)

# Find which variables were removed from the reduced  model
removed_vars <- setdiff(names(coef(creek_fire_aet_lm_interaction)), names(coef(creek_fire_lm_interaction_subset)))

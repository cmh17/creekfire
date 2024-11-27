# Load necessary libraries
library(here)
library(tidyverse)
library(vroom)
library(leaps)
library(corrplot)
library(car)
library(caret)
library(glmnet)
library(igraph)

# Load the dataset
model_df <- vroom(here("data", "model_df_outliers_removed.csv"))

# Define predictor variables
predictor_vars <- c("lat", "lon", "prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
                    "temp", "precip", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and test sets
set.seed(2024)
train_index <- createDataPartition(model_df$ret, p = 0.6, list = FALSE)
train_df <- model_df[train_index, ]
temp_df <- model_df[-train_index, ]

valid_index <- createDataPartition(temp_df$ret, p = 0.5, list = FALSE)
valid_df <- temp_df[valid_index, ]
test_df <- temp_df[-valid_index, ]

# Preprocess the data
preproc_values <- preProcess(train_df[, predictor_vars], method = c("center", "scale"))
train_df_scaled <- train_df
train_df_scaled[, predictor_vars] <- predict(preproc_values, train_df[, predictor_vars])
valid_df_scaled <- valid_df
valid_df_scaled[, predictor_vars] <- predict(preproc_values, valid_df[, predictor_vars])
test_df_scaled <- test_df
test_df_scaled[, predictor_vars] <- predict(preproc_values, test_df[, predictor_vars])

############################# Variable Selection ###############################

# Define conflict pairs based on high correlations
conflict_pairs <- list(
  c("lat", "temp"),
  c("lat", "precip"),
  c("lon", "temp"),
  c("lon", "precip"),
  c("lon", "soil_moisture"),
  c("prefire_ndvi", "prefire_nbr"),
  c("dndvi", "dnbr"),
  c("dndvi", "rdnbr"),
  c("dnbr", "rdnbr"),
  c("temp", "precip"),
  c("temp", "elev"),
  c("precip", "soil_moisture")
)

# Create a conflict graph
g <- make_empty_graph(n = 0, directed = FALSE)
g <- add_vertices(g, length(predictor_vars), name = predictor_vars)
for (pair in conflict_pairs) {
  g <- add_edges(g, pair)
}
# Visualize the conflict graph
# plot(g, vertex.label.cex = 0.8, main = "Conflict Graph for Predictors")

# Find maximal independent sets
g_complement <- complementer(g)
max_indep_sets <- max_cliques(g_complement)
indep_sets <- lapply(max_indep_sets, function(x) V(g_complement)[x]$name)

# Fit models for each independent set
model_performance <- list()
for (i in seq_along(indep_sets)) {
  current_set <- indep_sets[[i]]
  
  # Create a formula for the current model
  formula_str <- paste("ret ~", paste(current_set, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Fit the linear regression model on the training set
  model <- lm(model_formula, data = train_df_scaled)
  
  # Predict on the validation set
  valid_predictions <- predict(model, newdata = valid_df_scaled)
  
  # Calculate performance metrics for validation set
  valid_rmse <- sqrt(mean((valid_df_scaled$ret - valid_predictions)^2))
  valid_r2 <- cor(valid_df_scaled$ret, valid_predictions)^2
  
  # Store the performance metrics along with the predictors
  model_performance[[i]] <- list(
    Model_ID = i,
    predictors = current_set,
    valid_rmse = valid_rmse,
    valid_r2 = valid_r2
  )
}

# Convert the list to a data frame
performance_df <- bind_rows(model_performance)

# Find the best model based on validation R2
best_valid_r2 <- performance_df %>% arrange(desc(valid_r2)) %>% slice(1)
best_model_predictors <- model_performance[[as.numeric(best_valid_r2$Model_ID)]]$predictors

# Recreate and fit the best base model
best_model_formula <- paste("ret ~", paste(best_model_predictors, collapse = " + "))
best_model <- lm(as.formula(best_model_formula), data = train_df_scaled)
# Save the model
save(best_model, file = here("models", "lm_base.RData"))

################################### Interaction Models #####################################

# Generate two-way interactions for the best base model
two_way_interactions <- combn(best_model_predictors, 2, function(x) paste(x, collapse = ":"))
all_terms <- c(best_model_predictors, two_way_interactions)
interaction_formula <- as.formula(paste("ret ~", paste(all_terms, collapse = " + ")))

# Fit the full interaction model
full_interaction_model <- lm(interaction_formula, data = train_df_scaled)
# Save the model
save(full_interaction_model, file = here("models", "lm_interaction.RData"))

# Perform best subset selection on the interaction model
interaction_subsets <- regsubsets(interaction_formula,train_df_scaled,
  method = "exhaustive",
  nvmax = length(all_terms),
  force.in = seq_along(best_model_predictors)
  )
interaction_subset_summary <- summary(interaction_subsets)
best_interaction_model_size <- which.min(interaction_subset_summary$bic)
best_interaction_coefs <- coef(interaction_subsets, best_interaction_model_size)
best_interaction_predictors <- names(best_interaction_coefs)[-1]
best_interaction_formula <- as.formula(paste("ret ~", paste(best_interaction_predictors, collapse = " + ")))
best_interaction_model <- lm(best_interaction_formula, data = train_df_scaled)
# Save the model
save(best_interaction_model, file = here("models", "lm_interaction_subset.RData"))

######################## Models Excluding Lat and Lon ##########################

# The best-performing model included lat and lon, but I don't want to do that
# because they're so highly correlated with climate indicators
# and these relationships are not generalizable to other regions,
# whereas using the climate indicators might be

# Identify models without lat and lon
lat_lon_model_ids <- unique(performance_df %>% 
                              filter(map_lgl(predictors, ~ any(.x %in% c("lat", "lon")))) %>% 
                              pull(Model_ID))
models_without_lat_lon <- performance_df %>% filter(!Model_ID %in% lat_lon_model_ids)

# Select the best model without lat and lon
best_no_lat_lon_model_info <- models_without_lat_lon %>% arrange(desc(valid_r2)) %>% slice(1)
best_no_lat_lon_model_predictors <- model_performance[[as.numeric(best_no_lat_lon_model_info$Model_ID)]]$predictors

# Recreate and fit the best base model without lat and lon
best_no_lat_lon_model_formula <- paste("ret ~", paste(best_no_lat_lon_model_predictors, collapse = " + "))
best_no_lat_lon_model <- lm(as.formula(best_no_lat_lon_model_formula), data = train_df_scaled)
# Save the model
save(best_no_lat_lon_model, file = here("models", "lm_no_lat_lon_base.RData"))

# Generate two-way interactions 
no_lat_lon_two_way_interactions <- combn(best_no_lat_lon_model_predictors, 2, function(x) paste(x, collapse = ":"))
no_lat_lon_all_terms <- c(best_no_lat_lon_model_predictors, no_lat_lon_two_way_interactions)
no_lat_lon_interaction_formula <- as.formula(paste("ret ~", paste(no_lat_lon_all_terms, collapse = " + ")))

# Fit the full interaction model 
full_no_lat_lon_interaction_model <- lm(no_lat_lon_interaction_formula, data = train_df_scaled)
# Save the model
save(full_no_lat_lon_interaction_model, file = here("models", "lm_no_lat_lon_interaction.RData"))

# Perform best subset selection on the interaction model again
no_lat_lon_interaction_subsets <- regsubsets(no_lat_lon_interaction_formula,train_df_scaled,
  method = "exhaustive",
  nvmax = length(no_lat_lon_all_terms),
  force.in = seq_along(best_no_lat_lon_model_predictors)
  )
no_lat_lon_interaction_subset_summary <- summary(no_lat_lon_interaction_subsets)
best_no_lat_lon_interaction_model_size <- which.min(no_lat_lon_interaction_subset_summary$bic)
best_no_lat_lon_interaction_coefs <- coef(no_lat_lon_interaction_subsets, best_no_lat_lon_interaction_model_size)
best_no_lat_lon_interaction_predictors <- names(best_no_lat_lon_interaction_coefs)[-1]
best_no_lat_lon_interaction_formula <- as.formula(paste("ret ~", paste(best_no_lat_lon_interaction_predictors, collapse = " + ")))
best_no_lat_lon_interaction_model <- lm(best_no_lat_lon_interaction_formula, data = train_df_scaled)
# Save the model
save(best_no_lat_lon_interaction_model, file = here("models", "lm_no_lat_lon_interaction_subset.RData"))

######################### Evaluate All Models on Test Data #########################

# List of models to evaluate
models_list <- list(
  "base_model" = best_model,
  "interaction_model" = full_interaction_model,
  "reduced_interaction_model" = best_interaction_model,
  "base_no_lat_lon_model" = best_no_lat_lon_model,
  "interaction_no_lat_lon_model" = full_no_lat_lon_interaction_model,
  "reduced_interaction_no_lat_lon_model" = best_no_lat_lon_interaction_model
)

# Initialize a list to store model information
model_info_list <- list()

# Loop over the models
for (model_name in names(models_list)) {
  model <- models_list[[model_name]]
  
  # Get adjusted R², AIC, BIC
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
    Predictor = predictors,
    p_value = p_values,
    Significance = significance_levels,
    stringsAsFactors = FALSE
  )
  
  # Predict on the test data
  test_predictions <- predict(model, newdata = test_df_scaled)
  
  # Calculate RMSE and R² on the test data
  test_rmse <- sqrt(mean((test_df_scaled$ret - test_predictions)^2))
  test_r2 <- cor(test_df_scaled$ret, test_predictions)^2
  
  # Store model information
  model_info_list[[model_name]] <- list(
    Model_Name = model_name,
    Adjusted_R2 = adj_r2,
    AIC = model_aic,
    BIC = model_bic,
    Test_RMSE = test_rmse,
    Test_R2 = test_r2,
    Predictors = predictors_info
  )
}

# Create a data frame summarizing the models
model_summary_df <- data.frame(
  Model_Name = character(),
  Adjusted_R2 = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Test_RMSE = numeric(),
  Test_R2 = numeric(),
  stringsAsFactors = FALSE
)

# Data frame with predictors and significance levels for each model
predictors_df <- data.frame(
  Model_Name = character(),
  Predictor = character(),
  p_value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

for (model_name in names(model_info_list)) {
  info <- model_info_list[[model_name]]
  
  # Append to model_summary_df
  model_summary_df <- rbind(model_summary_df, data.frame(
    Model_Name = model_name,
    Adjusted_R2 = info$Adjusted_R2,
    AIC = info$AIC,
    BIC = info$BIC,
    Test_RMSE = info$Test_RMSE,
    Test_R2 = info$Test_R2,
    stringsAsFactors = FALSE
  ))
  
  # Append to predictors_df
  model_predictors_info <- info$Predictors
  model_predictors_info$Model_Name <- model_name
  predictors_df <- rbind(predictors_df,model_predictors_info)
}

# Save the model summary data frame
write_csv(model_summary_df, here("results", "model_summary.csv"))

# Save the predictors data frame
write_csv(predictors_df, here("results", "model_predictors_significance.csv"))


# Load necessary libraries
library(here)
library(dplyr)
library(readr)
library(mgcv)
library(Metrics)  
library(vroom)
library(caret)
# library(terra)
# library(tidyterra)

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

################################### Models #####################################

# Load models
# GAM models
load(here("models", "creek_fire", "creek_fire_aet_gam_base_k10.RData"))
creek_fire_gam_base_k10 <- creek_fire_aet_gam_base_k10
load(here("models", "creek_fire", "creek_fire_aet_gam_interaction_k10.RData"))  
creek_fire_gam_interaction_k10 <- creek_fire_aet_gam_interaction_k10
load(here("models", "creek_fire", "creek_fire_aet_gam_interaction_reduced_k10.RData"))  # Reduced interaction model -- reduce based on which variables got shrunk
creek_fire_gam_interaction_reduced_k10 <- reduced_interaction_model

# Linear models
load(here("models", "creek_fire", "creek_fire_aet_lm_base.RData"))    
creek_fire_lm_base <- best_model
load(here("models", "creek_fire", "creek_fire_aet_lm_interaction.RData"))  
creek_fire_lm_interaction <- creek_fire_aet_lm_interaction
load(here("models", "creek_fire", "creek_fire_aet_lm_interaction_subset.RData"))   
creek_fire_lm_interaction_reduced <- creek_fire_lm_interaction_subset

# List of models and their names
model_list <- list(
  gam_base = creek_fire_gam_base_k10,
  gam_interaction = creek_fire_gam_interaction_k10,
  gam_interaction_reduced = creek_fire_gam_interaction_reduced_k10,
  lm_base = creek_fire_lm_base,
  lm_interaction = creek_fire_lm_interaction,
  lm_interaction_reduced = creek_fire_lm_interaction_reduced
)

# Initialize an empty list to store model metrics
model_metrics <- list()

# Loop over each model to extract metrics
for (model_name in names(model_list)) {
  print(paste("Evaluating model:", model_name))
  model <- model_list[[model_name]]
  # Determine model type
  if (inherits(model, "gam") | inherits(model, "bam")) {
    model_type <- "gam"
  } else if (inherits(model, "lm")) {
    model_type <- "lm"
  } else {
    model_type <- "other"
  }
  
  # Extract model formula as a string
  model_formula_str <- deparse(formula(model))
  model_formula_str <- paste(model_formula_str, collapse = "")
  
  # Number of predictors
  if (model_type == "gam") {
    num_predictors <- length(model$smooth)
  } else if (model_type == "lm") {
    num_predictors <- length(coef(model)) - 1  # Exclude intercept
  } else {
    num_predictors <- NA
  }
  
  # Extract Adjusted R-squared
  if (model_type == "gam") {
    model_summary <- summary(model)
    adj_r2 <- model_summary$r.sq
    deviance_explained <- model_summary$dev.expl * 100  # Convert to percentage
    model_aic <- AIC(model)
    model_bic <- BIC(model)
  } else if (model_type == "lm") {
    model_summary <- summary(model)
    adj_r2 <- model_summary$adj.r.squared
    deviance_explained <- NA
    model_aic <- AIC(model)
    model_bic <- BIC(model)
  } else {
    adj_r2 <- NA
    deviance_explained <- NA
    model_aic <- NA
    model_bic <- NA
  }
  
  # Predict on test set
  valid_predictions <- predict(model, newdata = valid_df_scaled)
  test_predictions_tamarack <- predict(model, newdata = tamarack_df_scaled)
  test_predictions_knp_complex <- predict(model, newdata = knp_complex_df_scaled)
  
  # Calculate performance metrics on test set
  valid_rmse <- sqrt(mean((valid_df_scaled$aet - valid_predictions)^2))
  valid_r2 <- cor(valid_df_scaled$aet, valid_predictions)^2
  
  test_rmse_tamarack <- sqrt(mean((tamarack_df_scaled$aet - test_predictions_tamarack)^2))
  test_r2_tamarack <- cor(tamarack_df_scaled$aet, test_predictions_tamarack)^2
  
  test_rmse_knp_complex <- sqrt(mean((knp_complex_df_scaled$aet - test_predictions_knp_complex)^2))
  test_r2_knp_complex <- cor(knp_complex_df_scaled$aet, test_predictions_knp_complex)^2
  
  # For linear models, extract predictor significance
  if (model_type == "linear") {
    predictors <- names(coef(model))[-1]  # Exclude intercept
    p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]  # Exclude intercept
    # Get significance levels
    significance_levels <- symnum(p_values, corr = FALSE, na = FALSE, 
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                  symbols = c("***", "**", "*", ".", " "))
    # Create a data frame of predictors and their significance levels
    predictors_info <- data.frame(
      model_name = model_name,
      predictors = predictors,
      p_values = p_values,
      significance_levels = significance_levels,
      stringsAsFactors = FALSE
    )
  } else {
    predictors_info <- NULL
  }
  
  # Store the metrics in the list
  model_metrics[[model_name]] <- list(
    model_name = model_name,
    model_type = model_type,
    model_description = model_formula_str,
    num_predictors = num_predictors,
    adj_r2 = adj_r2,
    deviance_explained = deviance_explained,
    aic = model_aic,
    bic = model_bic,
    valid_rmse = valid_rmse,
    valid_r2 = valid_r2,
    test_rmse_tamarack = test_rmse_tamarack,
    test_r2_tamarack = test_r2_tamarack,
    test_rmse_knp_complex = test_rmse_knp_complex,
    test_r2_knp_complex = test_r2_knp_complex,
    Predictors_Info = predictors_info
  )
}

print("Model evaluation complete.")
# Combine all metrics into a single data frame
comparison_table <- do.call(rbind, lapply(model_metrics, function(x) {
  data.frame(
    model_name = x$model_name,
    model_type = x$model_type,
    num_predictors = x$num_predictors,
    adj_r2 = x$adj_r2,
    deviance_explained = x$deviance_explained,
    aic = x$aic,
    bic = x$bic,
    valid_rmse = x$valid_rmse,
    valid_r2 = x$valid_r2,
    test_rmse_tamarack = x$test_rmse_tamarack,
    test_r2_tamarack = x$test_r2_tamarack,
    test_rmse_knp_complex = x$test_rmse_knp_complex,
    test_r2_knp_complex = x$test_r2_knp_complex,
    stringsAsFactors = FALSE
  )
}))

# Round numeric values for better readability
comparison_table <- comparison_table %>%
  mutate_if(is.numeric, round, digits = 4)

# Arrange the table by Test_R_Squared
comparison_table <- comparison_table %>% arrange(desc(valid_r2))

# Display the comparison table
print(comparison_table)

# Combine predictor significance information
predictors_df <- do.call(rbind, lapply(model_metrics, function(x) x$num_predictors))

print("Predictor significance information combined.")
# Save the comparison table
write_csv(comparison_table, here("results", "creek_fire", "creek_fire_model_comparison_table.csv"))

# Save the predictor table
write_csv(predictors_df, here("results", "creek_fire", "creek_fire_predictor_significance_table.csv"))
print("Tables saved successfully.")

# Load the table to check
comparison_table <- read_csv(here("results", "creek_fire", "creek_fire_model_comparison_table.csv"))

########################## Check Basis Functions ###############################

# Find vars that were removed from the interaction lm
removed_vars <- setdiff(names(creek_fire_lm_interaction$coefficients), names(creek_fire_lm_interaction_reduced$coefficients))
print(removed_vars)

# # Look at some of the gams with higher basis dimensions
# 
# load(here("models", "gam_base_k100.RData"))
# best_model
# summary(best_model)
# 
# load(here("models", "gam_interaction_final.RData"))
# summary(final_model)

# Load necessary libraries
library(mgcv)   
library(caret)   
library(ggplot2)
library(dplyr)  
library(here)
library(vroom)
library(car)
library(parallel)
library(e1071)

# Load the dataset
model_df <- vroom(here("data", "model_df_outliers_removed.csv"))

# Define predictor variables
predictor_vars <- c("lat","lon","prefire_ndvi","prefire_nbr","dndvi","dnbr","rdnbr",
                    "temp","precip","elev","aspect","slope","northness","soil_moisture")

# Split data into train and test, 80/20
set.seed(2024)
train_index <- createDataPartition(model_df$ret, p = 0.8, list = FALSE)
train_df <- model_df[train_index, ]
test_df <- model_df[-train_index, ]

# Define a pre-processing object using the training data
preproc_values <- preProcess(train_df[, predictor_vars], method = c("center", "scale"))

# Apply the transformations to the training and test sets
train_df_scaled <- train_df
train_df_scaled[, predictor_vars] <- predict(preproc_values, train_df[, predictor_vars])

test_df_scaled <- test_df
test_df_scaled[, predictor_vars] <- predict(preproc_values, test_df[, predictor_vars])

######################## Explore Data Characteristics ##########################

shapiro.test(sample(train_df_scaled$ret,1000))
# rET fails the Shapiro Wilk test, suggesting it is not normally distributed

summary(train_df_scaled$ret)
ggplot(model_df, aes(x = ret)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") + 
  theme_minimal() +
  ggtitle("Distribution of rET")

# Can't use gamma link because there are negative values, but the data are
# skewed

skewness(train_df_scaled$ret)
# some negative skew

#################### Full Model with Cubic Regression Splines ##################



############################### Reduced Model ##################################
# 
# # Look at reduced model eliminating highly correlated predictors
# # Check for multicollinearity in the training set
# train_predictors <- train_df_scaled[, predictor_vars]
# train_cor_matrix <- cor(train_predictors, use = "complete.obs")
# 
# # Identify highly correlated predictors (correlation > 0.7)
# high_corr <- findCorrelation(train_cor_matrix, cutoff = 0.7)
# corr_removed_vars <- predictor_vars[high_corr]
# 
# # Remove highly correlated variables
# reduced_vars <- predictor_vars[-high_corr]
# train_df_reduced <- train_df_scaled[, c("ret", reduced_vars)]
# 
# # Fit a full MLR model with the reduced predictors and calculate VIF
# train_full_model <- lm(ret ~ ., data = train_df_reduced)
# vif_values <- vif(train_full_model)
# high_vif_vars <- names(vif_values[vif_values > 5])
# final_vars <- setdiff(reduced_vars, high_vif_vars)
# train_df_final <- train_df_scaled[, c("ret", final_vars)]
# 
# # Fit a reduced GAM using ts smooths
# reduced_model_ts_formula <- as.formula(paste("ret ~ ", paste(paste0("s(",final_vars), ", k = 20, bs='ts')", collapse = " + ")))
# 
# reduced_model_ts <- bam(reduced_model_ts_formula, data = train_df_final, method = "fREML")
# 
# # Check
# gam.check(reduced_model_ts)
# 
# # Summary
# summary(reduced_model_ts)

# save(reduced_model_ts, file = here("models/reduced_gam_ts.RData"))

################## Try Adding Interactions to Reduced Model ####################

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
library(doParallel)
library(foreach)
library(mgcv)

# Load the dataset
model_df <- vroom(here("data", "model_df_outliers_removed.csv"))

# Define response and predictor variables
response_var <- "ret"
predictor_vars <- c("lat","lon","prefire_ndvi","prefire_nbr","dndvi","dnbr","rdnbr",
                    "temp","precip","elev","aspect","slope","northness","soil_moisture")

# Split data into training (80%) and test (20%) sets
set.seed(2024)
train_index <- createDataPartition(model_df[[response_var]], p = 0.8, list = FALSE)
train_df <- model_df[train_index, ]
test_df <- model_df[-train_index, ]

# Define a pre-processing object using the training data
preproc_values <- preProcess(train_df[, predictor_vars], method = c("center", "scale"))

# Apply the transformations to the training and test sets
train_df_scaled <- train_df
train_df_scaled[, predictor_vars] <- predict(preproc_values, train_df[, predictor_vars])

test_df_scaled <- test_df
test_df_scaled[, predictor_vars] <- predict(preproc_values, test_df[, predictor_vars])

############################# Variable Selection ###############################

# Check for multicollinearity in the training set
train_predictors <- train_df_scaled[, predictor_vars]
train_cor_matrix <- cor(train_predictors, use = "complete.obs")
corrplot(train_cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Identify highly correlated predictors (correlation > 0.7)
high_corr <- findCorrelation(train_cor_matrix, cutoff = 0.7)
corr_removed_vars <- predictor_vars[high_corr]
print(paste("Variables to consider removing due to high correlation:", paste(corr_removed_vars, collapse = ", ")))

# Judgement:
# lat and lon are highly correlated with precip and temp
# since we're more interested in ecological setting than location, remove them
# since nbr and ndvi are somewhat redundant, remove nbr since ndvi is more informative about vegetative health
# soil moisture and precipitation have a strong correlation; could remove either
# temp and elev have a strong negative correlation, but one doesn't seem massively more useful than the other
# so try models with both; start with elev

# Define conflict pairs based on high correlations
conflict_pairs <- list(
  c("lat", "temp"),
  c("lat", "precip"),
  c("lon", "temp"),
  c("lon", "precip"),
  c("lon", "soil_moisture"),
  c("prefire_ndvi", "prefire_nbr"),
  c("dndvi", "dnbr"),
  c("dndvi","rdnbr"),
  c("dnbr", "rdnbr"),
  c("temp", "precip"),
  c("temp", "elev"),
  c("precip", "soil_moisture")
)

# Create a graph where nodes are predictors and edges represent conflicts
g <- make_empty_graph(n=0, directed=FALSE)
g <- add_vertices(g, length(predictor_vars), name=predictor_vars)

# Add edges based on conflict pairs
for(pair in conflict_pairs){
  g <- add_edges(g, pair)
}

# Visualize the conflict graph
plot(g, vertex.label.cex=0.8, main="Conflict Graph for Predictors")

# Create the complement graph to find independent sets as cliques
g_complement <- complementer(g)

# Find all maximal cliques in the complement graph (maximal independent sets in the original graph)
max_indep_sets <- maximal.cliques(g_complement)

# Extract variable names from cliques
indep_sets <- lapply(max_indep_sets, function(x) V(g_complement)[x]$name)

# Optionally, limit the size of independent sets to manage computational complexity
max_size <- 7  # Adjust based on desired model complexity
indep_sets <- indep_sets[sapply(indep_sets, length) <= max_size]

# Initialize parallel backend to use multiple cores
num_cores <- detectCores() - 1  # Reserve one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

model_performance <- list()
# Iterate through each independent set using parallel processing
for (i in 1:length(indep_sets)) {
  current_set <- indep_sets[[i]]
  
  # Create a formula for the current GAM model
  # Include smooth terms for continuous variables; adjust if some are categorical
  smooth_terms <- paste0("s(", current_set, ", k=30, bs='ts')", collapse = " + ")
  model_formula <- as.formula(paste(response_var, "~", smooth_terms))
  
  # Fit the GAM model on the training set
  model <- bam(model_formula, data = train_df_scaled, 
               subset = sample(1:nrow(train_df_scaled), 1000),
               method = "fREML")
  
  # Save model summary
  model_summary <- summary(model)
  
  # Predict on the test set
  test_predictions <- predict(model, newdata = test_df_scaled)
  
  # Calculate performance metrics for test set
  test_rmse <- sqrt(mean((test_df_scaled[[response_var]] - test_predictions)^2))
  test_r2 <- cor(test_df_scaled[[response_var]], test_predictions)^2
  
  # Store the performance metrics along with the predictors
  model_performance[[i]] <- 
    list(
      predictors = current_set,
      summary = model_summary,
      test_rmse = test_rmse,
      test_r2 = test_r2
    )
}


# Stop the cluster after processing
stopCluster(cl)
registerDoSEQ()

# Convert the list of model performances to a data frame
performance_df <- bind_rows(lapply(model_performance, as_tibble), .id = "Model_ID")

# View the first few rows
head(performance_df)

# Summarize performance metrics
summary(performance_df)

# Find the model with the highest test R2
best_test_r2 <- performance_df %>% arrange(desc(test_r2)) %>% slice(1)
print("Best Model by Test R-squared:")
print(best_test_r2)

# Find the model with the lowest test RMSE
best_test_rmse <- performance_df %>% arrange(test_rmse) %>% slice(1)
print("Best Model by Test RMSE:")
print(best_test_rmse)

# Save the results for future reference
write_csv(performance_df, here("data", "gam_variable_selection_results.csv"))

# Visualize the results
ggplot(performance_df, aes(x = test_rmse, y = test_r2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = Model_ID), nudge_y = 0.01, size = 3, check_overlap = TRUE) +
  labs(title = "GAM Model Performance Comparison",
       x = "Test RMSE",
       y = "Test R-squared") +
  theme_minimal()

# Extract the best model's predictors based on test R2
best_model_predictors <- model_performance[[as.numeric(best_test_r2$Model_ID)]]$predictors

# Recreate the formula for the best GAM model
smooth_terms_best <- paste0("s(", best_model_predictors, ")", collapse = " + ")
best_model_formula <- as.formula(paste("ret ~", smooth_terms_best))

# Fit the best GAM model on the training set
best_model <- gam(best_model_formula, data = train_df_scaled, method = "REML")

# Summarize the best model
summary(best_model)

# Plot the smooth terms
plot(best_model, pages = 1, shade = TRUE)

######################### Try Adding Interaction Terms #########################



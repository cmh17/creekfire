# Analyzing results
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(vroom)
library(leaps)
library(corrplot)
library(caret)
library(igraph)
library(ggplot2)
library(gridExtra)

# Load the dataset
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
tamarack_df <- vroom(here("data", "tamarack_fire", "tamarack_fire_model_df_outliers_removed.csv"))
knp_complex_df <- vroom(here("data", "knp_complex_fire", "knp_complex_fire_model_df_outliers_removed.csv"))

# Define variable variables
vars <- c("prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
          "tmax", "precip",  "vpd", "pdsi", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and valid sets
set.seed(2024)
train_index <- createDataPartition(model_df$aet, p = 0.7, list = FALSE)
train_df <- model_df[train_index, ]
valid_df <- model_df[-train_index, ]

# Preprocess the data
preproc_values <- preProcess(train_df[, vars], method = c("center", "scale"))
train_df_scaled <- train_df
train_df_scaled[, vars] <- predict(preproc_values, train_df[, vars])
valid_df_scaled <- valid_df
valid_df_scaled[, vars] <- predict(preproc_values, valid_df[, vars])
tamarack_df_scaled <- tamarack_df
tamarack_df_scaled[, vars] <- predict(preproc_values, tamarack_df[, vars])
knp_complex_df_scaled <- knp_complex_df
knp_complex_df_scaled[, vars] <- predict(preproc_values, knp_complex_df[, vars])

# Sample the data to plot sparser scatterplots
train_df_sampled <- train_df[sample(nrow(train_df), 10000),]
tamarack_df_sampled <- tamarack_df[sample(nrow(tamarack_df), 10000),]
knp_complex_df_sampled <- knp_complex_df[sample(nrow(knp_complex_df), 10000),]

########## Look at differences in performance between linear models ############
# Load results
results_df <- read.csv(here("results", "creek_fire", "creek_fire_aet_lm_model_predictors_significance.csv"))
variables <- aggregate(
    predictors ~ model_name,
    results_df,
    list
)[2]$predictors
names(variables) <- unique(results_df$model_name)

# Create histograms comparing distributions across fires for each variable
# Function to create histogram for a single variable
create_variable_histogram <- function(variable, var_names, use_scaled = FALSE) {
    # Choose which datasets to use
    if (use_scaled) {
        train_data <- train_df_scaled
        tamarack_data <- tamarack_df_scaled
        knp_data <- knp_complex_df_scaled
        title_suffix <- " (Scaled)"
    } else {
        train_data <- train_df
        tamarack_data <- tamarack_df
        knp_data <- knp_complex_df
        title_suffix <- ""
    }
    
    # Calculate means for vertical lines
    creek_mean <- mean(train_data[[variable]], na.rm = TRUE)
    tamarack_mean <- mean(tamarack_data[[variable]], na.rm = TRUE)
    knp_mean <- mean(knp_data[[variable]], na.rm = TRUE)
    
    # Create the plot
    ggplot() +
        geom_histogram(data = train_data, aes(x = !!sym(variable), after_stat(ncount), fill = "Creek Fire (Train)"), 
                       color = "black", alpha = 0.6, bins = 30) + 
        geom_histogram(data = knp_data, aes(x = !!sym(variable), after_stat(ncount), fill = "KNP Complex"), 
                       color = "black", alpha = 0.6, bins = 30) +
        geom_histogram(data = tamarack_data, aes(x = !!sym(variable), after_stat(ncount), fill = "Tamarack"), 
                       color = "black", alpha = 0.6, bins = 30) +
        # Add vertical lines for means
        geom_vline(xintercept = creek_mean, color = "#1f77b4", linetype = "dashed", linewidth = 1, alpha = 0.8) +
        geom_vline(xintercept = knp_mean, color = "#ff7f0e", linetype = "dashed", linewidth = 1, alpha = 0.8) +
        geom_vline(xintercept = tamarack_mean, color = "#2ca02c", linetype = "dashed", linewidth = 1, alpha = 0.8) +
        scale_fill_manual(values = c("Creek Fire (Train)" = "#1f77b4", "KNP Complex" = "#ff7f0e", "Tamarack" = "#2ca02c")) +
        labs(title = paste("Distribution of", var_names[which(vars == variable)], title_suffix),
             x = variable,
             y = "Normalized Count",
             fill = "Fire") +
        theme_minimal() +
        theme(legend.position = "bottom")
}

# Create histograms for all variables (original scale)
vars[length(vars) + 1] <- "aet"
var_names <-c("Prefire NDVI", "Prefire NBR", "dNDVI", "dNBR", "rdNBR",
              "Max. Temp.", "Precip.",  "VPD", "PDSI", "Elev.", "Aspect", "Slope", "Northness", "Soil Moisture", "aET") 
hist_plots_original <- list()
for (i in seq_along(vars)) {
    hist_plots_original[[i]] <- create_variable_histogram(vars[i], var_names, use_scaled = FALSE)
}

# Create histograms for all variable variables (scaled)
hist_plots_scaled <- list()
for (i in seq_along(vars)) {
    hist_plots_scaled[[i]] <- create_variable_histogram(vars[i], var_names, use_scaled = TRUE)
}

# Display all histograms in a grid (original scale)
grid_plot_original <- do.call(grid.arrange, c(hist_plots_original, ncol = 3))

# Display all histograms in a grid (scaled)
grid_plot_scaled <- do.call(grid.arrange, c(hist_plots_scaled, ncol = 3))

# Alternative: Create individual plots for specific variables of interest
# (based on your model results)
if ("aet_lm_base" %in% names(variables)) {
    base_variables <- unlist(variables$aet_lm_base)
    
    # Create plots for base model variables
    base_plots <- list()
    for (i in seq_along(base_variables)) {
        if (base_variables[i] %in% vars) {
            base_plots[[i]] <- create_variable_histogram(base_variables[i], var_names, use_scaled = FALSE)
        }
    }
    
    # Remove NULL elements
    base_plots <- base_plots[!sapply(base_plots, is.null)]
    
    # Display base model variable histograms
    if (length(base_plots) > 0) {
        base_grid_plot <- do.call(grid.arrange, c(base_plots, ncol = 2))
    }
}

# Create summary statistics table
create_summary_stats <- function() {
    summary_stats <- data.frame()
    
    for (variable in vars) {
        # Creek Fire (training data)
        creek_stats <- train_df %>%
            summarise(
                fire = "Creek Fire",
                variable = variable,
                mean = mean(!!sym(variable), na.rm = TRUE),
                median = median(!!sym(variable), na.rm = TRUE),
                sd = sd(!!sym(variable), na.rm = TRUE),
                min = min(!!sym(variable), na.rm = TRUE),
                max = max(!!sym(variable), na.rm = TRUE)
            )
        
        # Tamarack Fire
        tamarack_stats <- tamarack_df %>%
            summarise(
                fire = "Tamarack Fire",
                variable = variable,
                mean = mean(!!sym(variable), na.rm = TRUE),
                median = median(!!sym(variable), na.rm = TRUE),
                sd = sd(!!sym(variable), na.rm = TRUE),
                min = min(!!sym(variable), na.rm = TRUE),
                max = max(!!sym(variable), na.rm = TRUE)
            )
        
        # KNP Complex Fire
        knp_stats <- knp_complex_df %>%
            summarise(
                fire = "KNP Complex Fire",
                variable = variable,
                mean = mean(!!sym(variable), na.rm = TRUE),
                median = median(!!sym(variable), na.rm = TRUE),
                sd = sd(!!sym(variable), na.rm = TRUE),
                min = min(!!sym(variable), na.rm = TRUE),
                max = max(!!sym(variable), na.rm = TRUE)
            )
        
        summary_stats <- rbind(summary_stats, creek_stats, tamarack_stats, knp_stats)
    }
    
    return(summary_stats)
}

# Generate summary statistics
summary_stats <- create_summary_stats()
print(summary_stats)

########## Create scatter plots of aET vs each predictor ############

# Function to create scatter plot of aET vs predictor for each fire
create_aet_scatter <- function(predictor, var_names, use_scaled = FALSE) {
    # Choose which datasets to use
    if (use_scaled) {
        train_data <- train_df_scaled
        tamarack_data <- tamarack_df_scaled
        knp_data <- knp_complex_df_scaled
        title_suffix <- " (Scaled)"
        aet_var <- "aet"  # aET is not scaled in the current setup
    } else {
        train_data <- train_df_sampled
        tamarack_data <- tamarack_df_sampled
        knp_data <- knp_complex_df_sampled
        title_suffix <- ""
        aet_var <- "aet"
    }
    
    # Create the plot
    ggplot() +
        geom_point(data = train_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "Creek Fire (Train)"), 
                   alpha = 0.4, size = 0.5) +
        geom_point(data = knp_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "KNP Complex"), 
                   alpha = 0.4, size = 0.5) +
        geom_point(data = tamarack_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "Tamarack"), 
                   alpha = 0.4, size = 0.5) +
        # Add trend lines
        geom_smooth(data = train_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "Creek Fire (Train)"), 
                    method = "lm", se = FALSE, linewidth = 1) +
        geom_smooth(data = knp_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "KNP Complex"), 
                    method = "lm", se = FALSE, linewidth = 1) +
        geom_smooth(data = tamarack_data, aes(x = !!sym(predictor), y = !!sym(aet_var), color = "Tamarack"), 
                    method = "lm", se = FALSE, linewidth = 1) +
        scale_color_manual(values = c("Creek Fire (Train)" = "#1f77b4", "KNP Complex" = "#ff7f0e", "Tamarack" = "#2ca02c")) +
        labs(title = paste("aET vs", var_names[which(vars[-length(vars)] == predictor)], title_suffix),
             x = predictor,
             y = "aET",
             color = "Fire") +
        theme_minimal() +
        theme(legend.position = "bottom")
}

# Create scatter plots for all predictors (original scale) - exclude aET from predictors
predictor_vars_only <- vars[-length(vars)]  # Remove aET from the list
scatter_plots_original <- list()
for (i in seq_along(predictor_vars_only)) {
    scatter_plots_original[[i]] <- create_aet_scatter(predictor_vars_only[i], var_names, use_scaled = FALSE)
}

# Create scatter plots for all predictors (scaled)
scatter_plots_scaled <- list()
for (i in seq_along(predictor_vars_only)) {
    scatter_plots_scaled[[i]] <- create_aet_scatter(predictor_vars_only[i], var_names, use_scaled = TRUE)
}

# Create scatter plot grids (without displaying - for headless server)
scatter_grid_original <- do.call(arrangeGrob, c(scatter_plots_original, ncol = 3))

# Create scatter plot grids (without displaying - for headless server)
# scatter_grid_scaled <- do.call(arrangeGrob, c(scatter_plots_scaled, ncol = 3))

# Save histogram plots
png(here("figures", "creek_fire", "variable_distributions_original.png"), 
    width = 15, height = 20, units = "in", res = 300, type = "cairo")
grid::grid.draw(grid_plot_original)
dev.off()

png(here("figures", "creek_fire", "variable_distributions_scaled.png"), 
    width = 15, height = 20, units = "in", res = 300, type = "cairo")
grid::grid.draw(grid_plot_scaled)
dev.off()

# Save scatter plots
png(here("figures", "creek_fire", "aet_scatter_plots_original.png"), 
    width = 15, height = 20, units = "in", res = 300, type = "cairo")
grid::grid.draw(scatter_grid_original)
dev.off()

png(here("figures", "creek_fire", "aet_scatter_plots_scaled.png"), 
    width = 15, height = 20, units = "in", res = 300, type = "cairo")
grid::grid.draw(scatter_grid_scaled)
dev.off()

# Save summary statistics
write.csv(summary_stats, here("results", "creek_fire", "variable_summary_statistics.csv"), row.names = FALSE)


# Load necessary libraries
library(here)
library(dplyr)
library(readr)
library(mgcv)
library(Metrics)  
library(vroom)
library(caret)
library(terra)
library(tidyterra)

# Load the data and process it like before
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
pier_df <- vroom(here("data", "pier_fire", "pier_fire_model_df_outliers_removed.csv"))


# Define predictor variables
predictor_vars <- c("lat", "lon", "prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
                    "temp", "precip", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and test sets
set.seed(2024)
train_index <- createDataPartition(model_df$aet, p = 0.6, list = FALSE)
train_df <- model_df[train_index, ]
temp_df <- model_df[-train_index, ]

valid_index <- createDataPartition(temp_df$aet, p = 0.5, list = FALSE)
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

############################# Load Models ########################################

load(here("models", "aet_gam_interaction.RData"))  # Reduced interaction model -- reduce based on which variables got shrunk
gam_interaction <- full_interaction_model

load(here("models", "aet_lm_interaction.RData"))
lm_interaction <- full_interaction_model

########################### Create Predicted Rasters ##############################

# extract fitted values
fitted_values_gam_interaction <- predict(gam_interaction, test_df_scaled, type = "response")
fitted_values_lm_interaction <- predict(lm_interaction, test_df_scaled, type = "response")

# Create a SpatRaster object from the data frame
coords <- data.frame(lon = test_df$lon, lat = test_df$lat)

# save these in a df and csv to be safe
fitted_values_df <- data.frame(response=test_df_scaled$aet,
                               lm_interaction=fitted_values_lm_interaction,
                               gam_interaction=fitted_values_gam_interaction)

write_csv(fitted_values_df,here("results","aet_fitted_values_df.csv"))

# Convert the data frame to a terra vector (SpatVector)
points_response_aet <- terra::vect(coords, crs = "EPSG:4326")
points_fitted_values_gam_interaction <- terra::vect(coords, crs = "EPSG:4326")
points_fitted_values_lm_interaction <- terra::vect(coords, crs = "EPSG:4326")

################################# Make Rasters #################################

values(points_response_aet) <- fitted_values_df$response
values(points_fitted_values_gam_interaction) <- fitted_values_df$gam_interaction
values(points_fitted_values_lm_interaction) <- fitted_values_df$lm_interaction

# Rasterize the points
raster_template <- rast(ext(points_response_aet), resolution = 0.001, crs = "EPSG:4326")
response_rast <- rasterize(points_response_aet, raster_template, field="value")
gam_interaction_rast <- rasterize(points_fitted_values_gam_interaction, raster_template, field="value")
lm_interaction_rast <- rasterize(points_fitted_values_lm_interaction, raster_template, field="value")

# Write the outputs
writeRaster(response_rast, here("results", "aet_response_rast.tif"), overwrite = TRUE)
writeRaster(gam_interaction_rast, here("results", "aet_gam_interaction_predictions.tif"), overwrite = TRUE)
writeRaster(lm_interaction_rast, here("results", "aet_lm_interaction_predictions.tif"), overwrite = TRUE)

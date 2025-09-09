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

############################# Load Models ########################################

load(here("models", "creek_fire_gam_interaction_reduced_k10.RData"))  # Reduced interaction model -- reduce based on which variables got shrunk
gam_interaction_reduced <- reduced_interaction_model

load(here("models", "creek_fire_lm_interaction_reduced.RData"))                
lm_interaction_reduced <- best_no_lat_lon_interaction_model

########################### Create Predicted Rasters ##############################

# extract fitted values
fitted_values_gam_interaction_reduced <- predict(gam_interaction_reduced, valid_df_scaled, type = "response")
fitted_values_lm_interaction_reduced <- predict(lm_interaction_reduced, valid_df_scaled, type = "response")

fitted_values_gam_interaction_reduced_tamarack <- predict(gam_interaction_reduced, tamarack_df_scaled, type = "response")
fitted_values_lm_interaction_reduced_tamarack <- predict(lm_interaction_reduced, tamarack_df_scaled, type = "response")

fitted_values_gam_interaction_reduced_knp_complex <- predict(gam_interaction_reduced, knp_complex_df_scaled, type = "response")
fitted_values_lm_interaction_reduced_knp_complex <- predict(lm_interaction_reduced, knp_complex_df_scaled, type = "response")

# Create a SpatRaster object from the data frame
coords <- data.frame(lon = valid_df$lon, lat = valid_df$lat)
coords_tamarack <- data.frame(lon = tamarack_df$lon, lat = tamarack_df$lat)
coords_knp_complex <- data.frame(lon = knp_complex_df$lon, lat = knp_complex_df$lat)

# save these in a df and csv to be safe
fitted_values_df <- data.frame(response=valid_df_scaled$aet,
                               lm_interaction_reduced=fitted_values_lm_interaction_reduced,
                               gam_interaction_reduced=fitted_values_gam_interaction_reduced)

fitted_values_df_tamarack <- data.frame(response=tamarack_df$aet,
                               lm_interaction_reduced=fitted_values_lm_interaction_reduced_tamarack,
                               gam_interaction_reduced=fitted_values_gam_interaction_reduced_tamarack)

fitted_values_df_knp_complex <- data.frame(response=knp_complex_df$aet,
                                lm_interaction_reduced=fitted_values_lm_interaction_reduced_knp_complex,
                                gam_interaction_reduced=fitted_values_gam_interaction_reduced_knp_complex)

write_csv(fitted_values_df,here("results", "creek_fire", "fitted_values_df.csv"))
write_csv(fitted_values_df_tamarack,here("results", "creek_fire", "tamarack_fire_fitted_values_df.csv"))
write_csv(fitted_values_df_knp_complex,here("results", "creek_fire", "knp_complex_fire_fitted_values_df.csv"))

# Convert the data frame to a terra vector (SpatVector)
points_response_rET <- terra::vect(coords, crs = "EPSG:4326")
points_fitted_values_gam_interaction_reduced <- terra::vect(coords, crs = "EPSG:4326")
points_fitted_values_lm_interaction_reduced <- terra::vect(coords, crs = "EPSG:4326")

points_response_rET_tamarack <- terra::vect(coords_tamarack, crs = "EPSG:4326")
points_fitted_values_gam_interaction_reduced_tamarack <- terra::vect(coords_tamarack, crs = "EPSG:4326")
points_fitted_values_lm_interaction_reduced_tamarack <- terra::vect(coords_tamarack, crs = "EPSG:4326")

points_response_rET_knp_complex <- terra::vect(coords_knp_complex, crs = "EPSG:4326")
points_fitted_values_gam_interaction_reduced_knp_complex <- terra::vect(coords_knp_complex, crs = "EPSG:4326")
points_fitted_values_lm_interaction_reduced_knp_complex <- terra::vect(coords_knp_complex, crs = "EPSG:4326")

################################# Make Rasters #################################

values(points_response_rET) <- fitted_values_df$response
values(points_fitted_values_gam_interaction_reduced) <- fitted_values_df$gam_interaction_reduced
values(points_fitted_values_lm_interaction_reduced) <- fitted_values_df$lm_interaction_reduced

# Rasterize the points
raster_template <- rast(ext(points_response_rET), resolution = 0.001, crs = "EPSG:4326")
response_rast <- rasterize(points_response_rET, raster_template, field="value")
gam_interaction_reduced_rast <- rasterize(points_fitted_values_gam_interaction_reduced, raster_template, field="value")
lm_interaction_reduced_rast <- rasterize(points_fitted_values_lm_interaction_reduced, raster_template, field="value")

# Write the outputs
writeRaster(response_rast, here("results", "response_rast.tif"), overwrite = TRUE)
writeRaster(gam_interaction_reduced_rast, here("results", "gam_interaction_reduced_predictions.tif"), overwrite = TRUE)
writeRaster(lm_interaction_reduced_rast, here("results", "lm_interaction_reduced_predictions.tif"), overwrite = TRUE)

########################### Repeat for test data ###############################

values(points_response_rET_tamarack) <- fitted_values_df_tamarack$response
values(points_fitted_values_gam_interaction_reduced_tamarack) <- fitted_values_df_tamarack$gam_interaction_reduced
values(points_fitted_values_lm_interaction_reduced_tamarack) <- fitted_values_df_tamarack$lm_interaction_reduced

# Rasterize the points
raster_template_tamarack <- rast(ext(points_response_rET_tamarack), resolution = 0.001, crs = "EPSG:4326")
response_rast_tamarack <- rasterize(points_response_rET_tamarack, raster_template, field="value")
gam_interaction_reduced_rast_tamarack <- rasterize(points_fitted_values_gam_interaction_reduced_tamarack, raster_template_tamarack, field="value")
lm_interaction_reduced_rast_tamarack <- rasterize(points_fitted_values_lm_interaction_reduced_tamarack, raster_template_tamarack, field="value")

# Write the outputs
writeRaster(response_rast_tamarack, here("results", "response_rast.tif"), overwrite = TRUE)
writeRaster(gam_interaction_reduced_rast_tamarack, here("results", "gam_interaction_reduced_predictions.tif"), overwrite = TRUE)
writeRaster(lm_interaction_reduced_rast_tamarack, here("results", "lm_interaction_reduced_predictions.tif"), overwrite = TRUE)

################################################################################

values(points_response_rET_knp_complex) <- fitted_values_df_knp_complex$response
values(points_fitted_values_gam_interaction_reduced_knp_complex) <- fitted_values_df_knp_complex$gam_interaction_reduced
values(points_fitted_values_lm_interaction_reduced_knp_complex) <- fitted_values_df_knp_complex$lm_interaction_reduced

# Rasterize the points
raster_template_knp_complex <- rast(ext(points_response_rET_knp_complex), resolution = 0.001, crs = "EPSG:4326")
response_rast_knp_complex <- rasterize(points_response_rET_knp_complex, raster_template, field="value")
gam_interaction_reduced_rast_knp_complex <- rasterize(points_fitted_values_gam_interaction_reduced_knp_complex, raster_template_knp_complex, field="value")
lm_interaction_reduced_rast_knp_complex <- rasterize(points_fitted_values_lm_interaction_reduced_knp_complex, raster_template_knp_complex, field="value")

# Write the outputs
writeRaster(response_rast_knp_complex, here("results", "response_rast.tif"), overwrite = TRUE)
writeRaster(gam_interaction_reduced_rast_knp_complex, here("results", "gam_interaction_reduced_predictions.tif"), overwrite = TRUE)
writeRaster(lm_interaction_reduced_rast_knp_complex, here("results", "lm_interaction_reduced_predictions.tif"), overwrite = TRUE)

# title: "create_table"
# author: "Carrie Hashimoto"
# date: "2024-11-14"
  
## Setup:
library(terra)
library(ggplot2)
library(dplyr)
library(data.table)
library(here)


## Import all the data
# buffer vec
buffer <- terra::vect(here("data/boundaries/creek_fire_buffer.geojson"))

# HUC12 in 10 km buffer
huc12 <- terra::vect(here("data/boundaries/creek_fire_huc12_in_buffer.geojson"))

# Import the actual fire boundary
boundary <- terra::vect(here("data/boundaries/creek_fire_boundary.geojson"))

# ET data
prefire_et <- terra::rast(here("data/openet/tiffs/prefire_mean_et.tif"))
names(prefire_et) <- "prefire_et"

postfire_et <- terra::rast(here("data/openet/tiffs/postfire_mean_et.tif"))
names(postfire_et) <- "postfire_et"

# DEM data (note: NAD83 instead of WGS84)
elevation <- terra::rast(here("data/dem/usgs_10m_dem.tif"))
names(elevation) <- "elevation"

slope <- terra::rast(here("data/dem/usgs_10m_slope.tif"))
names(slope) <- "slope"

aspect <-terra::rast(here("data/dem/usgs_10m_aspect.tif"))
names(aspect) <- "aspect"

northness <-terra::rast(here("data/dem/usgs_10m_northness.tif"))
names(northness) <- "northness"

# NLDAS temperature at surface
temperature <- terra::rast(here("data/nldas/nldas_prefire_tasmean.tif"))
names(temperature) <- "temperature"

# TerraClimate precipitation and soil moisture
precipitation <- terra::rast(here("data/terraclimate/prefire_ppt.tif"))
names(precipitation) <- "precipitation"

soil_moisture <- terra::rast(here("data/terraclimate/prefire_soil.tif"))
names(soil_moisture) <- "soil_moisture"

# HLS prefire, postfire, and changes in NDVI and NBR
prefire_ndvi <- terra::rast(here("data/hls/prefire_mean_ndvi.tif"))
names(prefire_ndvi) <- "prefire_ndvi"

prefire_nbr <- terra::rast(here("data/hls/prefire_mean_nbr.tif"))
names(prefire_nbr) <- "prefire_nbr"

dndvi <- terra::rast(here("data/hls/diff_mean_ndvi.tif"))
names(dndvi) <- "dndvi"

dnbr <- terra::rast(here("data/hls/diff_mean_nbr.tif"))
names(dnbr) <- "dnbr"

rdnbr <- dnbr / sqrt(abs(prefire_nbr/1000)) # From Ma and Qin 2020
names(rdnbr) <- "rdnbr"

rasterized_boundary <- terra::rasterize(boundary, prefire_et, field=TRUE, background=0)
names(rasterized_boundary) <- "burned"

rasterized_buffer <- terra::rasterize(buffer, prefire_et, field=TRUE, background=0)
names(rasterized_buffer) <- "in_buffer"

rasterized_huc12 <- terra::rasterize(huc12, prefire_et, field="HUC12", background=NA)
names(rasterized_huc12) <- "huc12"

postfire_et_resampled <- terra::resample(postfire_et,prefire_et)
elevation_resampled <- terra::resample(elevation,prefire_et)
temperature_resampled <- terra::resample(temperature,prefire_et)
precipitation_resampled <- terra::resample(precipitation,prefire_et)
soil_moisture_resampled <- terra::resample(soil_moisture,prefire_et)
slope_resampled <- terra::resample(slope,prefire_et)
aspect_resampled <- terra::resample(aspect,prefire_et)
northness_resampled <- terra::resample(northness,prefire_et)
prefire_ndvi_resampled <-  terra::resample(prefire_ndvi,prefire_et)
prefire_nbr_resampled <-  terra::resample(prefire_nbr,prefire_et)
dndvi_resampled <- terra::resample(dndvi,prefire_et)
dnbr_resampled <- terra::resample(dnbr,prefire_et)
rdnbr_resampled <- terra::resample(rdnbr,prefire_et)


# prefire_et_cropped <- terra::crop(prefire_et,boundary_vec,mask=TRUE)
# postfire_et_resampled_cropped <- terra::crop(postfire_et_resampled,boundary_vec,mask=TRUE)
# 
# elevation_resampled_cropped <- terra::crop(elevation_resampled,boundary_vec,mask=TRUE)
# temperature_resampled_cropped <- terra::crop(temperature_resampled,boundary_vec,mask=TRUE)
# precipitation_resampled_cropped <- terra::crop(precipitation_resampled,boundary_vec,mask=TRUE)
# soil_moisture_resampled_cropped <- terra::crop(soil_moisture_resampled,boundary_vec,mask=TRUE)
# prefire_ndvi_resampled_cropped <- terra::crop(prefire_ndvi_resampled,boundary_vec,mask=TRUE)
# prefire_nbr_resampled_cropped <- terra::crop(prefire_nbr_resampled,boundary_vec,mask=TRUE)
# dndvi_resampled_cropped <- terra::crop(dndvi_resampled,boundary_vec,mask=TRUE)
# dnbr_resampled_cropped <- terra::crop(dnbr_resampled,boundary_vec,mask=TRUE)
# rdnbr_resampled_cropped <- terra::crop(rdnbr_resampled,boundary_vec,mask=TRUE)
# slope_resampled_cropped <- terra::crop(slope_resampled,boundary_vec,mask=TRUE)
# aspect_resampled_cropped <- terra::crop(aspect_resampled,boundary_vec,mask=TRUE)
# north_resampled_cropped <- terra::crop(north_resampled,boundary_vec,mask=TRUE)
# rasterized_boundary_cropped <- terra::crop(rasterized_boundary,boundary_vec,mask=TRUE)
# rasterized_buffer_cropped <- terra::crop(rasterized_buffer,boundary_vec,mask=TRUE)
# rasterized_HUC12_cropped <- terra::crop(rasterized_HUC12,boundary_vec,mask=TRUE)


# Extract values from other rasters
coords <- terra::crds(prefire_et)
colnames(coords) <- c("x","y")
prefire_et_values <- terra::extract(prefire_et, coords)
postfire_et_values <- terra::extract(postfire_et_resampled, coords)
temperature_values <- terra::extract(temperature_resampled, coords)
precipitation_values <- terra::extract(precipitation_resampled, coords)
elevation_values <- terra::extract(elevation_resampled, coords)
soil_moisture_values <- terra::extract(soil_moisture_resampled, coords)
prefire_ndvi_values <- terra::extract(prefire_ndvi_resampled, coords)
prefire_nbr_values <- terra::extract(prefire_nbr_resampled, coords)
dndvi_values <- terra::extract(dndvi_resampled, coords)
dnbr_values <- terra::extract(dnbr_resampled, coords)
rdnbr_values <- terra::extract(rdnbr_resampled, coords)
aspect_values <- terra::extract(aspect_resampled, coords)
slope_values <- terra::extract(slope_resampled, coords)
northness_values <- terra::extract(northness_resampled, coords)
rasterized_boundary_values <- terra::extract(rasterized_boundary, coords)
rasterized_buffer_values <- terra::extract(rasterized_buffer, coords)
rasterized_huc12_values <- terra::extract(rasterized_huc12, coords)

# Create the data.table
data_table <- data.table(
  lat = coords[, "y"],
  lon = coords[, "x"],
  within_burn_area = rasterized_boundary_values[,"burned"],
  within_buffer = rasterized_buffer_values[,"in_buffer"],
  huc12 = rasterized_huc12_values[,"huc12"],
  prefire_ndvi = prefire_ndvi_values[,"prefire_ndvi"],
  prefire_nbr = prefire_nbr_values[,"prefire_nbr"],
  dndvi = dndvi_values[, "dndvi"],
  dnbr = dnbr_values[, "dnbr"], 
  rdnbr = rdnbr_values[, "rdnbr"],
  prefire_et = prefire_et_values[, "prefire_et"],              
  postfire_et = postfire_et_values[, "postfire_et"],
  temp = temperature_values[, "temperature"],    
  precip = precipitation_values[, "precipitation"], 
  elev = elevation_values[, "elevation"],    
  aspect = aspect_values[,"aspect"], 
  slope = slope_values[,"slope"], 
  northness = northness_values[, "northness"],
  soil_moisture = soil_moisture_values[, "soil_moisture"] 
)

# remove rows outside the buffer
data_table_cleaned <- data_table[!is.na(data_table$within_buffer)]

# everything remaining is inside the buffer
# so if it still has within_boundary is.na, then it must be in the buffer
# but outside the burn area
data_table_cleaned[is.na(data_table_cleaned$within_burn_area)]$within_burn_area <- FALSE

# save results
write.csv(data_table_cleaned, here("data/big_table.csv"), row.names = FALSE)

# # check point spatial distribution
# set.seed(0)  # For reproducibility
# sample_size <- 1000  # Number of random points to sample
# 
# subset_df <- data_table_cleaned %>%
#   sample_n(sample_size)
# plot(creekfire_geojson)
# 
# # Plot the selected points using ggplot2
# ggplot(subset_df, aes(x = lon, y = lat)) +
#   geom_point(alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Random Subset of Longitude and Latitude Points",
#        x = "Longitude", y = "Latitude")




library(here)
library(terra)
library(tidyterra)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(sf)

# New fire -- try Caldor and Rim and see if I totally use up all quota
fire_name <- "Tamarack"
fire_name_clean <- gsub(" ", "_", str_to_lower(fire_name))
fire_folder_path <- here("data", paste0(fire_name_clean, "_fire"))
boundary_folder <- here(fire_folder_path, "boundaries")


# Make a new data folder for the new fire
dir.create(file.path(fire_folder_path))

et_dir <- here(fire_folder_path, "openet")
fire_path <- here(boundary_folder, paste0(str_to_lower(gsub(" ", "_", fire_name)), "_fire.geojson"))
fire_buffer_path <- here(fire_folder_path, "boundaries", paste0(fire_name_clean, "_fire_buffer.geojson"))


################################################################################
source(here("scripts/boundary_processing.R"))

buffer_dist <- 10000 # km

dir.create(file.path(boundary_folder))
if (length(strsplit(fire_name, " ")[[1]]) > 1){
  fire_name <- paste(str_to_upper(strsplit(fire_name, " ")[[1]][1]), (strsplit(fire_name, " ")[[1]][2]))
}else{
  fire_name <- str_to_upper(fire_name)
}
fire <- load_fire_boundaries(fire_name, boundary_folder)

# Create a buffer around the fire boundary
fire_buffer <- get_buffer(fire, fire_name, boundary_folder, buffer_dist)

# Create a bounding box around the fire boundary with buffer
fire_bbox <- get_bbox(fire_name, fire_buffer)

ggplot() +
  geom_spatvector(data = fire_bbox, fill="transparent") +
  geom_spatvector(data = fire, col="red", fill="transparent") +
  geom_spatvector(data = fire_buffer, col="blue", fill="transparent") +
  labs(title=paste0(fire_name," Boundary and Buffer"))

# st_area(fire)/4046.85642 # ac
# st_area(fire_buffer)/4046.85642 # ac

################################################################################
source(here("scripts/openet_acquisition.R"))

# Don't download unnecessarily since it'll exhaust quota
# Get API key from Renviron
openet_api_key <- Sys.getenv("OPENET_API_KEY")

# API Keys
# EqFb7ww4pmEbwqEiFeCShw7IL7MON7CJ92SUgt1YQjaIUqtZ25g4pzUj65Rb
# SOHFWhh9xdZoT5tGd3fvwTSiRnSI1T6FcXapVbxGXVoEhMdic0hQRxp2ispR
# hJtPCOsBINr85OpQde0gK7rupLfYUroAnUvmV6Kb671UYgflmoEryXDvzR5G
# VV1j9F1JIHLb4jOMMgygKrMJMWggXQ9Fv4us83MDaPIEmfwjczhfZVEpWTBX
# 0Tr9BOVrWgXm6RCBSd75jSjSF1DmXLAYbwyOJEZONYaSUPDuAPq6PhKyoufj

# This should add up to 200,000 + 50,000*4 = 400,000 ac I can download
# So if I want data for 6 months, I can only have an area of 130,000 ac
# Including buffers if I use July and August aliquots


# So identify a central Sierra fire that's ~40,000 acres I guess
# Ideas
# Donnell Fire 2018
# Could be good to try to do both Donnell and Pier fires since they
# together cover the 4 main climate types in the Creek Fire burn area
# Start with Pier since it has the two main ones
# whereas the Continental Subarctic climate is not much of the Creek Fire area

# Define output directory
dir.create(file.path(et_dir))

# Get pre- and post-fire summer months' ET
# Find the fire start date and get the 3 years before it
all_fires <- st_read(here("data", "california_fire_perimeters_all.geojson"))

# Select the fire boundary from all fires
fire_data <- all_fires[all_fires$FIRE_NAME == fire_name, ]

fire_data <- fire_data[which.max(fire_data$GIS_ACRES), ]

start_date <- as.Date(fire_data$ALARM_DATE)
et_date_ranges <- list(seq(as.Date(paste0(as.character(year(start_date)-3), "-06-01")), length.out=2, by="2 month"),
                    seq(as.Date(paste0(as.character(year(start_date)-2), "-06-01")), length.out=2, by="2 month"),
                    seq(as.Date(paste0(as.character(year(start_date)-1), "-06-01")), length.out=2, by="2 month"),
                    seq(as.Date(paste0(as.character(year(start_date)+1), "-06-01")), length.out=2, by="2 month"))

# Check OpenET download quota
check_quota(openet_api_key)

# Load buffer vector
fire_buffer_from_geojson <- st_read(fire_buffer_path)

# Get tiles of appropriate size to download from OpenET; plot=TRUE optionally
tiles <- get_tiles(fire_buffer,TRUE)

# Download the tiles with desired interval, model, reference ET, units, and variable
download_et(et_dir, tiles, et_date_ranges, "monthly", "ensemble", "gridMET", "mm", "ET", openet_api_key)

# Stack the rasters in each tile's folder
save_raster_stacks(et_dir,tiles)

################################################################################
source(here("scripts/openet_processing.R"))

# combine the tiles
combined_tiles <- combine_tiles(et_dir, output_name="full_mosaic", write=TRUE)

prefire_date_values <- as.Date(unlist(lapply(et_date_ranges[1:3], function(x) {seq(from=as.Date(x[1]), to=as.Date(x[-1]), by="1 month")})))

postfire_date_values <- as.Date(unlist(lapply(et_date_ranges[4], function(x) {seq(from=as.Date(x[1]), to=as.Date(x[-1]), by="1 month")})))

# calculate the mean ET for the prefire period
prefire_et <- calculate_mean_ET(combined_tiles, et_dir, prefire_date_values, output_name="prefire_et", write=TRUE)

# calculate the mean ET for the postfire period
postfire_et <- calculate_mean_ET(combined_tiles, et_dir, postfire_date_values, output_name="postfire_et", write=TRUE)

# ggplot of prefire ET with boundary, buffer, and bounding box
# Get global min and max across both rasters
all_vals <- c(terra::global(prefire_et, "min", na.rm = TRUE)$min,
              terra::global(prefire_et, "max", na.rm = TRUE)$max,
              terra::global(postfire_et, "min", na.rm = TRUE)$min,
              terra::global(postfire_et, "max", na.rm = TRUE)$max)

et_min <- min(all_vals)
et_max <- max(all_vals)

# Prefire
prefire_et_plot <- ggplot() +
  geom_spatraster(data = prefire_et, aes(fill = prefire_et)) +
  geom_spatvector(data = fire$geometry, fill = NA, color = "white") +
  geom_spatvector(data = fire_buffer, fill = NA, color = "yellow") +
  scale_fill_viridis_c(limits = c(et_min, et_max), name="Mean ET (mm)") +
  labs(title = "Pre-fire mean ET") +
  theme_minimal()

# Postfire
postfire_et_plot <- ggplot() +
  geom_spatraster(data = postfire_et, aes(fill = postfire_et)) +
  geom_spatvector(data = fire$geometry, fill = NA, color = "white") +
  geom_spatvector(data = fire_buffer, fill = NA, color = "yellow") +
  scale_fill_viridis_c(limits = c(et_min, et_max), name="Mean ET (mm)") +
  labs(title = "Post-fire mean ET") +
  theme_minimal()

# Fixed... thank the Lord
grid.arrange(prefire_et_plot, postfire_et_plot, ncol=2)

################################################################################
source(here("scripts/dem_acquisition.R"))

# Load API key
opentopo_api_key <- Sys.getenv("OPENTOPOGRAPHY_API_KEY")

dem_dir <- here(fire_folder_path, "dem")
dir.create(file.path(dem_dir))

output_name <- "usgs_10m_dem"

et_ref_path <- here(et_dir, "tiffs", "full_mosaic.tif")

# Get the DEM
dem <- get_dem(dem_dir, fire_buffer, output_name, et_ref_path, opentopo_api_key, write=TRUE)

# Plot it
ggplot() +
  geom_spatraster(data = dem, aes(fill = usgs_10m_dem)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "USGS DEM") +
  scale_fill_viridis_c() +
  theme_minimal()

################################################################################
source(here("scripts/dem_feature_engineering.R"))

dem_path <- here(fire_folder_path, "dem", "usgs_10m_dem.tif")
output_name <- here(fire_folder_path, "dem")

# Calculate slope, aspect, and northness
dem_processed_outputs <- calculate_slope_aspect_northness(dem_path, output_name, "usgs_10m", write=TRUE)

# Plot the outputs
slope_plot <- ggplot() +
  geom_spatraster(data = dem_processed_outputs$slope, aes(fill = slope)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Slope") +
  scale_fill_viridis_c() +
  theme_minimal()

aspect_plot <- ggplot() +
  geom_spatraster(data = dem_processed_outputs$aspect, aes(fill = aspect)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Aspect") +
  scale_fill_viridis_c() +
  theme_minimal()

northness_plot <- ggplot() +
  geom_spatraster(data = dem_processed_outputs$northness, aes(fill = northness)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Northness") +
  scale_fill_viridis_c() +
  theme_minimal()

grid.arrange(slope_plot, aspect_plot, northness_plot, ncol=3)

################################################################################
source(here("scripts/hls_acquisition.R"))
source(here("scripts/earthdata_netrc_setup.R"))

set_gdal_configs()

# set up date ranges of interest by reformatting the date ranges
hls_date_ranges <- c(paste0(prefire_date_values, "T00:00:00Z/", prefire_date_values %m+% months(1), "T00:00:00Z"),
  paste0(postfire_date_values, "T00:00:00Z/", postfire_date_values %m+% months(1), "T00:00:00Z"))

# define the output directory
hls_path <- here(fire_folder_path, "hls")
dir.create(file.path(hls_path))

# call the function
save_hls_bands(fire_buffer_path, hls_date_ranges, et_ref_path, hls_path)

################################################################################
source(here("scripts/hls_feature_engineering.R"))

# set the path to the reference raster
reference_raster_path <- here(et_dir, "tiffs", "prefire_et.tif")

# save the NDVI and NBR rasters
save_ndvi(here(hls_path), here(reference_raster_path), year(start_date))
save_nbr(here(hls_path), here(reference_raster_path), year(start_date))


################################################################################
source(here("scripts/terraclimate_acquisition.R"))

terraclimate_dir <- here(fire_folder_path, "terraclimate")
dir.create(file.path(terraclimate_dir))

# Download the data
download_terraclimate_soil_ppt(fire_buffer_path, terraclimate_dir, start_date)

# Take a look
prefire_ppt <- terra::rast(here(fire_folder_path, "terraclimate", "prefire_ppt.tif"))
prefire_soil <- terra::rast(here(fire_folder_path, "terraclimate", "prefire_soil.tif"))
prefire_vpd <- terra::rast(here(fire_folder_path, "terraclimate", "prefire_vpd.tif"))
prefire_PDSI <- terra::rast(here(fire_folder_path, "terraclimate", "prefire_PDSI.tif"))
prefire_tmax <- terra::rast(here(fire_folder_path, "terraclimate", "prefire_tmax.tif"))

ppt_plot <- ggplot() +
  geom_spatraster(data = prefire_ppt$prefire_ppt, aes(fill = prefire_ppt)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Pre-fire 6-mo. mean precipitation") +
  scale_fill_viridis_c(name="mm") +
  theme_minimal()

soil_plot <- ggplot() +
  geom_spatraster(data = prefire_soil$prefire_soil, aes(fill = prefire_soil)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Pre-fire 6-mo. mean soil moisture") +
  scale_fill_viridis_c(name="mm") +
  theme_minimal()

vpd_plot <- ggplot() +
  geom_spatraster(data = prefire_vpd$prefire_vpd, aes(fill = prefire_vpd)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Pre-fire 6-mo. mean vpd") +
  scale_fill_viridis_c(name="kPa") +
  theme_minimal()

pdsi_plot <- ggplot() +
  geom_spatraster(data = prefire_PDSI$prefire_PDSI, aes(fill = prefire_PDSI)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Pre-fire 6-mo. PDSI") +
  scale_fill_viridis_c(name="Value (unitless)") +
  theme_minimal()

tmax_plot <- ggplot() +
  geom_spatraster(data = prefire_tmax$prefire_tmax, aes(fill = prefire_tmax)) +
  geom_spatvector(data = fire$geometry, fill = NA, col="white") +
  geom_spatvector(data = fire_buffer, fill = NA, col="yellow") +
  labs(title = "Pre-fire 6-mo. tmax") +
  scale_fill_viridis_c(name="Deg. C") +
  theme_minimal()

grid.arrange(ppt_plot, soil_plot, vpd_plot, pdsi_plot, tmax_plot, ncol=3, nrow=2)

################################################################################

# On to combining the data
source(here("scripts","create_table.R"))

fire_folder_path <- here("data", paste0(fire_name_clean,"_fire"))
buffer_path <- here(fire_folder_path, "boundaries", paste0(fire_name_clean, "_fire_buffer.geojson"))
boundary_path <- here(fire_folder_path, "boundaries", paste0(fire_name_clean, "_fire.geojson"))
prefire_et_path <- here(fire_folder_path, "openet", "tiffs", "prefire_et.tif")
postfire_et_path <- here(fire_folder_path, "openet", "tiffs", "postfire_et.tif")
elevation_path <- here(fire_folder_path, "dem", "usgs_10m_dem.tif")
slope_path <- here(fire_folder_path, "dem", "usgs_10m_slope.tif")
aspect_path <- here(fire_folder_path, "dem", "usgs_10m_aspect.tif")
northness_path <- here(fire_folder_path, "dem", "usgs_10m_northness.tif")
ppt_path <- here(fire_folder_path, "terraclimate", "prefire_ppt.tif")
soil_path <- here(fire_folder_path, "terraclimate", "prefire_soil.tif")
tmax_path <- here(fire_folder_path, "terraclimate", "prefire_tmax.tif")
vpd_path <- here(fire_folder_path, "terraclimate", "prefire_vpd.tif")
pdsi_path <- here(fire_folder_path, "terraclimate", "prefire_PDSI.tif")
prefire_ndvi_path <- here(fire_folder_path, "hls", "mean_prefire_ndvi.tif")
prefire_nbr_path <- here(fire_folder_path, "hls", "mean_prefire_nbr.tif")
dndvi_path <- here(fire_folder_path, "hls", "diff_mean_ndvi.tif")
dnbr_path <- here(fire_folder_path, "hls", "diff_mean_nbr.tif")
out_path <- here(fire_folder_path, paste0(fire_name_clean, "_fire_big_table.csv"))

make_table(buffer_path,
           boundary_path,
           prefire_et_path,
           postfire_et_path,
           elevation_path,
           slope_path,
           aspect_path,
           northness_path,
           tmax_path,
           ppt_path,
           soil_path,
           vpd_path,
           pdsi_path,
           prefire_ndvi_path,
           prefire_nbr_path,
           dndvi_path,
           dnbr_path,
           out_path
)

################################################################################
source(here("scripts","feature_engineering.R"))

# Feature engineering to get control points, aet, and ret
big_table_path <- here(fire_folder_path, paste0(fire_name_clean, "_fire_big_table.csv"))
out_path_with_outliers <- here(fire_folder_path, paste0(fire_name_clean, "_fire_model_df_with_outliers.csv"))
out_path_outliers_removed <- here(fire_folder_path, paste0(fire_name_clean, "_fire_model_df_outliers_removed.csv"))

create_modeling_df(big_table_path, out_path_with_outliers, out_path_outliers_removed)

# Check for data processing errors
big_table <- vroom(out_path_outliers_removed)

# check point spatial distribution
set.seed(0)  # For reproducibility
sample_size <- 1000  # Number of random points to sample

subset_df <- big_table %>%
  sample_n(sample_size)

# plot the selected points using ggplot2 with aET shown
ggplot(subset_df, aes(x = lon, y = lat, color = aet)) +
  geom_point() +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Distribution of Selected Points", color = "Absolute Evapotranspiration (mm/mo)")



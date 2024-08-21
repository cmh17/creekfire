# title: HLS_feature_engineering.R
# author: Carrie Hashimoto
# version: 2024-08-18

# load necessary packages
library(terra)
library(tidyterra)
library(ggplot2)
library(rprojroot)
library(gridExtra)

# define output directory
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", "HLS_data", fsep="/")
suppressWarnings(dir.create(outDir)) 

# load prefire ET raster for resampling purposes
prefire_et <- terra::rast(paste0(outDir,"/openET_data/tiffs/prefire_mean_ET.tif"))

# get all the dates for which you have scenes
all_files <- list.files(paste0(outDir, "/red/"))
scene_files <- all_files[grepl("mosaicked", all_files)] # don't want individual pieces

# extract the scene dates
scene_dates <- as.Date(sapply(scene_files, FUN=function(x) {
  substr(strsplit(x, "_")[[1]][2], start=2, stop=11)
}))

# extract whether it's from Landsat or Sentinel-2
scene_labels <- sapply(scene_files, FUN=function(x) {
  substr(strsplit(x, "_")[[1]][2], start=1, stop=1)
})

# Load the NIR, RED, SWIR2, and Fmask rasters
bands <- c("red", "nir", "swir2", "fmask")
stacks <- list()

# collect all the rasters in stacks
for (i in 1:length(bands)) {
  band_stack <- list()
  for (j in 1:length(scene_dates)) {
    raster_path <- paste0(outDir, bands[i], "/", bands[i], "_", scene_labels[j], scene_dates[j], "_mosaicked.tiff")
    band_stack[[j]] <- terra::resample(terra::rast(raster_path), prefire_et)
  }
  stacks[[bands[i]]] <- band_stack
}


# define functions to calculate NDVI and NBR
calculate_ndvi <- function(nir, red){
  (nir - red) / (nir + red)
}

calculate_nbr <- function(nir, swir2){
  (nir - swir2) / (nir + swir2)
}

# initialize NDVI and NBR stacks
ndvi_stack <- list()
nbr_stack <- list()

# calculate NDVI and NBR for each scene
for (t in 1:length(scene_dates)) {
  if (!is.null(stacks$nir[[t]]) & !is.null(stacks$red[[t]]) & !is.null(stacks$swir2[[t]])) {
    ndvi_stack[[t]] <- calculate_ndvi(stacks$nir[[t]], stacks$red[[t]])
    nbr_stack[[t]] <- calculate_nbr(stacks$nir[[t]], stacks$swir2[[t]])
    
    # save scene dates
    names(ndvi_stack[[t]]) <- names(nbr_stack[[t]]) <- scene_dates[t]
  } else {
    message(paste("Missing data for date:", scene_dates[t]))
  }
}

# initialize cleaned stacks
ndvi_stack_cleaned <- list()
nbr_stack_cleaned <- list()

for (i in 1:length(ndvi_stack)) {
  ndvi_layer <- ndvi_stack[[i]]
  nbr_layer <- nbr_stack[[i]]
  
  # remove any values outside of (-1, 1)
  ndvi_layer[ndvi_layer < -1 | ndvi_layer > 1] <- NA
  nbr_layer[nbr_layer < -1 | nbr_layer > 1] <- NA
  
  # remove any remaining non-finite values... they should be gone already
  ndvi_layer[!is.finite(ndvi_layer)] <- NA
  nbr_layer[!is.finite(nbr_layer)] <- NA
  
  # save the cleaned scene
  ndvi_stack_cleaned[[i]] <- ndvi_layer
  nbr_stack_cleaned[[i]] <- nbr_layer
}

# define the function to apply the quality filter
apply_quality_filter <- function(ndvi_raster, quality_raster) {
  
  # create a mask from the quality raster
  masking_layer <- quality_raster
  
  # define the acceptable quality criteria (bits 7-6 should be 01 or 00)
  # it's an 8-bit value, so it must be under 128 in decimal
  # if you want bit 7 to be 0
  masking_layer[masking_layer >= 128] <- NA
  
  # mask the NDVI raster with the quality mask
  masked_ndvi <- mask(ndvi_raster, masking_layer, maskvalue=NA)
  
  return(masked_ndvi)
}

# apply the quality filter to each NDVI raster
filtered_ndvi_stack <- mapply(apply_quality_filter, ndvi_stack_cleaned, stacks[[4]], SIMPLIFY=FALSE)

# get the indices for each year
prefire_indices <- as.vector(which(format(scene_dates, "%Y") == "2020"))
postfire_indices <- as.vector(which(format(scene_dates, "%Y") == "2021"))

# calculate the mean NDVI and NBR for the prefire period

# get just the prefire scenes for NDVI
prefire_ndvi_stack <- terra::rast(ndvi_stack_cleaned[prefire_indices])

# apply mean to get average summer NDVI
prefire_ndvi <- terra::app(prefire_ndvi_stack, fun=mean, na.rm=TRUE)

# same with NBR
prefire_nbr_stack <- terra::rast(nbr_stack_cleaned[prefire_indices])
prefire_nbr <- terra::app(prefire_nbr_stack, fun=mean, na.rm=TRUE)

# calculate the mean NDVI and NBR for the postfire period too
postfire_ndvi_stack <- terra::rast(ndvi_stack_cleaned[postfire_indices])
postfire_ndvi <- terra::app(postfire_ndvi_stack, fun=mean, na.rm=TRUE)

postfire_nbr_stack <- terra::rast(nbr_stack_cleaned[postfire_indices])
postfire_nbr <- terra::app(postfire_nbr_stack, fun=mean, na.rm=TRUE)

# save the processed rasters
terra::writeRaster(prefire_ndvi, paste0(outDir, "/prefire_mean_ndvi.tif"), overwrite=TRUE)
terra::writeRaster(prefire_nbr, paste0(outDir, "/prefire_mean_nbr.tif"), overwrite=TRUE)
terra::writeRaster(postfire_ndvi, paste0(outDir, "/postfire_mean_ndvi.tif"), overwrite=TRUE)
terra::writeRaster(postfire_nbr, paste0(outDir, "/postfire_mean_nbr.tif"), overwrite=TRUE)

# I don't love that I'm using d(mean(NDVI)) instead of mean(dNDVI)
# but I don't have a solution for that right now
dndvi_mean <- prefire_ndvi-postfire_ndvi
dnbr_mean <- prefire_nbr-postfire_nbr

# save the difference rasters too
terra::writeRaster(dndvi_mean, paste0(outDir, "HLS_data/diff_mean_ndvi.tif"), overwrite=TRUE)
terra::writeRaster(dnbr_mean, paste0(outDir, "HLS_data/diff_mean_nbr.tif"), overwrite=TRUE)

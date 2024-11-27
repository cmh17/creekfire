# title: DEM_feature_engineering.R
# author: Carrie Hashimoto
# version: 2024-08-19

library(terra)
library(rprojroot)

# set working directory and output directory
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "data", "dem", fsep="/")
suppressWarnings(dir.create(outDir)) 

# load DEM
dem <- terra::rast(paste0(wd,"/data/dem/usgs_10m_dem.tif"))

# calculate slope in degrees
slope <- terra::terrain(dem, v = "slope", unit = "degrees")

# calculate aspect in degrees
aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")

# convert slope and aspect to radians
slope_radians <- slope * pi / 180
aspect_radians <- aspect * pi / 180

# calculate northness = sin(slope) * cos(aspect) (Qin and Ma)
northness <- sin(slope_radians) * cos(aspect_radians)
names(northness) <- "northness"

# save raster results
writeRaster(slope, paste0(outDir, "/usgs_10m_slope.tif"), overwrite = TRUE)
writeRaster(aspect, paste0(outDir, "/usgs_10m_aspect.tif"), overwrite = TRUE)
writeRaster(northness, paste0(outDir, "/usgs_10m_northness.tif"), overwrite = TRUE)

# title: PRISM_data_processing.R
# author: Carrie Hashimoto
# version: 2024-08-19

# just a very short script to change PRISM climate normals for
# precipitation and temperature from .bil to .tif

library(terra)
library(rprojroot)

# set working directory and output directory
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", "PRISM_data", fsep="/")
suppressWarnings(dir.create(outDir)) 

# load the .bil file
ppt_bil <- terra::rast(paste0(wd,"/Data/PRISM_data/PRISM_ppt/PRISM_ppt.bil"))
tmean_bil <- terra::rast(paste0(wd,"/Data/PRISM_data/PRISM_tmean/PRISM_tmean.bil"))

# write the raster to a .tif file
writeRaster(ppt_bil, paste0(outDir, "/PRISM_ppt.tif"))
writeRaster(tmean_bil, paste0(outDir, "/PRISM_tmean.tif"))

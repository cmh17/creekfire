# openET_data_processing.R
# author: Caroline Hashimoto
# version: 2024-08-17

# setup

packages <- c("rprojroot")

new.packages = packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/') else print('All required packages are installed.')

invisible(lapply(packages, library, character.only = TRUE))

###############################################################################

# mosaic together all the stacks from the boxes

# create output directory if it doesn't exist
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", "openET_data", fsep="/")
suppressWarnings(dir.create(outDir)) 

# get all the boxes
# might need to modify this if there are things in the folder that aren't box directories
boxes <- list.files(paste0(outDir,"/tiffs"))

# get the buffer boundary so you include control points
boundary_vect <- terra::vect(paste0(wd,"/Data/Boundary_data/creekfire_1km_buffer.geojson"))

# save the extent of the buffer, too
roi_ext <- ext(boundary_vect)

# initialize stack to make mosaic
big_stack <- list()

# add all the boxes' stacks to a bigger stack
for (i in 1:length(boxes)) {
  # for each box, grab the agg stack
  file_path <- paste0(outDir, "/tiffs/", boxes[i], "/", boxes[i], "_stack.tif")
  box_stack <- terra::rast(file_path)
  
  # and add it to a list
  big_stack[[i]] <- box_stack
}

# mosaic together all the stacked pieces and label
# note: mosaicking is averaging; hopefully should have a more consistent
# result than merging
# if you do this or merge since these areas shouldn't overlap
mosaicked <- terra::mosaic(terra::sprc(big_stack))

# we have monthly data, so the dates should be as follows, regenerate them
# for now, just have to hardcode this
names(mosaicked) <- c(seq(as.Date("2020-06-01"), as.Date("2020-08-01"), "months"),
                      seq(as.Date("2021-06-01"), as.Date("2021-08-01"), "months"))

# raster stack of each date's ET for whole bounding box
writeRaster(mosaicked,paste0(outDir,"/tiffs/full_mosaic.tif"))

# grab prefire dates for prefire mean and postfire dates for postfire mean... simple
prefire_mean_ET <- mean(mosaicked$`2020-06-01`,mosaicked$`2020-07-01`,mosaicked$`2020-08-01`)
postfire_mean_ET <- mean(mosaicked$`2021-06-01`,mosaicked$`2021-07-01`,mosaicked$`2021-08-01`)
names(postfire_mean_ET) <- names(prefire_mean_ET) <- "mean_ET"

writeRaster(prefire_mean_ET,paste0(outDir,"/tiffs/prefire_mean_ET.tif"))
writeRaster(postfire_mean_ET,paste0(outDir,"/tiffs/postfire_mean_ET.tif"))


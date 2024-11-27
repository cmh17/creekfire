# title: HLS_data_acquisition.R
# author: Carrie Hashimoto
# version: 2024-08-18
# Adapted from HLS R tutorial: https://git.earthdata.nasa.gov/scm/lpdur/hls_tutorial_r.git

library(here)
library(terra)
library(jsonlite)
library(sp)
library(httr)
library(magrittr)
library(htmltools)

# call .netrc file
source("Scripts/earthdata_netrc_setup.R")

# create search query function
get_search <- function(date_range,boundary_str) {
  # input date_range (vec of str) and boundary_str (str)
  search_body <- list(limit=250,
                      datetime=date_range,
                      bbox= boundary_str,
                      collections= HLS_col)
  
  search_req <- httr::POST(search_URL, body = search_body, encode = "json") %>%
    httr::content(as = "text") %>%
    fromJSON()
  
  cat('There are',search_req$numberMatched, 'features that matched our request for', date_ranges[i], "\n")
  
  return(search_req)
}

# set GDAL configs
setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")

# function for searchable data table
make_table <- function(search_features, i) {
  
  granule_list <- list()
  n <- 1
  for (item in row.names(search_features)){                       # get the NIR, Red, SWIR2 and Quality band layer names
    if (search_features[item,]$collection == 'HLSS30_2.0'){ # Sentinel-2
      ndvi_bands <- c('B8A','B04','B12','Fmask')
    }
    else{ # Landsat
      ndvi_bands <- c('B05','B04','B07','Fmask')
    }
    for(b in ndvi_bands){
      f <- search_features[item,]
      b_assets <- f$assets[[b]]$href
      
      df <- data.frame(Collection = f$collection,                    # make a data frame including links and other info
                       Granule_ID = f$id,
                       Cloud_Cover = f$properties$`eo:cloud_cover`,
                       band = b,
                       Asset_Link = b_assets, stringsAsFactors=FALSE)
      granule_list[[n]] <- df
      n <- n + 1
    }
  }
  
  # create a searchable datatable
  search_df <- do.call(rbind, granule_list)
  return(search_df)
}

# check .netrc location
cat('The netrc file can be found in:', Sys.getenv('HOME'))

# create a function to switch crs of boundary to that of HLS data
update_crs <- function(search_df,boundary_vect) {
  # extract data's crs
  coordinate_reference <- terra::rast(paste0(search_df$Asset_Link[1]))
  
  # update boundary's crs
  boundary_updated <- terra::project(boundary_vect, crs(coordinate_reference)) # transfer crs

  return(boundary_updated)
}

# create function to load rasters based on a searchable data table created
load_rasters <- function(search_df, boundary_load_rasters) {
  
  # initialize stacks
  red_stack <- nir_stack <- fmask_stack <- swir2_stack <- list()
  date_vect <- c()
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = length(search_df$band), initial = 0, style = 3)
  
  # initialize counters for each stack
  l <- m <- n <- k <- 0
  
  # create function to extract date from raster source
  extract_date <- function(granule_id) {
    doy_time <- strsplit(granule_id, "[.]")[[1]][4]
    doy <- substr(doy_time, 1, 7)
    dat <- as.Date(as.integer(substr(doy, 5, 7)), origin = paste0(substr(doy, 1, 4), "-01-01"))
    return(dat)
  }
  
  # go through every row in the search_df
  for (b in 1:length(search_df$band)) {
    
    # increment the progress bar
    setTxtProgressBar(pb, b)
    
    # retrieve this file
    b_layer <- tryCatch({
      terra::rast(paste0('/vsicurl/', search_df$Asset_Link[b]))
    }, error = function(e) {
      warning(paste("Error loading raster:", e))
      return(NULL)
    })
    
    if (is.null(b_layer)) next
    
    lab <- substr(search_df$Granule_ID[b], 5, 5)
    dat <- extract_date(search_df$Granule_ID[b])
    
    # project it to WGS
    b_layer <- tryCatch({
      terra::project(b_layer, "EPSG:4326")
    }, error = function(e) {
      warning(paste("Error in reprojection:", e))
      return(NULL)
    })
    
    if (is.null(b_layer)) next
    
    # crop it to the boundary of interest
    b_layer_crop <- tryCatch({
      terra::crop(b_layer, terra::ext(boundary_load_rasters))
    }, error = function(e) {
      warning(paste("Error in cropping raster:", e))
      return(NULL)
    })
    
    if (is.null(b_layer_crop)) next
    
    # depending on what the band is, add it to the appropriate stack
    if (search_df$band[b] == 'B04') {
      l <- l + 1
      red_stack[[l]] <- b_layer_crop
      
      # save dates for one of the bands' data (assuming all bands have same date)
      date_vect[l] <- ifelse(lab == 'S', paste0('S', as.character(dat)), paste0('L', as.character(dat)))
      
    } else if (search_df$band[b] == 'Fmask') {
      m <- m + 1
      fmask_stack[[m]] <- b_layer_crop
      
    } else if (search_df$band[b] == 'B05' || search_df$band[b] == 'B8A') {
      n <- n + 1
      nir_stack[[n]] <- b_layer_crop
      
    } else if (search_df$band[b] == 'B12' || search_df$band[b] == 'B07') {
      k <- k + 1
      swir2_stack[[k]] <- b_layer_crop
    }
  }
  
  # close the progress bar
  close(pb)
  
  # return all the stacks
  return(list(red_stack, nir_stack, fmask_stack, swir2_stack, date_vect))
}

###############################################################################

# main

# assign search url
search_URL <- 'https://cmr.earthdata.nasa.gov/stac/LPCLOUD/search'

# assign data names
HLS_col <- list("HLSS30.v2.0", "HLSL30.v2.0")

# retrieve spatial boundary
boundary_vect <- terra::vect("data/boundaries/creek_fire_buffer.geojson")

# get spatial extent of boundary
roi <- terra::ext(boundary_vect) # ext gets a spatial extent
roi_bbox <- paste(roi[1], roi[3], roi[2], roi[4], sep = ',')
prefire_et <- terra::rast("data/openet/tiffs/prefire_mean_et.tif")

# set up date ranges of interest

# Prefire 2020-06-01 to 2020-08-31
# Postfire 2021-06-01 to 2020-08-31
date_ranges <- c('2020-06-01T00:00:00Z/2020-07-01T00:00:00Z',
                 '2020-07-01T00:00:00Z/2020-08-01T00:00:00Z',
                 '2020-08-01T00:00:00Z/2020-09-01T00:00:00Z',
                 '2021-06-01T00:00:00Z/2021-07-01T00:00:00Z',
                 '2021-07-01T00:00:00Z/2021-08-01T00:00:00Z',
                 '2021-08-01T00:00:00Z/2021-09-01T00:00:00Z')
i <- 1
for (i in 1:length(date_ranges)) {
  print(date_ranges[i])
  creekfire_search_req <- get_search(date_ranges[i],roi_bbox) # roi_bbox defined above
  
  # this takes in an extent string
  search_df <- make_table(creekfire_search_req$features,i)
  
  # only use features without excess cloud cover
  search_df <- search_df[search_df$Cloud_Cover < 30, ]
  
  raster_output <-  load_rasters(search_df,boundary_vect)
  red_stack <- raster_output[[1]]
  nir_stack <- raster_output[[2]]
  fmask_stack <- raster_output[[3]]
  swir2_stack <- raster_output[[4]]
  date_vect <- raster_output[[5]]
  cat("Rasters loaded for", date_ranges[i], "\n")
  
  bands <- c("red","nir","swir2","fmask")
  stacks <- list(red_stack,nir_stack,swir2_stack,fmask_stack)
  
  
  # merge them and save rasters covering the whole area and the individuals too
  for (k in 1:length(bands)) {
    suppressWarnings(dir.create(here(paste0("data/hls/",bands[k]))))
    # for each unique scene date,
    for (d in 1:length(unique(date_vect))) {
      # get the date
      scene_date <- unique(date_vect)[d]
      
      # find the indices of the rasters for the scene
      scene_indices <- which(date_vect == scene_date)
      
      # subset the stack data for that scene
      scene_data <- stacks[[k]][scene_indices] 
      # kth list item is a list and [c(...)] should return spatraster items from it
      # so scene_data is a list of rasters in one scene for one band
      
      # need to resample to match resolution for every raster in the scene
      # so I can mosaic the scene together
      # this methodology seems somewhat questionable, ask someone about it
      scene_data_resampled <- lapply(scene_data,FUN=function(x){terra::resample(x,prefire_et,method="near")}) # match res to et
      
      # save these all to be safe; label the pieces of the scene 1,...,h,
      for (h in 1:length(scene_data_resampled)) {
        terra::writeRaster(scene_data_resampled[[h]],here(paste0("data/hls/",bands[k],"/",bands[k],"_",
                                                            scene_date,"_",h,".tif")),
                           overwrite=TRUE)
      }
      
      # mosaic them together
      scene_data_merged <- terra::mosaic(terra::sprc(scene_data_resampled))
      
      # save mosaicked file
      terra::writeRaster(scene_data_merged,here(paste0("data/hls/",bands[k],"/",bands[k],"_",
                                                  scene_date,"_mosaicked.tif")),
                         overwrite=TRUE)
    }
    cat("Rasters saved for", bands[k], "band. \n")
  }
}

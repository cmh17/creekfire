# source: https://github.com/codeswitching/OpenET-API-with-R-tutorial/blob/main/README.md
# edited by Carrie Hashimoto
# version: 2024-08-17

# load packages
packages <- c('terra','jsonlite','sp','httr','ggplot2','xml2','dygraphs',
              'xts','lubridate','DT','rmarkdown', 'rprojroot','imager','htmltools',
              "raster","ggplot2","rasterVis","tmap")

new.packages = packages[!(packages %in% installed.packages()[,"Package"])]

# install new packages
if(length(new.packages)) install.packages(
  new.packages, repos='http://cran.rstudio.com/') else print('All required packages are installed.')
invisible(lapply(packages, library, character.only = TRUE))


# create an output directory if it doesn't exist
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", "OpenET_data", fsep="/")
suppressWarnings(dir.create(outDir)) 
suppressWarnings(dir.create(paste0(outDir,"/zips"))) 
suppressWarnings(dir.create(paste0(outDir,"/tiffs"))) 

httr::set_config(httr::config(ssl_verifypeer=0L))

# function to format box coordinates
make_box <- function(xmin,xmax,ymin,ymax) {
  # use ordering appropriate for OpenET
  # topleft, bottomleft, bottomright, topright (I think?)
  return(c(xmin,ymax,xmin,ymin,xmax,ymin,xmax,ymax))
}

make_JSON <- function(date_range = c('2020-01-01', '2020-12-31'), model = 'ensemble',
                      variable = c('et','ndvi'), reference_et = 'cimis', units = 'english',
                      geometry = c(-119.7937, 35.58995, -119.7937, 35.53326, -119.71268, 35.53326, -119.71268, 35.58995),
                      interval = 'monthly') {
  # inputs
  ## date_range: c('YYYY-MM-DD','YYYY-MM-DD')
  ## model: str
  ## variable: str or vec of strs
  ## reference_et: str
  ## units: str
  ## geometry: double vec, c(xmin, ymax, xmin, ymin, xmax, ymin, xmax, ymax)
  ## interval: str
  
  # outputs: none
  
  toJSON(list(
    date_range = date_range,
    geometry = geometry,
    interval = interval,
    model = model,
    reference_et = reference_et,
    units = units,
    variable = variable
  ), auto_unbox = TRUE)
}
################################################################################

# determine the number of boxes needed to get area per box under 200,000 ac

# first import the boundary vector
boundary_vect <- terra::vect("Data/Boundary_data/CreekFire_1km_buffer.geojson")
boundary_vect_ext <- terra::ext(boundary_vect)
bbox_polygon <- as.polygons(boundary_vect_ext, crs=crs(boundary_vect))

# calculate the area in km2
bbox_polygon_area <- terra::expanse(bbox_polygon, unit="km")

# choose a coarse resolution while keeping the area of each box under 200,000 ac
box_area <- 200000/247.105381 # ac to km2
edge_length <- sqrt(box_area) # in km

# calculate minimum number of boxes
min_num_boxes <- ceiling(bbox_polygon_area/box_area)

make_box(boundary_vect_ext[1],boundary_vect_ext[2],
                           boundary_vect_ext[3],boundary_vect_ext[4])

# divide the horiz and vert distances between the points by the approximate side length of the 200,000 ac sub-boxes
# to get the number of boxes on the x and y sides

###############################################################################

# and round up to get boxes that fit the area exactly... maybe not necessary
# but without it, have issues w the rasterization
num_y_box <- ceiling(max(terra::distance(matrix(make_box(boundary_vect_ext[1],boundary_vect_ext[2],
                           boundary_vect_ext[3],boundary_vect_ext[4]),ncol=2,byrow=TRUE), lonlat=TRUE, sequential=FALSE, pairs=TRUE, symmetrical=TRUE, unit="km")[c(1,6),3]/edge_length))


num_x_box <- ceiling(max(terra::distance(matrix(make_box(boundary_vect_ext[1],boundary_vect_ext[2],
                                                    boundary_vect_ext[3],boundary_vect_ext[4]),ncol=2,byrow=TRUE), lonlat=TRUE, sequential=FALSE, pairs=TRUE, symmetrical=TRUE, unit="km")[c(3,4),3]/edge_length))

# to get the resolution, divide the side lengths in degrees by number of divisions
x_res <- (boundary_vect_ext[2] - boundary_vect_ext[1])/num_x_box
y_res <- (boundary_vect_ext[4] - boundary_vect_ext[3])/num_y_box

boundary_rast <- terra::rasterize(boundary_vect,terra::rast(boundary_vect,
                                                            res=c(x_res,y_res)),
                                  touches=TRUE) # use touches=TRUE to get all the boundary inside the boxes chosen

# now get the coordinates for each each pixel
selected_boxes <- which(values(boundary_rast) == 1)

# create a matrix to store the bounding points for the pixels with a value of 1
n_boxes <- length(selected_boxes)

# initialize bounds list
box_geometries <- list()

# loop through selected boxes to get bounding points
for (i in 1:n_boxes) {
  box_ext <- ext(boundary_rast, cells = selected_boxes[i])
  box_geometries[[i]] <- make_box(box_ext$xmin, box_ext$xmax, box_ext$ymin, box_ext$ymax)
}

names(selected_boxes) <- names(box_geometries) <- 1:length(selected_boxes)

###############################################################################

# define the URL and API key
url_text <- "https://openet-api.org/raster/geotiff/stack"
api_key <- "api key here"

# define date range - use 3 month increments or so 
date_ranges <- list(c('2020-06-01', '2020-08-31'),
                    c('2021-06-01', '2021-08-31'))

i <- 1
j <- 1
for (i in 1:length(box_geometries)){
  suppressWarnings(dir.create(paste0(outDir, "/zips/box",i))) 
  suppressWarnings(dir.create(paste0(outDir, "/tiffs/box",i))) 
  for (j in 1:length(date_ranges)) {
    payload <- make_JSON(
      date_range = date_ranges[[j]],
      geometry = box_geometries[[i]],
      interval = "monthly",
      model = "ensemble",
      reference_et = "gridMET",
      units = "mm",
      variable = "ET")
    
    # print the payload for verification
    cat("payload:",payload)
    
    # Make the POST request
    response <- POST(
      url = url_text,
      add_headers(
        accept = "application/json",
        `Content-Type` = "application/json",
        Authorization = api_key
      ),
      body = payload,
      encode = "json"
    )
    
    # print the response content and status code
    cat("box: ", names(selected_boxes)[[i]], ", date_range: ", date_ranges[[j]], ", status_code: ",print(status_code(response)), "\n", sep="")
    
    response_urls <- content(response, "parsed") # I'm not totally sure what this line does...
    
    # download the zips
    for (k in 1:length(response_urls)) {
      
      # create an output directory if it doesn't exist
      wd <- rprojroot::find_rstudio_root_file()
      
      # save both the zips and the tifs to be safe
      zip_dir <- file.path(wd, "Data/openET_data/zips", paste0("box",names(selected_boxes)[[i]]), fsep="/")
      tif_dir <- file.path(wd, "Data/openET_data/tiffs", paste0("box",names(selected_boxes)[[i]]), fsep="/")
      box_response_name <- paste0("box",names(selected_boxes)[[i]],"_",names(response_urls)[k])
      suppressWarnings(dir.create(zip_dir))
      
      download.file(as.character(response_urls[k]),
                    destfile=paste0(zip_dir,"/",box_response_name,".zip"),
                    method = "curl")
      
      unzip(paste0(zip_dir,"/",box_response_name,".zip"),
            exdir = tif_dir)
      
      file.rename(from=paste0(tif_dir,"/ensemble_et_",names(response_urls)[k],".tif"),
                  to=paste0(tif_dir,"/",box_response_name,".tif"))
    }
    
    cat("box", names(selected_boxes)[[i]], "dates", date_ranges[[j]], "completed.")
  }
}

# check the number of queries you have left
check_quota <- GET(
  url = "https://openet-api.org/account/status",
  add_headers(
    accept = "application/json",
    `Content-Type` = "application/json",
    Authorization = api_key
  ),
  encode = "json"
)

content(check_quota, "parsed")

###############################################################################

# create stacks of the monthly rasters for each box

for (i in 1:length(selected_boxes)){
  
  # get all files in box i directory that are for individual dates
  dir_str <- paste0("Data/openET_data/tiffs/",paste0("box",selected_boxes[i]))
  dir_contents <- list.files(dir_str)
  match_name <- grepl(pattern=".+_[0-9]{4}-[0-9]{2}-[0-9]{2}.tif",x=dir_contents)
  
  # initialize stack
  et_stack <- list()
  # add all ET date tifs into stack
  for (j in 1:length(dir_contents[match_name])) {
    file_str <- paste0(dir_str,"/",dir_contents[j])
    et_layer <- terra::rast(file_str)
    # exclude infs
    et_layer[!is.finite(values(et_layer))] <- NA # might need to confirm this method works
    et_stack[[j]] <- et_layer
    rm(et_layer) # don't clutter up working memory
  }
  
  # put the stack into one raster
  et_stack <- terra::rast(et_stack)
  output_name <- file.path(dir_str, paste0("box",selected_boxes[i], "_stack.tif"))
  
  # save the raster for the box
  terra::writeRaster(et_stack, output_name, overwrite = TRUE)
  cat(output_name, "created. \n")
  
}


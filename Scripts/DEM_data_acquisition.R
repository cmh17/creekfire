# title: DEM_data_acquisition.R
# author: Carrie Hashimoto
# version: 2024-08-09

packages <- c('terra','jsonlite','sp','httr',
              'rasterVis','ggplot2','magrittr','RColorBrewer','xml2','dygraphs',
              'xts','lubridate','DT','rmarkdown', 'rprojroot','imager','htmltools',
              'tidyterra')

# identify missing (not installed) packages
new.packages = packages[!(packages %in% installed.packages()[,"Package"])]

# install new (not installed) packages
if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/') else print('All required packages are installed.')

# load libraries
invisible(lapply(packages, library, character.only = TRUE))

# set working directory and output directory
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", "DEM_data", fsep="/")
suppressWarnings(dir.create(outDir)) 

# retrieve spatial boundary
boundary_vect <- terra::vect(paste0(wd,"/Data/Boundary_data/CreekFire_1km_buffer.geojson"))

# get spatial extent of boundary
roi <- terra::ext(boundary_vect) # ext gets a spatial extent
prefire_et <- terra::rast(paste0(wd,"/Data/openET_data/tiffs/prefire_mean_ET.tif"))

# input API key
API_Key <- "api key here"

# input URL text
url_base <- "https://portal.opentopography.org/API/usgsdem"

# create input URL for GET
get_url <- function(url_base,datasetName,roi,outputFormat,API_Key){
  return(paste0(url_base,"?",
                "datasetName=",datasetName,"&",
                "south=",roi[3],"&",
                "north=",roi[4],"&",
                "west=",roi[1],"&",
                "east=",roi[2],"&",
                "outputFormat=",outputFormat,"&",
                "API_Key=",API_Key))
}

payload <- get_url(url_base,"USGS10m",roi,"GTiff",API_Key)

response <- GET(
  url = payload,
  accept("application/octet-stream")
  )

# check if the request was successful
if (status_code(response) == 200) {
  # write content to a file
  writeBin(content(response, "raw"), paste0(outDir,"/USGS_10m_DEM.tif"))
} else {
  print("Failed to download the file.")
}

# # check the result
# dem <- terra::rast(paste0(outDir,"/USGS_10m_DEM.tif"))
# plot(dem)


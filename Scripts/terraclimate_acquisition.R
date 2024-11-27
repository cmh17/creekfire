# https://www.climatologylab.org/uploads/2/2/1/3/22133936/terraclimate_downloadv2.r
########### Alternative version using RNetCDF package ###############################################################################################
library(here)
library(RNetCDF)
library(terra)

# Load geojson to get boundary box
boundary_vect <- terra::vect(here("data/boundaries/creek_fire_buffer.geojson"))

# Get lat range and lon range
boundary_vect_ext <- terra::ext(boundary_vect)

#Enter lat and lon ranges -- adding buffer here bc they're not capturing the edges
lat.range <- c(boundary_vect_ext[3]-0.1,boundary_vect_ext[4]+0.1)        #! Ranges instead of point values. Order does not matter
lon.range <- c(boundary_vect_ext[1]-0.1,boundary_vect_ext[2]+0.1)

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
vars <- c("soil","ppt")

for (var in vars){
  baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")
  
  nc <- open.nc(baseurlagg)
  lon <- var.get.nc(nc, "lon")
  lat <- var.get.nc(nc, "lat")
  lat.range <- sort(lat.range)                              #!sort user input values from low to high
  lon.range <-sort(lon.range)
  lat.index <- which(lat>=lat.range[1]&lat<=lat.range[2])    #! index values within specified range
  lon.index <- which(lon>=lon.range[1]&lon<=lon.range[2])    
  lat.n <- length(lat.index)                                #!value for count
  lon.n <- length(lon.index)
  start <- c(lon.index[1], lat.index[1], 1)
  count <- c(lon.n, lat.n, NA)                            #! parameter change: 'NA' instead of '-1' to signify entire dimension
  
  
  # read in the full period of record using aggregated files
  
  data <-var.get.nc(nc, variable = var,start = start, count,unpack=TRUE)    #! argument change: 'variable' instead of 'varid'  # Output is now a matrix
  
  # Timesteps from 1958
  timesteps <- seq((2020-1958)*12+3, (2020-1958)*12+8)  #! 6 months prior to the fire
  
  mean_data <- apply(data[,,timesteps],c(1,2),mean)  #! Output is now a matrix
  
  # soil moisture as a raster
  raster <- raster::raster(t(mean_data), xmn=min(lon[lon.index]), xmx=max(lon[lon.index]), ymn=min(lat[lat.index]), ymx=max(lat[lat.index]),
                              crs="+proj=longlat +datum=WGS84")

  # save it
  raster::writeRaster(raster, here("data", "terraclimate", paste0("prefire_", var, ".tif")), overwrite=TRUE)
}


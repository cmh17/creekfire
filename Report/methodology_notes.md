---
title: Methodology Notes
author: Carrie Hashimoto
version: 2024-08-06
output:
  pdf_document: default
  html_document: default
---

### Data sources:

- OpenET: 30 m, monthly
  + Get average monthly ET data for study area bounding box for June, July, and August 2020 and 2021
  + files: openET_data_acquisition.Rmd, openET_data_processing.Rmd
  
- Harmonized Landsat Sentinel-2: 30 m, ~5 days
  + Get all Landsat and Sentinel-2 images for the bounding box for 2020-06-01 through 2020-08-31 and 2021-06-01 through 2021-08-31
  + files: HLS_data_acquisition.Rmd, HLS_feature_engineering.Rmd
    > check methodology on combining scenes; some potential issues
      1. I assume if images are from the same day, they're part of the same scene, but I could be missing images if one scene falls over multiple days
      2. Do I really need to make the rasters into scenes before I average them, or should I just average everything after resampling?
      3. Instead of mean dNDVI, I have difference in mean NDVI (and NBR) since in order to get dNDVI, I think I'd need to match up dates before and after the fire better. Is this okay? How can I make it more meaningful?
  
- Shuttle Radar Topography Mission: 30 m, measured in 2000
  + Get the raster containing the study area (it's quite a bit larger) and crop
  + Calculate aspect, slope, and northness
    > Did this in QGIS, but it's easier to do with R terra package - update with USGS DEM later on
  + I just downloaded by inputting the buffer area bounding box coords into EarthData, but I'd like to write a script to acquire this via API to make it easier
  + Alternatively, look into **OpenTopography** for 1 m (probably not, since fire area exceeds data limit) or 10 m resolution
  
- TerraClimate: 4 km, monthly
  + Get cumulative precipitation for the study area bounding box from 2020-03-01 through 2020-08-31
  + Get average soil moisture for the study area bounding box from 2020-03-01 through 2020-08-31
  + Can I get any higher resolution data on this?
    > consider replacing precipitation with PRISM normal
    > could use SMAP soil moisture - HydroBlocks is interesting, but not sure how to use considering 2015-2019 timeframe

- North American Land Data Assimilation System: 12 km temperature data
  + Really low resolution compared to other data; consider replacing with PRISM 800 m temperature normals
    > Or something else?
  
- Calfire boundaries for recent wildfires - selected Creek Fire boundary from it
  + Used ArcGIS to create a 10 km buffer area around it from which to select control points
  
- USGS HUC12 watersheds intersecting with the wildfire boundary
  + Initially planned to use this for finding control points, but changed methodology from Ma et al 2020 study
  + Instead used control points based on similar elevation, northness, temperature, and soil moisture
    > Probably need to do more lit review and refine this control point selection process
  
- Additional data sources to consider adding:
  + biodiversity - what metrics
  + canopy structure / forest structure - waveform lidar
  
## Modeling

- Notes on modeling to come


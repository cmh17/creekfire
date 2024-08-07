---
title: Methodology Notes
author: Carrie Hashimoto
version: 2024-08-06
output:
  pdf_document: default
  html_document: default
---


## Data collection

### Data sources:

- OpenET: 30 m, monthly
  + Get average monthly ET data for study area bounding box for June, July, and August 2020 and 2021
  + files: openET_data_acquisition.Rmd, openET_data_processing.Rmd
  
- Harmonized Landsat Sentinel-2: 30 m, ~5 days?
  + Get all Landsat and Sentinel-2 images for the bounding box for 2020-06-01 through 2020-08-31 and 2021-06-01 through 2021-08-31
  + files: HLS_processing.Rmd
    * check methodology on combining scenes; some potential issues
      1. I assume if images are from the same day, they're part of the same scene, but I could be missing images if one scene falls over multiple days
      2. Do I really need to make the rasters into scenes before I average them, or should I just average everything after resampling so they have the same resolution and coordinate reference system?
      3. I reproject the rasters to WGS before saving them instead of keeping them in UTM zones 10 and 11; might be losing accuracy or something or creating issues with projction
  
- Shuttle Radar Topography Mission: 30 m, measured in 2000
  + Get the raster containing the study area (it's quite a bit larger)
  
- TerraClimate: 4 km, monthly or hourly??
  + Get cumulative precipitation for the study area bounding box from 2020-03-01 through 2020-08-31
  + Get average soil moisture for the study area bounding box from 2020-03-01 through 2020-08-31
  + can I get any higher resolution data on this?

- North American Land Data Assimilation System: 12 km
  + **consider replacing this with ECOSTRESS, ASTER, or some other higher res. data source?**
  
- Calfire boundaries for recent wildfires - select the Creek Fire from it
  + Used ArcGIS to create a 10 km buffer area around it from which to select control points
  
- USGS HUC12 watersheds intersecting with the wildfire boundary
  + Initially planned to use this for finding control points, but changed methodology from Ma et al 2020 study



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
  
- Harmonized Landsat Sentinel-2: 30 m, ~5 days
  + Get all Landsat and Sentinel-2 images for the bounding box for 2020-06-01 through 2020-08-31 and 2021-06-01 through 2021-08-31
  + files: HLS_data_acquisition.Rmd, HLS_feature_engineering.Rmd
    > check methodology on combining scenes; some potential issues
      1. I assume if images are from the same day, they're part of the same scene, but I could be missing images if one scene falls over multiple days
      2. Do I really need to make the rasters into scenes before I average them, or should I just average everything after resampling so they have the same resolution and coordinate reference system?
      3. I reproject the rasters to WGS before saving them and then crop to the boundary box in WGS instead of keeping them in UTM zones 10 and 11 and converting the boundary box to UTM; might be losing accuracy or something or creating issues with projection
      4. A new edit: I had used merge to make the scenes, but I'm switching to mosaic to hopefully avoid weird patterns in the rasters. Is there a better way to solve that problem?
      5. I've been using SWIR2, but SWIR1 also exists... which one should I be using? Kind of a big issue.
      6. Instead of mean dNDVI, I have difference in mean NDVI (and NBR) since in order to get dNDVI, I think I'd need to match up dates before and after the fire better. Is this okay? How can I make it more meaningful?
  
- Shuttle Radar Topography Mission: 30 m, measured in 2000
  + Get the raster containing the study area (it's quite a bit larger)
  + I just downloaded by inputting the buffer area bounding box coords into EarthData, but I'd like to write a script to acquire this via API to make it easier
  + Alternatively, look into **OpenTopography** for 1 m or 10 m resolution :000
    > https://portal.opentopography.org/apidocs/
  
- TerraClimate: 4 km, monthly
  + Get cumulative precipitation for the study area bounding box from 2020-03-01 through 2020-08-31
  + Get average soil moisture for the study area bounding box from 2020-03-01 through 2020-08-31
  + Can I get any higher resolution data on this?
    > consider replacing precipitation with PRISM normal
    > could use SMAP soil moisture - HydroBlocks is interesting, but not sure how to use considering 2015-2019 timeframe
  
  + Alternative data sources:
    > SMAP 1 km downscaled soil moisture?
    > PRISM for temperature and precipitation?
    > SMAP-HydroBlocks??????? long-term climatology dataset?

- North American Land Data Assimilation System: 12 km
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


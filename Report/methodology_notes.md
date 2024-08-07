---
title: Methodology Notes
author: Carrie Hashimoto
version: 2024-08-06
output:
  pdf_document: default
  html_document: default
---


# Methodology notes

## Data collection

### Data sources:

- OpenET:
  + Get average monthly ET data for study area bounding box for June, July, and August 2020 and 2021
  
- Harmonized Landsat Sentinel-2
  + Get all Landsat and Sentinel-2 images for the bounding box for 2020-06-01 through 2020-08-31 and 2021-06-01 through 2021-08-31
  
- Shuttle Radar Topography Mission
  + Get the raster containing the study area (it's quite a bit larger)
  
- TerraClimate
  + Get cumulative precipitation for the study area bounding box from 2020-03-01 through 2020-08-31
  + Get average soil moisture for the study area bounding box from 2020-03-01 through 2020-08-31
  
  + **consider replacing this with ECOSTRESS, ASTER, or some other higher res. data source?**
  

- North American Land Data Assimilation System
- Calfire boundaries for recent wildfires - select the Creek Fire from it
  + Used ArcGIS to create a 10 km buffer area around it from which to select control points
- USGS HUC12 watersheds intersecting with the wildfire boundary
  + Initially planned to use this for finding control points, but changed methodology from Ma et al 2020 study



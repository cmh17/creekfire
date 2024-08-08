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
  
- TerraClimate: 4 km, monthly or hourly??
  + Get cumulative precipitation for the study area bounding box from 2020-03-01 through 2020-08-31
  + Get average soil moisture for the study area bounding box from 2020-03-01 through 2020-08-31
  + Can I get any higher resolution data on this?
  
  + Alternative data sources:
    > SMAP 1 km downscaled soil moisture?
    > PRISM for temperature and precipitation?
    > SMAP-HydroBlocks??????? long-term climatology dataset?

- North American Land Data Assimilation System: 12 km
  + **consider replacing this with ECOSTRESS, ASTER, or some other higher res. data source?**
  + Or Landsat LST
  + Or PRISM - only 4 km res, but long-term normals... maybe more meaningful than just average over a few months
  
- Calfire boundaries for recent wildfires - select the Creek Fire from it
  + Used ArcGIS to create a 10 km buffer area around it from which to select control points
  
- USGS HUC12 watersheds intersecting with the wildfire boundary
  + Initially planned to use this for finding control points, but changed methodology from Ma et al 2020 study
  
- Additional data sources to add:
  + biodiversity - what metrics?
  + canopy structure / forest structure - waveform lidar

# Open Science Notes

- Adding a README can be confusing
- Gists are single files, only appropriate occasionally

Using Git with terminal

- ll or ls -alh
- git init -> this initializes a git repo in your folder, and after that Git tracks what happens in the folder (?)
- git add <file> -> this "adds it to your cardboard box"
- git status will tell you if a file has been added
- git commit -m "add cloud lesson" commits the files and adds a message

To make it show up online, need to add a remote repository

- lingo that Git uses is "origin" - always means the online repo
- git remote add origin
- git push -u origin main (do this when you do it the first time)
- git push -> this is fine once you do it the first time

If you haven't done the linking step with initializing the remote "origin", it won't work

- git log -> will tell you all the commits you've done in the past, whereas git status will tell you your files and what's changed with them

- git push -> needed to mak the commit

Good research code handbook: https://goodresearch.dev/

Get it out there -> make sure it is commented and files are named clearly -> include an environment -> provide documentation -> ...
- take an hour to add comments / delete stuff you don't need / etc
- use a notebook and add markdown sections

Where do you access SARP data? https://www-air.larc.nasa.gov/ -> have some data, but not spatial, so functionality is limited

Modular code - another step

Resume things - "terminal" and "linux"

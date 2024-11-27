# title: HLS_data_acquisition.R
# author: Carrie Hashimoto
# version: 2024-10-26

library(sf)
library(here)

all_fires <- st_read(here("data/boundaries/california_fire_perimeters_all.geojson"))

# Select the Creek Fire boundary from all fires and save it as a geojson
creek_fire <- all_fires[all_fires$FIRE_NAME == "CREEK", ]

# Sort and choose the one with the greatest area
creek_fire <- creek_fire[which.max(creek_fires$GIS_ACRES), ]

# Save the Creek Fire boundary as a geojson
st_write(creek_fire, here("data/boundaries/creek_fire.geojson"), delete_dsn = TRUE)

# plot(creek_fire$geometry)

# Create a version of the Creek Fire boundary with a 10 km buffer around it
creek_fire_buffer <- st_buffer(creek_fire, dist = 10000)

# plot(creek_fire_buffer$geometry)

# Save the buffered geojson
st_write(creek_fire_buffer, here("data/boundaries/creek_fire_buffer.geojson"), delete_dsn = TRUE)

# Create a bounding box around the Creek Fire boundary with 10 km buffer
creek_fire_bbox <- st_bbox(creek_fire_buffer)

# Turn the bbox into a geojson
creek_fire_bbox <- st_as_sfc(creek_fire_bbox)

# plot(creek_fire_bbox)

# Save the bbox
st_write(creek_fire_bbox, here("data/boundaries/creek_fire_bbox.geojson"), delete_dsn = TRUE)

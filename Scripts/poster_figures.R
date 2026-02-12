# poster_figures.R
# Load necessary libraries
library(here)
library(dplyr)
library(readr)
library(mgcv)
library(Metrics)  
library(vroom)
library(caret)
library(ggplot2)
library(viridis)
library(cowplot)  # For plot_grid
library(terra)
library(ggspatial)
library(tidyterra)
library(scales)

# Load the dataset
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
tamarack_df <- vroom(here("data", "tamarack_fire", "tamarack_fire_model_df_outliers_removed.csv"))
knp_complex_df <- vroom(here("data", "knp_complex_fire", "knp_complex_fire_model_df_outliers_removed.csv"))

# Define predictor variables
predictor_vars <- c("prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
                    "tmax", "precip",  "vpd", "pdsi", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and valid sets
set.seed(2024)
train_index <- createDataPartition(model_df$aet, p = 0.7, list = FALSE)
train_df <- model_df[train_index, ]
valid_df <- model_df[-train_index, ]

# Preprocess the data
preproc_values <- preProcess(train_df[, predictor_vars], method = c("center", "scale"))
train_df_scaled <- train_df
train_df_scaled[, predictor_vars] <- predict(preproc_values, train_df[, predictor_vars])
valid_df_scaled <- valid_df
valid_df_scaled[, predictor_vars] <- predict(preproc_values, valid_df[, predictor_vars])

tamarack_df_scaled <- tamarack_df
tamarack_df_scaled[, predictor_vars] <- predict(preproc_values, tamarack_df[, predictor_vars])
knp_complex_df_scaled <- knp_complex_df
knp_complex_df_scaled[, predictor_vars] <- predict(preproc_values, knp_complex_df[, predictor_vars])

################################################################################

# 1. -Time series of the average ET over the burn area for 2019-2024-
# Maps of dNBR, precipitation, and temperature

# Load the data
# boundaries
creek_fire_buffer <- terra::vect(here("data", "creek_fire", "boundaries", "creek_fire_buffer.geojson"))
creek_fire_boundary <- terra::vect(here("data", "creek_fire", "boundaries", "creek_fire_boundary.geojson"))
creek_fire_buffer$layer <- "10 km buffer"
creek_fire_boundary$layer <- "burn area"
combined_vect <- rbind(creek_fire_buffer, creek_fire_boundary)

viridis_colors <- viridis(6, option = "viridis")

aspect_rast <- terra::rast(here("data", "creek_fire", "dem", "usgs_10m_aspect.tif"))
dnbr_rast <- terra::rast(here("data", "creek_fire", "hls", "diff_mean_nbr.tif"))
soil_rast <- terra::rast(here("data", "creek_fire", "terraclimate", "prefire_soil.tif"))
pdsi_rast <- terra::rast(here("data", "creek_fire", "terraclimate", "prefire_pdsi.tif"))

aspect_map <- ggplot() +
  geom_spatraster(data = aspect_rast) +
  geom_spatvector(
    data = combined_vect, 
    aes(color = layer), 
    fill = NA, 
    lwd = 1,  # Adjust size for thickness
    show.legend = FALSE  # Suppress the "Boundaries" legend
  ) +  
  labs(
    title = "\n\n\nAspect", 
    x = "Latitude", 
    y = "Longitude",
    fill = "Degrees from N"
    # Removed color label since it's suppressed
  ) +
  scale_fill_viridis(option = "viridis") +  
  scale_color_manual(
    values = c(
      "10 km buffer" = viridis_colors[5], 
      "burn area" = viridis_colors[6]
    )
  ) +
  theme(
    legend.position = "right",  # Remove legend completely
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)  # Consistent margins
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add scale bar
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",  # Add north arrow
    style = north_arrow_fancy_orienteering
  )

ggsave(
  here("figures", "creek_fire", "aspect_map.png"), 
  aspect_map, 
  width = 8, 
  height = 8, 
  units = "in",
  bg = "white"  # Ensure consistent background
)

dnbr_map <- ggplot() +
  geom_spatraster(data = dnbr_rast) +
  geom_spatvector(
    data = combined_vect, 
    aes(color = layer), 
    fill = NA, 
    lwd = 1,  # Adjust size for thickness
    show.legend = FALSE  # Suppress the "Boundaries" legend
  ) +  
  labs(
    title = "Difference between\npre-fire and\npost-fire NBR (dNBR)", 
    x = "Latitude", 
    y = "Longitude",
    fill = "dNBR\n(unitless)"
    # Removed color label since it's suppressed
  ) +
  scale_fill_viridis(option = "viridis") +  
  scale_color_manual(
    values = c(
      "10 km buffer" = viridis_colors[5], 
      "burn area" = viridis_colors[6]
    )
  ) +
  theme(
    legend.position = "right",  # Remove legend completely
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)  # Consistent margins
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add scale bar
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",  # Add north arrow
    style = north_arrow_fancy_orienteering
  )

# Save dnbr_map
ggsave(
  here("figures", "creek_fire", "dnbr_map.png"), 
  dnbr_map, 
  width = 8, 
  height = 8, 
  units = "in",
  bg = "white"  # Ensure consistent background
)

soil_map <- ggplot() +
  geom_spatraster(data = soil_rast) +
  geom_spatvector(
    data = combined_vect, 
    aes(color = layer), 
    fill = NA, 
    lwd = 1,  # Adjust size for thickness
    show.legend = FALSE
  ) +  
  labs(
    title = "Pre-fire mean\nmonthly\nsoil moisture", 
    x = "Latitude", 
    y = "Longitude",
    fill = "Soil moisture\n(mm)", 
  ) +
  scale_fill_viridis(option = "viridis") +  
  scale_color_manual(
    values = c(
      "10 km buffer" = viridis_colors[5], 
      "burn area" = viridis_colors[6]
    )
  ) +
  theme(
    legend.position = "right",  # Retain legend in this plot
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)  # Consistent margins
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add scale bar
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",  # Add north arrow
    style = north_arrow_fancy_orienteering
  )

ggsave(
  here("figures", "creek_fire", "soil_map.png"), 
  soil_map, 
  width = 8, 
  height = 8, 
  units = "in",
  bg = "white"  # Ensure consistent background
)

pdsi_map <- ggplot() +
  geom_spatraster(data = pdsi_rast) +
  geom_spatvector(
    data = combined_vect, 
    aes(color = layer), 
    fill = NA, 
    lwd = 1  # Adjust size for thickness
    # No change; legend will be displayed
  ) +  
  labs(
    title = "Pre-fire mean\nmonthly\nPDSI", 
    x = "Latitude", 
    y = "Longitude",
    fill = "(Unitless)", 
    color = "Boundaries"  # Retain color label
  ) +
  scale_fill_viridis(option = "viridis") +  
  scale_color_manual(
    values = c(
      "10 km buffer" = viridis_colors[5], 
      "burn area" = viridis_colors[6]
    )
  ) +
  theme(
    legend.position = "right",  # Retain legend in this plot
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)  # Consistent margins
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add scale bar
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",  # Add north arrow
    style = north_arrow_fancy_orienteering
  )

ggsave(
  here("figures", "creek_fire", "pdsi_map.png"), 
  pdsi_map, 
  width = 8, 
  height = 8, 
  units = "in",
  bg = "white"  # Ensure consistent background
)


# Combine the plots using plot_grid
predictor_maps <- plot_grid(
  aspect_map, 
  dnbr_map, 
  soil_map, 
  pdsi_map,
  ncol = 4, 
  rel_widths = c(1, 1, 1, 1),  # Equal widths for all plots
  align = "hv",              # Align both horizontally and vertically
  axis = "tblr"              # Align axes: top, bottom, left, right
)

# Save the combined figure
ggsave(
  here("figures", "creek_fire", "predictor_maps.png"), 
  predictor_maps, 
  width = 24,      # Adjusted width for better resolution
  height = 8,      # Adjusted height to match individual plots
  units = "in", 
  bg = "white"     # Ensure consistent background
)

################################################################################

# 2. Pretty correlation plot for all data

# Define readable variable labels
var_labels <- c(
  "lat" = "Latitude",
  "lon" = "Longitude",
  "prefire_ndvi" = "Prefire NDVI",
  "prefire_nbr" = "Prefire NBR",
  "dndvi" = "dNDVI",
  "dnbr" = "dNBR",
  "rdnbr" = "RdNBR",
  "tmax" = "Temperature",
  "pdsi" = "PDSI",
  "vpd" = "VPD",
  "precip" = "Precipitation",
  "elev" = "Elevation",
  "aspect" = "Aspect",
  "slope" = "Slope",
  "northness" = "Northness",
  "soil_moisture" = "Soil Moisture"
)

# Compute the correlation matrix
train_cor_matrix <- cor(train_df[, predictor_vars], use = "complete.obs")

# Replace row and column names with readable labels
rownames(train_cor_matrix) <- var_labels[rownames(train_cor_matrix)]
colnames(train_cor_matrix) <- var_labels[colnames(train_cor_matrix)]

# Define a viridis color palette
col_palette <- viridis::viridis(100)

# Save the plot as high-resolution PNG
png(filename = here("figures", "creek_fire", "enhanced_correlation_matrix.png"), 
    width = 2000, height = 1600, res = 300)

# Generate the enhanced correlation matrix plot
corrplot::corrplot(
  train_cor_matrix, 
  method = "color", 
  type = "upper", 
  diag = FALSE,
  tl.col = "black", 
  tl.srt = 45, 
  tl.cex = 0.8, 
  cl.cex = 0.8, 
  col = col_palette, 
  addCoef.col = "black", 
  number.cex = 0.8, 
  mar = c(0, 0, 1, 0), 
  title = "Correlation Matrix of Predictors"
)
dev.off()

################################################################################

############################ Load all the rasters ##############################

# Turn this into a function so I can do it with the base, interaction, and 
# interaction reduced models

# sets_of_paths <- list(c(here("results", "creek_fire", "validation_response_rast.tif"),
#                         here("results", "creek_fire", "validation_gam_interaction_predictions.tif"),
#                         here("results", "creek_fire", "validation_lm_interaction_predictions.tif")),
#                       c(here("results", "creek_fire", "tamarack_response_rast.tif"),
#                         here("results", "creek_fire", "tamarack_gam_interaction_predictions.tif"),
#                         here("results", "creek_fire", "tamarack_lm_interaction_predictions.tif")),
#                       c(here("results", "creek_fire", "knp_complex_response_rast.tif"),
#                         here("results", "creek_fire", "knp_complex_gam_interaction_predictions.tif"),
#                         here("results", "creek_fire", "knp_complex_lm_interaction_predictions.tif")))
# 
# sets_of_titles <- list(c("\n\n\nObserved aET", "\nPredicted aET, \nMLR \nwith interaction", "\nPredicted aET, \nGAM \nwith interaction"),
#                        c("\n\n\nObserved aET", "\nPredicted aET, \nMLR \nwith interaction", "\nPredicted aET, \nGAM \nwith interaction"),
#                        c("\n\n\nObserved aET", "\nPredicted aET, \nMLR \nwith interaction", "\nPredicted aET, \nGAM \nwith interaction"))
# 
# sets_of_filepaths <- list(c("response_plot.png", "predicted_values_lm_interaction_plot.png", "predicted_values_gam_interaction_plot.png", "response_predicted_values_lm_interaction_and_gam_interaction_plots.png"),
#                           c("tamarack_response_plot.png", "tamarack_predicted_values_lm_interaction_plot.png", "tamarack_predicted_values_gam_interaction_plot.png", "tamarack_response_predicted_values_lm_interaction_and_gam_interaction_plots.png"),
#                           c("knp_complex_response_plot.png", "knp_complex_predicted_values_lm_interaction_plot.png", "knp_complex_predicted_values_gam_interaction_plot.png", "knp_complex_response_predicted_values_lm_interaction_and_gam_interaction_plots.png"))

sets_of_paths <- list(c(here("results", "creek_fire", "validation_response_rast.tif"),
                        here("results", "creek_fire", "validation_gam_base_predictions.tif"),
                        here("results", "creek_fire", "validation_lm_base_predictions.tif")),
                      c(here("results", "creek_fire", "tamarack_response_rast.tif"),
                        here("results", "creek_fire", "tamarack_gam_base_predictions.tif"),
                        here("results", "creek_fire", "tamarack_lm_base_predictions.tif")),
                      c(here("results", "creek_fire", "knp_complex_response_rast.tif"),
                        here("results", "creek_fire", "knp_complex_gam_base_predictions.tif"),
                        here("results", "creek_fire", "knp_complex_lm_base_predictions.tif")))

sets_of_titles <- list(c("\n\n\nObserved aET", "\nPredicted aET, \nbase MLR", "\nPredicted aET, \nbase GAM"),
                       c("\n\n\nObserved aET", "\nPredicted aET, \nbase MLR", "\nPredicted aET, \nbase GAM"),
                       c("\n\n\nObserved aET", "\nPredicted aET, \nbase MLR", "\nPredicted aET, \nbase GAM"))

sets_of_filepaths <- list(c("response_plot.png", "predicted_values_lm_base_plot.png", "predicted_values_gam_base_plot.png", "response_predicted_values_lm_base_and_gam_base_plots.png"),
                          c("tamarack_response_plot.png", "tamarack_predicted_values_lm_base_plot.png", "tamarack_predicted_values_gam_base_plot.png", "tamarack_response_predicted_values_lm_base_and_gam_base_plots.png"),
                          c("knp_complex_response_plot.png", "knp_complex_predicted_values_lm_base_plot.png", "knp_complex_predicted_values_gam_base_plot.png", "knp_complex_response_predicted_values_lm_base_and_gam_base_plots.png"))

names(sets_of_paths) <- c("response", "tamarack", "knp_complex")


for (i in seq(length(sets_of_paths))) {
  
  response_rast <- terra::rast(sets_of_paths[[i]][1])
  predicted_values_gam_rast <- terra::rast(sets_of_paths[[i]][2])
  predicted_values_lm_rast <- terra::rast(sets_of_paths[[i]][3])
  
  observed_limits <- c(terra::global(response_rast, "min", na.rm=TRUE)$min, terra::global(response_rast, "max", na.rm=TRUE)$max)
  modeled_limits_gam <- c(terra::global(predicted_values_gam_rast, "min", na.rm=TRUE)$min, terra::global(predicted_values_gam_rast, "max", na.rm=TRUE)$max)
  modeled_limits_lm <- c(terra::global(predicted_values_lm_rast, "min", na.rm=TRUE)$min, terra::global(predicted_values_lm_rast, "max", na.rm=TRUE)$max)
  global_limits <- c(min(observed_limits[1], modeled_limits_gam[1], modeled_limits_lm[1]), max(observed_limits[2], modeled_limits_gam[2], modeled_limits_lm[2]))
  
  ############################ Set up colormaps ##################################
  
  colour_breaks <- sort(c(modeled_limits_gam[1], modeled_limits_lm[1], observed_limits[1], 0, observed_limits[2], modeled_limits_lm[2], modeled_limits_lm[2]))
  v <- viridis(101)
  colours <- c(v[1], v[11], v[21], v[51], v[81], v[91], v[101])# c("darkblue", "lightgreen", "white", "yellow", "orange")
  
  ############################ Make all the plots ################################
  
  # Plot using ggplot2 and tidyterra
  response_plot <- ggplot() +
    annotation_map_tile(
      type = "osm",   
      zoom = 10,             
      cachedir = "cache"     # Directory to cache map tiles
    ) +
    geom_spatraster(data = response_rast) +
    scale_fill_gradientn(
      limits  = global_limits,
      colours = colours[c(1, seq_along(colours), length(colours))],
      values  = c(0, scales::rescale(colour_breaks, from = global_limits), 1),
      na.value = NA
    ) +
    labs(title = sets_of_titles[[1]][1],
         x = "Longitude",
         y = "Latitude",
         fill = "aET (mm/mo)") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = 24),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.background=element_rect(fill = 'transparent', colour = NA)) +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    guides(fill = guide_colorbar(title = "aET (mm/mo)"), nbin=10)
  
  predicted_values_lm_plot <- ggplot() +
    annotation_map_tile(
      type = "osm",   
      zoom = 10,           
      cachedir = "cache"     
    ) +
    geom_spatraster(data = predicted_values_lm_rast) +
    scale_fill_gradientn(
      limits  = global_limits,
      colours = colours[c(1, seq_along(colours), length(colours))],
      values  = c(0, scales::rescale(colour_breaks, from = global_limits), 1),
      na.value = NA
    ) +
    labs(title = sets_of_titles[[1]][2],
         x = "Longitude",
         y = "Latitude",
         fill = "aET (mm/mo)") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = 24),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.background=element_rect(fill = 'transparent', colour = NA)) +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    guides(fill = guide_colorbar(title = "aET (mm/mo)"))
  
  predicted_values_gam_plot <- ggplot() +
    annotation_map_tile(
      type = "osm",   
      zoom = 10,            
      cachedir = "cache"    
    ) +
    geom_spatraster(data = predicted_values_gam_rast) +
    scale_fill_gradientn(
      limits  = global_limits,
      colours = colours[c(1, seq_along(colours), length(colours))],
      values  = c(0, scales::rescale(colour_breaks, from = global_limits), 1),
      na.value = NA
    ) +
    labs(title = sets_of_titles[[1]][3],
         x = "Longitude",
         y = "Latitude",
         fill = "aET (mm/mo)") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = 24),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.background=element_rect(fill = 'transparent', colour = NA)) +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    guides(fill = guide_colorbar(title = "aET (mm/mo)")) 
  
  ggsave(here("figures", "creek_fire", sets_of_filepaths[[i]][1]), response_plot, width = 8,
         height = 8, units = "in")
  
  ggsave(here("figures", "creek_fire", sets_of_filepaths[[i]][2]), predicted_values_lm_plot, width = 8,
         height = 8, units = "in")
  
  ggsave(here("figures", "creek_fire", sets_of_filepaths[[i]][3]), predicted_values_gam_plot, width = 8,
         height = 8, units = "in")
  
  response_predicted_values_lm_plots <- plot_grid(response_plot + theme(legend.position = "none"), predicted_values_lm_plot, ncol = 2, rel_widths = c(1, 1.3))
  
  response_predicted_values_gam_plots <- plot_grid(response_plot + theme(legend.position = "none"), predicted_values_gam_plot, ncol = 2, rel_widths = c(1, 1.3))
  
  response_predicted_values_lm_and_gam_plots <- plot_grid(response_plot + theme(legend.position = "none"), predicted_values_lm_plot + theme(legend.position = "none"), predicted_values_gam_plot, ncol = 3, rel_widths = c(1, 1, 1.5))
  
  ggsave(here("figures", "creek_fire", sets_of_filepaths[[i]][4]), response_predicted_values_lm_and_gam_plots, width = 15,
         height = 8, units = "in", bg = "transparent")
  
  
  
  
  
  }
  
  
  

  
  
  
  
  
  
  
  
  

# 4. Plot of aET vs dNBR with prefire NBR colored using a subset of the data
train_df_subset <- train_df[sample(1:nrow(train_df), 10000), ]
aet_vs_dnbr <- ggplot(train_df_subset, aes(x = dnbr, y = aet, color = prefire_nbr)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "aET vs dNBR sample with NBR colored",
       x = "dNBR \n(unitless)",
       y = "aET (mm/mo)",
       color = "NBR\n(unitless)") +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'white', colour = 'white'))
# # Check correlation of dNBR and NBR
# cor(model_df$dnbr, model_df$prefire_nbr, use = "complete.obs")

# Save it
ggsave(here("figures", "creek_fire", "aet_vs_dnbr_nbr_colored.png"), aet_vs_dnbr, width = 8,
       height = 8, units = "in")



# Now make plots for test cases

tamarack_response_rast <- terra::rast(here("results", "creek_fire", "tamarack_response_rast.tif"))
tamarack_predicted_values_gam_rast <- terra::rast(here("results", "creek_fire", "tamarack_gam_interaction_predictions.tif"))
tamarack_predicted_values_lm_rast <- terra::rast(here("results", "creek_fire", "tamarack_lm_interaction_predictions.tif"))

tamarack_observed_limits <- c(terra::global(tamarack_response_rast, "min", na.rm=TRUE)$min, terra::global(tamarack_response_rast, "max", na.rm=TRUE)$max)
tamarack_modeled_limits_gam <- c(terra::global(tamarack_predicted_values_gam_rast, "min", na.rm=TRUE)$min, terra::global(tamarack_predicted_values_gam_rast, "max", na.rm=TRUE)$max)
tamarack_modeled_limits_lm <- c(terra::global(tamarack_predicted_values_lm_rast, "min", na.rm=TRUE)$min, terra::global(tamarack_predicted_values_lm_rast, "max", na.rm=TRUE)$max)

# Plot using ggplot2 and tidyterra
tamarack_response_plot <- ggplot() +
  annotation_map_tile(
    type = "osm",   
    zoom = 10,             
    cachedir = "cache"     # Directory to cache map tiles
  ) +
  geom_spatraster(data = tamarack_response_rast) +
  scale_fill_viridis_c(option="viridis", limits = tamarack_observed_limits, na.value=NA, oob=squish) +
  labs(title = "\n\n\nObserved aET,\nTamarack Fire",
       x = "Longitude",
       y = "Latitude",
       fill = "aET (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'transparent', colour = NA)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  guides(fill = guide_legend(title = "aET (mm/mo)"))

tamarack_predicted_values_lm_interaction_plot <- ggplot() +
  annotation_map_tile(
    type = "osm",   
    zoom = 10,           
    cachedir = "cache"     
  ) +
  + geom_spatraster(data = tamarack_predicted_values_lm_rast) +
  scale_fill_gradientn(
    limits  = tamarack_modeled_limits_lm,
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = tamarack_modeled_limits_lm), 1),
    na.value = NA
  ) +
  labs(title = "\nPredicted aET, \nMLR \nwith interaction,\nTamarack Fire",
       x = "Longitude",
       y = "Latitude",
       fill = "aET (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'transparent', colour = NA)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  guides(fill = guide_legend(title = "aET (mm/mo)"))

tamarack_predicted_values_gam_interaction_plot <- ggplot() +
  annotation_map_tile(
    type = "osm",   
    zoom = 10,            
    cachedir = "cache"    
  ) + geom_spatraster(data = tamarack_predicted_values_gam_rast) +
  scale_fill_gradientn(
    limits  = tamarack_modeled_limits_gam,
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = tamarack_modeled_limits_gam), 1),
    na.value = NA
  ) +
  labs(title = "\nPredicted aET, \nGAM \nwith interaction,\nTamarack Fire",
       x = "Longitude",
       y = "Latitude",
       fill = "aET (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'transparent', colour = NA)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  guides(fill = guide_legend(title = "aET (mm/mo)"))

ggsave(here("figures", "creek_fire", "tamarack_response_plot.png"), response_plot, width = 8,
       height = 8, units = "in")

ggsave(here("figures", "creek_fire", "tamarack_predicted_values_lm_interaction_plot.png"), predicted_values_lm_interaction_plot, width = 8,
       height = 8, units = "in")

ggsave(here("figures", "creek_fire", "tamarack_predicted_values_gam_interaction_plot.png"), predicted_values_gam_interaction_plot, width = 8,
       height = 8, units = "in")

tamarack_response_predicted_values_lm_interaction_plots <- plot_grid(tamarack_response_plot + theme(legend.position = "none"), tamarack_predicted_values_lm_interaction_plot, ncol = 2, rel_widths = c(1, 1.3))

tamarack_response_predicted_values_bam_interaction_plots <- plot_grid(tamarack_response_plot + theme(legend.position = "none"), tamarack_predicted_values_gam_interaction_plot, ncol = 2, rel_widths = c(1, 1.3))

tamarack_response_predicted_values_lm_interaction_and_bam_interaction_plots <- plot_grid(tamarack_response_plot + theme(legend.position = "none"), tamarack_predicted_values_lm_interaction_plot + theme(legend.position = "none"), tamarack_predicted_values_gam_interaction_plot, ncol = 3, rel_widths = c(1, 1, 1.5))

ggsave(here("figures", "creek_fire", "tamarack_response_predicted_values_lm_interaction_and_gam_interaction_plots.png"), tamarack_response_predicted_values_lm_interaction_and_bam_interaction_plots, width = 15,
       height = 8, units = "in", bg = "transparent")

# 4. Plot of aET vs dNBR with prefire NBR colored using a subset of the data
train_df_subset <- train_df[sample(1:nrow(train_df), 10000), ]
aet_vs_dnbr <- ggplot(train_df_subset, aes(x = dnbr, y = aet, color = prefire_nbr)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "aET vs dNBR sample with NBR colored",
       x = "dNBR \n(unitless)",
       y = "aET (mm/mo)",
       color = "NBR\n(unitless)") +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'white', colour = 'white'))
# # Check correlation of dNBR and NBR
# cor(model_df$dnbr, model_df$prefire_nbr, use = "complete.obs")

# Save it
ggsave(here("figures", "creek_fire", "aet_vs_dnbr_nbr_colored.png"), aet_vs_dnbr, width = 8,
       height = 8, units = "in")




























# 5. Residuals plots for reduced interaction lm and reduced interaction gam
# UPDATED: Now includes analysis for validation set AND two test cases (Tamarack and KNP Complex)

# Load the pre-computed fitted values from CSV files
fitted_values_df <- read_csv(here("results", "creek_fire", "validation_fitted_values_df.csv"))
fitted_values_df_tamarack <- read_csv(here("results", "creek_fire", "tamarack_fire_fitted_values_df.csv"))
fitted_values_df_knp_complex <- read_csv(here("results", "creek_fire", "knp_complex_fire_fitted_values_df.csv"))

# Extract predictions and calculate residuals for VALIDATION SET
gam_predictions_valid <- fitted_values_df$gam_interaction
gam_residuals_valid <- fitted_values_df$response - gam_predictions_valid

lm_predictions_valid <- fitted_values_df$lm_interaction
lm_residuals_valid <- fitted_values_df$response - lm_predictions_valid

# Extract predictions and calculate residuals for TAMARACK FIRE (Test Case 1)
gam_predictions_tamarack <- fitted_values_df_tamarack$gam_interaction
gam_residuals_tamarack <- fitted_values_df_tamarack$response - gam_predictions_tamarack

lm_predictions_tamarack <- fitted_values_df_tamarack$lm_interaction
lm_residuals_tamarack <- fitted_values_df_tamarack$response - lm_predictions_tamarack

# Extract predictions and calculate residuals for KNP COMPLEX FIRE (Test Case 2)
gam_predictions_knp <- fitted_values_df_knp_complex$gam_interaction
gam_residuals_knp <- fitted_values_df_knp_complex$response - gam_predictions_knp

lm_predictions_knp <- fitted_values_df_knp_complex$lm_interaction
lm_residuals_knp <- fitted_values_df_knp_complex$response - lm_predictions_knp

# Combine predictions and residuals for all datasets
gam_residuals_valid_df <- valid_df %>%
  mutate(Fitted = gam_predictions_valid,
         Residuals = gam_residuals_valid,
         Model = "GAM with interactions",
         Dataset = "Creek Fire Validation")

lm_residuals_valid_df <- valid_df %>%
  mutate(Fitted = lm_predictions_valid,
         Residuals = lm_residuals_valid,
         Model = "MLR with interactions",
         Dataset = "Creek Fire Validation")

gam_residuals_tamarack_df <- tamarack_df %>%
  mutate(Fitted = gam_predictions_tamarack,
         Residuals = gam_residuals_tamarack,
         Model = "GAM with interactions",
         Dataset = "Tamarack Fire Test")

lm_residuals_tamarack_df <- tamarack_df %>%
  mutate(Fitted = lm_predictions_tamarack,
         Residuals = lm_residuals_tamarack,
         Model = "MLR with interactions",
         Dataset = "Tamarack Fire Test")

gam_residuals_knp_df <- knp_complex_df %>%
  mutate(Fitted = gam_predictions_knp,
         Residuals = gam_residuals_knp,
         Model = "GAM with interactions",
         Dataset = "KNP Complex Test")

lm_residuals_knp_df <- knp_complex_df %>%
  mutate(Fitted = lm_predictions_knp,
         Residuals = lm_residuals_knp,
         Model = "MLR with interactions",
         Dataset = "KNP Complex Test")

# Combine all residuals data
combined_residuals_df <- bind_rows(
  gam_residuals_valid_df, lm_residuals_valid_df,
  gam_residuals_tamarack_df, lm_residuals_tamarack_df,
  gam_residuals_knp_df, lm_residuals_knp_df
)

# Create fitted vs actual plots for all datasets
fitted_vs_actual_plot <- ggplot(combined_residuals_df, aes(x = aet, y = Fitted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_grid(Model ~ Dataset) +
  scale_color_manual(
    values = c("GAM with interactions" = viridis_colors[1], "MLR with interactions" = viridis_colors[3])
  ) +
  labs(title = "Fitted vs Actual aET Values Across Datasets",
       x = "Actual aET (mm/mo)",
       y = "Fitted aET (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(size = 12),
        strip.text = element_text(size = 10))

# Save fitted vs actual plot
ggsave(here("figures", "fitted_vs_actual_all_datasets.png"), 
       fitted_vs_actual_plot, 
       width = 15, height = 10, units = "in", bg = "white")

# Residuals vs Fitted for all datasets
residuals_fitted_plot <- ggplot(combined_residuals_df, aes(x = Fitted, y = Residuals, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(Model ~ Dataset) +
  scale_color_manual(
    values = c("GAM with interactions" = viridis_colors[1], "MLR with interactions" = viridis_colors[3])
  ) +
  labs(title = "Residuals vs Fitted Values Across Datasets",
       x = "Fitted Values (mm/mo)",
       y = "Residuals (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        strip.text = element_text(size = 10))

# Save residuals vs fitted plot
ggsave(here("figures", "residuals_vs_fitted_all_datasets.png"), 
       residuals_fitted_plot, 
       width = 15, height = 10, units = "in", bg = "white")

# Normal Q-Q Plot for all datasets
qq_plot <- ggplot(combined_residuals_df, aes(sample = Residuals, color = Model)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line() +
  facet_grid(Model ~ Dataset) +
  scale_color_manual(
    values = c("GAM with interactions" = viridis_colors[1], "MLR with interactions" = viridis_colors[3])
  ) +
  labs(title = "Normal Q-Q Plots for Residuals Across Datasets",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        strip.text = element_text(size = 10))

# Save QQ plot
ggsave(here("figures", "qq_plots_all_datasets.png"), 
       qq_plot, 
       width = 15, height = 10, units = "in", bg = "white")

# Calculate performance metrics for all datasets
performance_metrics <- combined_residuals_df %>%
  group_by(Model, Dataset) %>%
  summarise(
    RMSE = sqrt(mean(Residuals^2, na.rm = TRUE)),
    MAE = mean(abs(Residuals), na.rm = TRUE),
    R2 = cor(aet, Fitted, use = "complete.obs")^2,
    Bias = mean(Residuals, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

# Print performance metrics
print("Performance Metrics Across All Datasets:")
print(performance_metrics)

# Save performance metrics
write_csv(performance_metrics, here("results", "test_case_performance_metrics.csv"))

# Create a summary table for the paper
performance_summary <- performance_metrics %>%
  select(Model, Dataset, RMSE, MAE, R2) %>%
  pivot_wider(names_from = Model, 
              values_from = c(RMSE, MAE, R2),
              names_sep = "_")

# Save summary table
write_csv(performance_summary, here("results", "model_performance_summary_table.csv"))

# Scale-Location Plot for all datasets
scale_location_plot <- ggplot(combined_residuals_df, aes(x = Fitted, y = sqrt(abs(Residuals)), color = Model)) +
  geom_point(alpha = 0.5) +
  facet_grid(Model ~ Dataset) +
  scale_color_manual(
    values = c("GAM with interactions" = viridis_colors[1], "MLR with interactions" = viridis_colors[3])
  ) +
  labs(title = "Scale-Location Plots Across Datasets",
       x = "Fitted Values (mm/mo)",
       y = "√|Residuals|") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        strip.text = element_text(size = 10))

# Save scale-location plot
ggsave(here("figures", "scale_location_all_datasets.png"), 
       scale_location_plot, 
       width = 15, height = 10, units = "in", bg = "white")

# Combine all diagnostic plots into one comprehensive figure
library(gridExtra)

png(filename = here("results", "comprehensive_residuals_diagnostic_plots.png"), 
    width = 4000, height = 3000, res = 300)

grid.arrange(
  fitted_vs_actual_plot + theme(legend.position = "none"),
  residuals_fitted_plot + theme(legend.position = "none"), 
  qq_plot + theme(legend.position = "none"),
  scale_location_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 2
)

dev.off()

print("Updated analysis complete! Generated plots and metrics for:")
print("- Creek Fire validation set")
print("- Tamarack Fire test case")  
print("- KNP Complex Fire test case")









# Ughghg
colour_breaks_lm <- c(tamarack_modeled_limits_gam[1], tamarack_observed_limits[1], 0, tamarack_observed_limits[2], tamarack_modeled_limits_gam[2])
colour_breaks_gam <- c(tamarack_modeled_limits_gam[1], tamarack_observed_limits[1], 0, tamarack_observed_limits[2], tamarack_modeled_limits_gam[2])
v <- viridis(100)
colours_gam <- c(v[1], v[20], v[50], v[80], v[100])# c("darkblue", "lightgreen", "white", "yellow", "orange")


ggplot() +
  geom_spatraster(data = tamarack_predicted_values_gam_rast) +
  scale_fill_gradientn(
    limits  = tamarack_modeled_limits_gam,
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = tamarack_modeled_limits_gam), 1),
    na.value = NA
  )

tamarack_predicted_values_gam_interaction_above <- tamarack_predicted_values_gam_rast
tamarack_predicted_values_gam_interaction_above[tamarack_predicted_values_gam_interaction_above < tamarack_observed_limits[2]] <- NA

tamarack_predicted_values_gam_interaction_below <- tamarack_predicted_values_gam_rast
tamarack_predicted_values_gam_interaction_below[tamarack_predicted_values_gam_interaction_below > tamarack_observed_limits[1]] <- NA


ggplot() +
  geom_spatraster(data = tamarack_predicted_values_gam_rast) +
  scale_fill_viridis(option="D", na.value = NA, limits=tamarack_observed_limits) +
  labs(fill = "aET within obs. range (mm/mo)") +
  new_scale_fill() +
  geom_spatraster(data = tamarack_predicted_values_gam_interaction_above) +
  scale_fill_viridis(option="A", na.value = NA) +
    labs(fill = "aET above obs. range (mm/mo)") +
  new_scale_fill() +
  geom_spatraster(data = tamarack_predicted_values_gam_interaction_below) +
  scale_fill_viridis(option="E", na.value = NA) +
  labs(title = "\n\n\nObserved aET,\nTamarack Fire",
       x = "Longitude",
       y = "Latitude",
       fill = "aET below obs. range (mm/mo)") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.background=element_rect(fill = 'transparent', colour = NA)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  guides(fill = guide_legend(title = "aET (mm/mo)"))

library(ggnewscale)


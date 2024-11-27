# title: "feature_engineering.Rmd"
# author: "Carrie Hashimoto"
# date: "2024-08-02"

# Packages
library(doParallel)
library(vroom)
library(here)
library(tidyverse)
library(data.table)

# load data crom create_table.R
df <- vroom(here("data/big_table.csv"))

# drop huc12 column -- not using it to find controls for now
df <- subset(df, select = -c(huc12))

# drop rows with NA values
df <- df[complete.cases(df), ]

# create elevation bins
# use 100-m elevation breaks, after Qin and Ma (2020)
df$elev_bin <- cut(df$elev, breaks = seq(floor(min(df$elev)), ceiling(max(df$elev)), by = 100), labels = FALSE, right = FALSE)

# bc of limited temp resolution, harder to get matches using temp, and it's collinear w elevation anyway
df$ndvi_bin <- cut(df$prefire_ndvi, breaks = seq(floor(min(df$prefire_ndvi)), ceiling(max(df$prefire_ndvi)), length.out = 30), labels = FALSE, right = FALSE)

df$northness_bin <- cut(df$northness, breaks = seq(floor(min(df$northness)), ceiling(max(df$northness)), length.out = 30), labels = FALSE, right = FALSE)

df$soil_moisture_bin <- cut(df$soil_moisture, breaks = seq(floor(min(df$soil_moisture)), ceiling(max(df$soil_moisture)), length.out = 30), labels = FALSE, right = FALSE)

df <- as.data.table(df)

# separate within burn and control points
within_burn <- df[within_burn_area == 1]
control <- df[within_burn_area == 0]

# Check the number of points in each bin
within_burn[, .N, by = .(elev_bin,ndvi_bin,northness_bin,soil_moisture_bin)]
control[, .N, by = .(elev_bin,ndvi_bin,northness_bin,soil_moisture_bin)]

# Aggregate control points by bins to compute the mean of prefire_et and postfire_et
control_aggregated <- control[, .(
  prefire_et_control = mean(prefire_et, na.rm = TRUE),
  postfire_et_control = mean(postfire_et, na.rm = TRUE)
), by = .(elev_bin, ndvi_bin, northness_bin, soil_moisture_bin)]

# Merge burned points with aggregated control points
joined <- merge(
  within_burn, 
  control_aggregated, 
  by = c("elev_bin", "ndvi_bin", "northness_bin", "soil_moisture_bin"), 
  suffixes = c("", "_control")
)

# calculate aET and rET
joined$aet <- (joined$prefire_et - joined$postfire_et) - (joined$prefire_et_control - joined$postfire_et_control)
joined$ret <- joined$aet / joined$prefire_et * 100

# select columns to keep
columns_to_keep <- setdiff(names(joined), grep("control", names(joined), value = TRUE))

# create a cleaned data table with only the necessary columns
cleaned_df <- joined[, ..columns_to_keep]

# discard other unneeded columns
cleaned_df <- cleaned_df %>%
  select(-c("elev_bin","northness_bin","ndvi_bin","soil_moisture_bin","within_burn_area","within_buffer"))

# save intermediate results to a CSV file
fwrite(cleaned_df, here("data/model_df_with_outliers.csv"))

###############################################################################

# further processing:
# compute z-scores for each predictor
z_scores <- scale(cleaned_df)

# identify outliers with z-score > 3
outlier_indices <- which(apply(z_scores, 1, function(x) any(abs(x) > 3)))

# remove outliers
final_df <- cleaned_df[-outlier_indices, ]

fwrite(final_df, here("data/model_df_outliers_removed.csv"))

# # check point spatial distribution
# set.seed(0)  # For reproducibility
# sample_size <- 1000  # Number of random points to sample
# 
# subset_df <- final_df %>%
#   sample_n(sample_size)
# 
# # plot the selected points using ggplot2 with rET shown
# ggplot(subset_df, aes(x = lon, y = lat, color = ret)) +
#   geom_point() +
#   scale_color_viridis_c() +
#   theme_minimal() +
#   labs(title = "Spatial Distribution of Selected Points", color = "Relative Evapotranspiration (%)")
# 
# # plot histogram of rET
# ggplot(final_df, aes(x = ret)) +
#   geom_histogram(binwidth = 10, fill = "blue", color = "black") +
#   theme_minimal() +
#   ggtitle("Distribution of rET")

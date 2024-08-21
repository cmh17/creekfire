# title: "feature_engineering.Rmd"
# author: "Carrie Hashimoto"
# date: "2024-08-02"

# load packages
# some of these are extraneous
packages <- c('tidyverse','mgcv','akima','readr','data.table','vroom',
              'foreach','doParallel','readr','caret','progressr','progress',
              'ggplot2')

# Identify missing (not installed) packages
new.packages <-  packages[!(packages %in% installed.packages()[,"Package"])]

# Install new (not installed) packages
if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/') else print('All required packages are installed.')

invisible(lapply(packages, library, character.only = TRUE))

# set wd and outDir
wd <- rprojroot::find_rstudio_root_file()
outDir <- file.path(wd, "Data", fsep="/")
suppressWarnings(dir.create(outDir))

# load data crom create_table.R
df <- vroom(paste0(wd, "/Data/big_table.csv"))
df <- na.omit(df)

# create bins
df$elev_bin <- cut(df$elev, breaks = seq(floor(min(df$elev)), ceiling(max(df$elev)), by = 100), labels = FALSE, right = FALSE)

df$temp_bin <- cut(df$temp, breaks = seq(floor(min(df$elev)), ceiling(max(df$elev)), length.out = 30), labels = FALSE, right = FALSE)

df$north_bin <- cut(df$temp, breaks = seq(floor(min(df$north)), ceiling(max(df$north)), length.out = 30), labels = FALSE, right = FALSE)

df$soil_moisture_bin <- cut(df$temp, breaks = seq(floor(min(df$soil_moisture)), ceiling(max(df$soil_moisture)), length.out = 30), labels = FALSE, right = FALSE)

df <- as.data.table(df)

# separate within burn and control points
within_burn <- df[within_burn_area == 1]
control <- df[within_burn_area == 0]

# create unique identifier
within_burn[, id := .I]
control[, id := .I]

# randomly sample one control point per unique combination
control_points <- control[, .SD[sample(.N, 1)], by = .(elev_bin, temp_bin, north_bin, soil_moisture_bin)]

# perform the inner join using merge
joined <- merge(within_burn, control_points, by = c("elev_bin", "temp_bin", "north_bin", "soil_moisture_bin"), suffixes = c("", ".control"))

# calculate aET and rET
joined$aET <- (joined$prefire_et - joined$prefire_et.control) - (joined$postfire_et - joined$postfire_et.control)
joined$rET <- joined$aET / joined$prefire_et * 100

# select columns to keep
columns_to_keep <- setdiff(names(joined), grep("control", names(joined), value = TRUE))

# create a cleaned data table with only the necessary columns
cleaned_df <- joined[, ..columns_to_keep]

# save intermediate results to a CSV file
temp_file <- "model_df_with_outliers.csv"
fwrite(cleaned_df, temp_file)

###############################################################################

# further processing:
# compute z-scores for each predictor
z_scores <- scale(cleaned_df)

# identify outliers with z-score > 3
outlier_indices <- which(apply(z_scores, 1, function(x) any(abs(x) > 3)))

# remove outliers
final_df <- cleaned_df[-outlier_indices, ]

fwrite(final_df, "model_df_outliers_removed.csv")

# check point spatial distribution
set.seed(0)  # For reproducibility
sample_size <- 1000  # Number of random points to sample

subset_df <- final_df %>%
  sample_n(sample_size)

# plot the selected points using ggplot2
ggplot(subset_df, aes(x = lon, y = lat)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Random Subset of Longitude and Latitude Points",
       x = "Longitude", y = "Latitude")
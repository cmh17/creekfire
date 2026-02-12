# Analyzing results
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(vroom)
library(leaps)
library(corrplot)
library(caret)
library(igraph)
library(ggplot2)
library(gridExtra)

# Load the dataset
model_df <- vroom(here("data", "creek_fire", "creek_fire_model_df_outliers_removed.csv"))
tamarack_df <- vroom(here("data", "tamarack_fire", "tamarack_fire_model_df_outliers_removed.csv"))
knp_complex_df <- vroom(here("data", "knp_complex_fire", "knp_complex_fire_model_df_outliers_removed.csv"))

# Define variable variables
vars <- c("prefire_ndvi", "prefire_nbr", "dndvi", "dnbr", "rdnbr",
          "tmax", "precip",  "vpd", "pdsi", "elev", "aspect", "slope", "northness", "soil_moisture")

# Split data into train, validation, and valid sets
set.seed(2024)
train_index <- createDataPartition(model_df$aet, p = 0.7, list = FALSE)
train_df <- model_df[train_index, ]
valid_df <- model_df[-train_index, ]

# Preprocess the data
preproc_values <- preProcess(train_df[, vars], method = c("center", "scale"))
train_df_scaled <- train_df
train_df_scaled[, vars] <- predict(preproc_values, train_df[, vars])
valid_df_scaled <- valid_df
valid_df_scaled[, vars] <- predict(preproc_values, valid_df[, vars])
tamarack_df_scaled <- tamarack_df
tamarack_df_scaled[, vars] <- predict(preproc_values, tamarack_df[, vars])
knp_complex_df_scaled <- knp_complex_df
knp_complex_df_scaled[, vars] <- predict(preproc_values, knp_complex_df[, vars])

########## Look at differences in performance between linear models ############

library(gam.hp)
library(mgcv)

load(here("models", "creek_fire", "creek_fire_aet_gam_base_k10.RData"))
load(here("models", "creek_fire", "creek_fire_aet_gam_interaction_k10.RData"))  
load(here("models", "creek_fire", "creek_fire_aet_gam_interaction_reduced_k10.RData"))

print("#################################### base ####################################")
summary(creek_fire_aet_gam_base_k10)
gam.hp(creek_fire_aet_gam_base_k10)
gam.hp(creek_fire_aet_gam_base_k10,type="adjR2")
png(filename = here("figures", "creek_fire", "creek_fire_aet_gam_base_k10_gam_hp.png"), width = 2000, height = 1600, res = 300)
plot(gam.hp(creek_fire_aet_gam_base_k10,type="adjR2"))
def.off()

print("#################################### interaction ####################################")
summary(creek_fire_aet_gam_interaction_k10)
gam.hp(creek_fire_aet_gam_interaction_k10)
gam.hp(creek_fire_aet_gam_interaction_k10,type="adjR2")
png(filename = here("figures", "creek_fire", "creek_fire_aet_gam_interaction_k10_gam_hp.png"), width = 2000, height = 1600, res = 300)
plot(gam.hp(creek_fire_aet_gam_interaction_k10,type="adjR2"))
def.off()

print("#################################### interaction ####################################")
summary(creek_fire_aet_gam_interaction_reduced_k10)
gam.hp(creek_fire_aet_gam_interaction_reduced_k10)
gam.hp(creek_fire_aet_gam_interaction_reduced_k10,type="adjR2")
png(filename = here("figures", "creek_fire", "creek_fire_aet_gam_interaction_reduced_k10_gam_hp.png"), width = 2000, height = 1600, res = 300)
plot(gam.hp(creek_fire_aet_gam_interaction_reduced_k10,type="adjR2"))
def.off()


# script to combine carbon and nitrogen data with "all data" ####

## packages #########
library(dplyr)

## Import data ###############
cn_data <- read.csv("Git/variability_ms_thesis/Data/carbon_nitrogen/carbon_nitrogen.csv")
all_data <- read.csv("Git/variability_ms_thesis/Data/all_data.csv")

## Calculate Nitrogen (g/g) ##########
  # Total N (ug)/ Sample weight (mg) * 1000
cn_data <- transform(cn_data, nitrogen_g.g = total_n / (weight_mg * 1000))
cn_data

## join data frames together by unique_id (all_data) and plant_id (cn_data) ####
all_data2 <- left_join(all_data, cn_data, join_by(unique_id == plant_id))

write.csv(all_data2, "Git/variability_ms_thesis/Data/all_data2.csv")

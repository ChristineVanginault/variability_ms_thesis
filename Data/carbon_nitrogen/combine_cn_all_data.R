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

## Calculate Nitrogen (area) ####
  # nitrogen_g.g * (1/SLA) = gN/m2
sla <- data.frame(subset(all_data, select = c(unique_id, SLA_focal)))
sla

cn_data <- left_join(cn_data, sla, join_by(plant_id == unique_id))
cn_data

cn_data <- transform(cn_data, nitrogen_area = nitrogen_g.g * (1/SLA_focal))
cn_data

## join data frames together by unique_id (all_data) and plant_id (cn_data) ####
all_data2 <- left_join(all_data, cn_data, join_by(unique_id == plant_id))

write.csv(all_data2, "Git/variability_ms_thesis/Data/all_data2.csv")


# check ci/ca values are around 0.7 ######
  # using air in growth chambers around -0.0125 instead of -0.008
cn_data$bigd13c <- ((-0.0125-(cn_data$c13/1000))/(1+(cn_data$c13/1000)))*1000
cn_data$cica = (cn_data$bigd13c - 4.4)/(27-4.4)
cn_data
mean <- mean(cn_data$cica)
mean
# [1] 0.7137657
stde <- sd(cn_data$cica)/sqrt(length((cn_data$cica)))
stde
# [1] 0.006217974
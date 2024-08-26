## script to calculate SLA with weight (g) and area (cm^2)

# Add packages
library(dplyr)

# import data
harvest_data <- read.csv("Git/variability_ms_thesis/Data/structural/harvest_datasheet.csv")

# calculate SLA for focal weight and area
harvest_data <- transform(harvest_data, SLA_focal = area_focal/focal_dry_weight)
harvest_data

# export new dataframe
write.csv(harvest_data, file = "harvest_datasheet.csv")

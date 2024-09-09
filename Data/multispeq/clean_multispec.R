# script to clean multispec data
library(dplyr)
## import data
multi_data <- read.csv("Git/variability_ms_thesis/Data/multispeq/multispeq­_data.csv")
colnames(multi_data)
all_harvest_data <- read.csv("Git/variability_ms_thesis/Data/structural_curve_chlor_data.csv")


## extract dark and light rows
multi_data_dark <- subset(multi_data, Light.or.Dark.Fluorescence. == "Dark")
multi_data_light <- subset(multi_data, Light.or.Dark.Fluorescence. == "Light")

## select relevant columns and rename
multi_data_dark <- multi_data_dark[c(4:23, 29:30)]
colnames(multi_data_dark) <- paste(colnames(multi_data_dark), "_dark", sep = '')
names(multi_data_dark)

multi_data_light <- multi_data_light[c(4:23, 29:30)]
colnames(multi_data_light) <- paste(colnames(multi_data_light), "_light", sep = '')
names(multi_data_light)

## join dataframes together 
all_data <- left_join(all_harvest_data, multi_data_dark, join_by(unique_id == Unique.ID..._dark))
all_data <- left_join(all_data, multi_data_light, join_by(unique_id == Unique.ID..._light))

write.csv(all_data, "Git/variability_ms_thesis/Data/all_data.csv")

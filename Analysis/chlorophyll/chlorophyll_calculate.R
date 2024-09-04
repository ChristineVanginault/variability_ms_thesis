## script to calculate chlorophyll

library(tidyr)
library(dplyr)

## load in data
plate_1 <- read.csv("Git/variability_ms_thesis/Data/chlorophyll/plate1_chlorophyll_cleaned.csv")
plate_2 <- read.csv("Git/variability_ms_thesis/Data/chlorophyll/plate2_chlorophyll_cleaned.csv")
harvest_data <- read.csv("Git/variability_ms_thesis/Data/structural_curve_data.csv")


##combine both plate 1 and 2
both_plates <- full_join(plate_1, plate_2)


## Get average and one row per id
chlor.df <- (both_plates) %>% 
  filter(id != "blk") %>%
  dplyr::select(id, everything()) %>%
  group_by(id) %>%
  summarize(abs_649 = mean(abs_649, na.rm = TRUE),
            cv_649 = (sd(abs_649, na.rm = TRUE) / abs_649) * 100,
            abs_665 = mean(abs_665, na.rm = TRUE),
            cv_665 = (sd(abs_665, na.rm = TRUE) / abs_665) * 100) %>%
  mutate(abs_649 = ifelse(abs_649 < 0, 0, abs_649),
         abs_665 = ifelse(abs_665 < 0, 0, abs_665))


##subset disc area from harvest data
disc_area <- data.frame(subset(harvest_data, select = c(unique_id, disc_area)))
disc_area <- transform(disc_area, unique_id = as.character(unique_id))

## combine disc area with chlorophyll file
chlorophyll <- chlor.df %>% 
  full_join(disc_area, join_by(id == unique_id)) %>%
  dplyr::select(id, abs_649, abs_665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs_665) - (3.45 * abs_649),
         chlB.ugml = (21.99 * abs_649) - (5.32 * abs_665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 10, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 10, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000,
         chl.mmolm2 = chlA.mmolm2 + chlB.mmolm2,
         chlA.chlB = chlA.g / chlB.g) %>%
  dplyr::select(id, chlA.ugml:chlA.chlB)

write.csv(chlorophyll, "Git/variability_ms_thesis/Analysis/chlorophyll/chlorophyll_calculations.csv")


## combine chlorophyll calculations with structural_curve_data sheet
structural_curve_data <- read.csv("Git/variability_ms_thesis/Data/structural_curve_data.csv")
chlorophyll <- read.csv("Git/variability_ms_thesis/Analysis/chlorophyll/chlorophyll_calculations.csv")

all_data <- left_join(structural_curve_data, chlorophyll, join_by(unique_id == id))


write.csv(all_data, "Git/variability_ms_thesis/Data/structural_curve_chlor_data.csv")

## not sure how to remove X.y column before chlorophyll data is joined

## script to calculate chlorophyll

library(tidyr)
library(dplyr)

## load in data
plate_1 <- data.frame(read.csv("Git/variability_ms_thesis/Data/chlorophyll/plate1_chlorophyll_cleaned.csv"))
plate_2 <- data.frame(read.csv("Git/variability_ms_thesis/Data/chlorophyll/plate2_chlorophyll_cleaned.csv"))


##combine both plate 1 and 2
both_plates <- rbind(plate_1, plate_2)

## Get average and one row per id
chlor.df <- (both_plates) %>%
  filter(id != "blank") %>%
  dplyr::select(rep = id, everything()) %>%
  group_by(rep) %>%
  summarize(abs.649 = mean(abs_649, na.rm = TRUE),
            cv.649 = (sd(abs_649, na.rm = TRUE) / abs.649) * 100,
            abs.665 = mean(abs_665, na.rm = TRUE),
            cv.665 = (sd(abs_665, na.rm = TRUE) / abs.665) * 100) %>%
  mutate(abs.649 = ifelse(abs.649 < 0, 0, abs.649),
         abs.665 = ifelse(abs.665 < 0, 0, abs.665))

## isolate disc area
harvest_data <- 

## calculate chlorophyll using Wellburn (1994) equations
data$avg_649 <- (data$A6491 + data$A6491.1 + data$A6491.2)/3
data$avg_665 <- (data$A6651 + data$A6651.1 + data$A6651.2)/3
data$chla_ug.ml <- (12.19 * data$avg_665) - (3.45 * data$avg_649) # ug mL-1, from Wellburn (1994)
data$chlb_ug.ml <- (21.99 * data$avg_665) - (5.32 * data$avg_649) # ug mL-1, from Wellburn (1994)
data$chla_g.ml <- data$chla_ug.ml / 1000000 # g mL-1
data$chlb_g.ml <- data$chlb_ug.ml / 1000000 # g mL-1
data$chla_g <- data$chla_g.ml * 10 # 10 mL of DMSO
data$chlb_g <- data$chlb_g.ml * 10 # 10 mL of DMSO
data$chla_g.m2 <- data$chla_g / (data$chl_area / 10000) # convert area to m2
data$chlb_g.m2 <- data$chlb_g / (data$chl_area / 10000) # convert area to m2
data$chla_mol.m2 <- data$chla_g.m2 / 893.51 # 893.51 g mol-1 chlorophyll a
data$chlb_mol.m2 <- data$chlb_g.m2 / 907.47 # 907.47 g mol-1 chlorophyll b
data$chla_mmol.m2 <- data$chla_mol.m2 * 1000
data$chlb_mmol.m2 <- data$chla_mol.m2 * 1000
data$chl_marea <- data$chl_wt / (data$chl_area / 10000) #g m-2
data$chla_mmol.g <- data$chla_mmol.m2 * (1 / data$chl_marea)
data$chlb_mmol.g <- data$chlb_mmol.m2 * (1 / data$chl_marea)
data$chl_mmol.g <- data$chla_mmol.g + data$chlb_mmol.g
hist(chl_mmol.g)
data$chl_mmol.m2 <- data$chla_mmol.m2 + data$chlb_mmol.m2
data$chl_mmol.m2_narea <- data$chl_mmol.m2 / data$narea

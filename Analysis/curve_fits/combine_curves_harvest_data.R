## Script to combine Harvest Datasheet with curve_fits data

# import data
harvest_data <- read.csv("Git/variability_ms_thesis/Data/structural/harvest_datasheet.csv")
curve_fits <- read.csv("Git/variability_ms_thesis/Analysis/curve_fits/curve_fits.csv")

# extract rows from curve fits as columns in harvest datasheet
curve_fit_20 <- subset(curve_fits, set_leafT == 20)
curve_fit_25 <- subset(curve_fits, set_leafT == 25)
curve_fit_31 <- subset(curve_fits, set_leafT == 31)
colnames(curve_fit_20)[5:24] <- paste(colnames(curve_fit_20)[5:24], "_20", sep = '')
colnames(curve_fit_25)[5:24] <- paste(colnames(curve_fit_25)[5:24], "_25", sep = '')
colnames(curve_fit_31)[5:24] <- paste(colnames(curve_fit_31)[5:24], "_31", sep = '')

# join all data frames together by unique_id (harvest data) and id (licor data)
all_data <- left_join(harvest_data, curve_fit_20[,-c(1, 4)], join_by(unique_id == id))
all_data <- left_join(all_data, curve_fit_25[,-c(1, 2, 4)], join_by(unique_id == id))
all_data <- left_join(all_data, curve_fit_31[,-c(1, 2, 4)], join_by(unique_id == id))

write.csv(all_data, "Git/variability_ms_thesis/Data/structural_curve_data.csv")

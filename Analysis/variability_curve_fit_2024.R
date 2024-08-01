# chamber experiment 2024 analysis
## r script to analyze the growth chamber experiment from 2024

## load packages
library(plantecophys)
library(R.utils)

## load functions
sourceDirectory('functions')

## load data
aci_data <- read.csv("C:/Users/tineb/OneDrive/Desktop/Experiment Data/LICOR data/merged_licor_curve_data.csv")
head(aci_data)
colnames(aci_data)

## start visualization, curve fitting, and data frame creation

# Ignore this below; documenting thought process
# ids <- levels(as.factor(aci_data$id))
# Tleaf <- levels(as.factor(aci_data$set_leafT))
# ids_Tleaf <- paste(ids, Tleaf, sep = "-")
# ids_Tleaf
# [1] "6-20"  "12-25" "6-31" 
# not sure why it produces these values

# id_Tleaf <- levels(as.factor(paste(aci_data$id, aci_data$set_leafT, sep = "," )))
# id_Tleaf
#[1] "12,20" "12,25" "12,31" "6,20"  "6,25"  "6,31" 
# when using the ids above, I wouldnt any data from aci_data...

curve_fits <- c()

################################################################################
### plant id1 tleaf20
aci_data_id1_20 = subset(aci_data, id == 1 & set_leafT == 20)
aci_data_id1_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_20)
#### fit aci curve
fit_aci_id1_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 275,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_20)
summary(fit_aci_id1_20)
coef_id1_20 <- coef(fit_aci_id1_20)
#### plot
plot(fit_aci_id1_20)
#### add to dataframe
aci_data_id1_20_data <- cbind(aci_data_id1_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id1_20[,30]),
                               mean(aci_data_id1_20[,72]),
                               fit_aci_id1_20[[2]][1,1],
                               fit_aci_id1_20[[2]][1,2],
                               fit_aci_id1_20[[2]][2,1],
                               fit_aci_id1_20[[2]][2,2],
                               fit_aci_id1_20[[2]][3,1],
                               fit_aci_id1_20[[2]][3,2],
                               # fit_aci_id1_20[[2]][4,1],
                               # fit_aci_id1_20[[2]][4,2],
                               fit_aci_id1_20$RMSE,
                               fit_aci_id1_20$Ci_transition,
                               fit_aci_id1_20$citransition,
                               fit_aci_id1_20$Km,
                               fit_aci_id1_20$GammaStar,
                               fit_aci_id1_20$fitmethod,
                               fit_aci_id1_20$Tcorrect,
                               fit_aci_id1_20$fitTPU)
colnames(aci_data_id1_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_20_data)

################################################################################
### plant id1 tleaf25
aci_data_id1_25 = subset(aci_data, id == 1 & set_leafT == 25)
aci_data_id1_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_25)
#### fit aci curve
fit_aci_id1_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_25)
summary(fit_aci_id1_25)
coef_id1_25 <- coef(fit_aci_id1_25)
#### plot
plot(fit_aci_id1_25)
#### add to dataframe
aci_data_id1_25_data <- cbind(aci_data_id1_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id1_25[,30]),
                               mean(aci_data_id1_25[,72]),
                               fit_aci_id1_25[[2]][1,1],
                               fit_aci_id1_25[[2]][1,2],
                               fit_aci_id1_25[[2]][2,1],
                               fit_aci_id1_25[[2]][2,2],
                               fit_aci_id1_25[[2]][3,1],
                               fit_aci_id1_25[[2]][3,2],
                               # fit_aci_id1_25[[2]][4,1],
                               # fit_aci_id1_25[[2]][4,2],
                               fit_aci_id1_25$RMSE,
                               fit_aci_id1_25$Ci_transition,
                               fit_aci_id1_25$citransition,
                               fit_aci_id1_25$Km,
                               fit_aci_id1_25$GammaStar,
                               fit_aci_id1_25$fitmethod,
                               fit_aci_id1_25$Tcorrect,
                               fit_aci_id1_25$fitTPU)
colnames(aci_data_id1_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_25_data)

################################################################################
### plant id1 tleaf31
aci_data_id1_31 = subset(aci_data, id == 1 & set_leafT == 31)
aci_data_id1_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_31)
#### fit aci curve
fit_aci_id1_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_31)
summary(fit_aci_id1_31)
coef_id1_31 <- coef(fit_aci_id1_31)
#### plot
plot(fit_aci_id1_31)
#### add to dataframe
aci_data_id1_31_data <- cbind(aci_data_id1_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id1_31[,30]),
                               mean(aci_data_id1_31[,72]),
                               fit_aci_id1_31[[2]][1,1],
                               fit_aci_id1_31[[2]][1,2],
                               fit_aci_id1_31[[2]][2,1],
                               fit_aci_id1_31[[2]][2,2],
                               fit_aci_id1_31[[2]][3,1],
                               fit_aci_id1_31[[2]][3,2],
                               # fit_aci_id1_31[[2]][4,1],
                               # fit_aci_id1_31[[2]][4,2],
                               fit_aci_id1_31$RMSE,
                               fit_aci_id1_31$Ci_transition,
                               fit_aci_id1_31$citransition,
                               fit_aci_id1_31$Km,
                               fit_aci_id1_31$GammaStar,
                               fit_aci_id1_31$fitmethod,
                               fit_aci_id1_31$Tcorrect,
                               fit_aci_id1_31$fitTPU)
colnames(aci_data_id1_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_31_data)

################################################################################
### plant id2 tleaf20
aci_data_id2_20 = subset(aci_data, id == 2 & set_leafT == 20)
aci_data_id2_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id2_20)
#### fit aci curve
fit_aci_id2_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id2_20)
summary(fit_aci_id2_20)
coef_id2_20 <- coef(fit_aci_id2_20)
#### plot
plot(fit_aci_id2_20)
#### add to dataframe
aci_data_id2_20_data <- cbind(aci_data_id2_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id2_20[,30]),
                              mean(aci_data_id2_20[,72]),
                              fit_aci_id2_20[[2]][1,1],
                              fit_aci_id2_20[[2]][1,2],
                              fit_aci_id2_20[[2]][2,1],
                              fit_aci_id2_20[[2]][2,2],
                              fit_aci_id2_20[[2]][3,1],
                              fit_aci_id2_20[[2]][3,2],
                              # fit_aci_id2_20[[2]][4,1],
                              # fit_aci_id2_20[[2]][4,2],
                              fit_aci_id2_20$RMSE,
                              fit_aci_id2_20$Ci_transition,
                              fit_aci_id2_20$citransition,
                              fit_aci_id2_20$Km,
                              fit_aci_id2_20$GammaStar,
                              fit_aci_id2_20$fitmethod,
                              fit_aci_id2_20$Tcorrect,
                              fit_aci_id2_20$fitTPU)
colnames(aci_data_id2_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id2_20_data)

################################################################################
### plant id2 tleaf25
aci_data_id2_25 = subset(aci_data, id == 2 & set_leafT == 25)
aci_data_id2_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id2_25)
#### fit aci curve
fit_aci_id2_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id2_25)
summary(fit_aci_id2_25)
coef_id2_25 <- coef(fit_aci_id2_25)
#### plot
plot(fit_aci_id2_25)
#### add to dataframe
aci_data_id2_25_data <- cbind(aci_data_id2_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id2_25[,30]),
                              mean(aci_data_id2_25[,72]),
                              fit_aci_id2_25[[2]][1,1],
                              fit_aci_id2_25[[2]][1,2],
                              fit_aci_id2_25[[2]][2,1],
                              fit_aci_id2_25[[2]][2,2],
                              fit_aci_id2_25[[2]][3,1],
                              fit_aci_id2_25[[2]][3,2],
                              # fit_aci_id2_25[[2]][4,1],
                              # fit_aci_id2_25[[2]][4,2],
                              fit_aci_id2_25$RMSE,
                              fit_aci_id2_25$Ci_transition,
                              fit_aci_id2_25$citransition,
                              fit_aci_id2_25$Km,
                              fit_aci_id2_25$GammaStar,
                              fit_aci_id2_25$fitmethod,
                              fit_aci_id2_25$Tcorrect,
                              fit_aci_id2_25$fitTPU)
colnames(aci_data_id2_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id2_25_data)

################################################################################
### plant id2 tleaf31
aci_data_id2_31 = subset(aci_data, id == 2 & set_leafT == 31)
aci_data_id2_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id2_31)
#### fit aci curve
fit_aci_id2_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id2_31)
summary(fit_aci_id2_31)
coef_id2_31 <- coef(fit_aci_id2_31)
#### plot
plot(fit_aci_id2_31)
#### add to dataframe
aci_data_id2_31_data <- cbind(aci_data_id2_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id2_31[,30]),
                              mean(aci_data_id2_31[,72]),
                              fit_aci_id2_31[[2]][1,1],
                              fit_aci_id2_31[[2]][1,2],
                              fit_aci_id2_31[[2]][2,1],
                              fit_aci_id2_31[[2]][2,2],
                              fit_aci_id2_31[[2]][3,1],
                              fit_aci_id2_31[[2]][3,2],
                              # fit_aci_id2_31[[2]][4,1],
                              # fit_aci_id2_31[[2]][4,2],
                              fit_aci_id2_31$RMSE,
                              fit_aci_id2_31$Ci_transition,
                              fit_aci_id2_31$citransition,
                              fit_aci_id2_31$Km,
                              fit_aci_id2_31$GammaStar,
                              fit_aci_id2_31$fitmethod,
                              fit_aci_id2_31$Tcorrect,
                              fit_aci_id2_31$fitTPU)
colnames(aci_data_id2_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id2_31_data)

################################################################################
### plant id3 tleaf20
aci_data_id3_20 = subset(aci_data, id == 3 & set_leafT == 20)
aci_data_id3_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id3_20)
#### fit aci curve
fit_aci_id3_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id3_20)
summary(fit_aci_id3_20)
coef_id3_20 <- coef(fit_aci_id3_20)
#### plot
plot(fit_aci_id3_20)
#### add to dataframe
aci_data_id3_20_data <- cbind(aci_data_id3_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id3_20[,30]),
                              mean(aci_data_id3_20[,72]),
                              fit_aci_id3_20[[2]][1,1],
                              fit_aci_id3_20[[2]][1,2],
                              fit_aci_id3_20[[2]][2,1],
                              fit_aci_id3_20[[2]][2,2],
                              fit_aci_id3_20[[2]][3,1],
                              fit_aci_id3_20[[2]][3,2],
                              # fit_aci_id3_20[[2]][4,1],
                              # fit_aci_id3_20[[2]][4,2],
                              fit_aci_id3_20$RMSE,
                              fit_aci_id3_20$Ci_transition,
                              fit_aci_id3_20$citransition,
                              fit_aci_id3_20$Km,
                              fit_aci_id3_20$GammaStar,
                              fit_aci_id3_20$fitmethod,
                              fit_aci_id3_20$Tcorrect,
                              fit_aci_id3_20$fitTPU)
colnames(aci_data_id3_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id3_20_data)

################################################################################
### plant id3 tleaf25
aci_data_id3_25 = subset(aci_data, id == 3 & set_leafT == 25)
aci_data_id3_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id3_25)
#### fit aci curve
fit_aci_id3_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id3_25)
summary(fit_aci_id3_25)
coef_id3_25 <- coef(fit_aci_id3_25)
#### plot
plot(fit_aci_id3_25)
#### add to dataframe
aci_data_id3_25_data <- cbind(aci_data_id3_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id3_25[,30]),
                              mean(aci_data_id3_25[,72]),
                              fit_aci_id3_25[[2]][1,1],
                              fit_aci_id3_25[[2]][1,2],
                              fit_aci_id3_25[[2]][2,1],
                              fit_aci_id3_25[[2]][2,2],
                              fit_aci_id3_25[[2]][3,1],
                              fit_aci_id3_25[[2]][3,2],
                              # fit_aci_id3_25[[2]][4,1],
                              # fit_aci_id3_25[[2]][4,2],
                              fit_aci_id3_25$RMSE,
                              fit_aci_id3_25$Ci_transition,
                              fit_aci_id3_25$citransition,
                              fit_aci_id3_25$Km,
                              fit_aci_id3_25$GammaStar,
                              fit_aci_id3_25$fitmethod,
                              fit_aci_id3_25$Tcorrect,
                              fit_aci_id3_25$fitTPU)
colnames(aci_data_id3_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id3_25_data)

################################################################################
### plant id3 tleaf31
aci_data_id3_31 = subset(aci_data, id == 3 & set_leafT == 31)
aci_data_id3_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id3_31)
#### fit aci curve
fit_aci_id3_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id3_31)
summary(fit_aci_id3_31)
coef_id3_31 <- coef(fit_aci_id3_31)
#### plot
plot(fit_aci_id3_31)
#### add to dataframe
aci_data_id3_31_data <- cbind(aci_data_id3_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id3_31[,30]),
                              mean(aci_data_id3_31[,72]),
                              fit_aci_id3_31[[2]][1,1],
                              fit_aci_id3_31[[2]][1,2],
                              fit_aci_id3_31[[2]][2,1],
                              fit_aci_id3_31[[2]][2,2],
                              fit_aci_id3_31[[2]][3,1],
                              fit_aci_id3_31[[2]][3,2],
                              # fit_aci_id3_31[[2]][4,1],
                              # fit_aci_id3_31[[2]][4,2],
                              fit_aci_id3_31$RMSE,
                              fit_aci_id3_31$Ci_transition,
                              fit_aci_id3_31$citransition,
                              fit_aci_id3_31$Km,
                              fit_aci_id3_31$GammaStar,
                              fit_aci_id3_31$fitmethod,
                              fit_aci_id3_31$Tcorrect,
                              fit_aci_id3_31$fitTPU)
colnames(aci_data_id3_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id3_31_data)

################################################################################
### plant id4 tleaf20
aci_data_id4_20 = subset(aci_data, id == 4 & set_leafT == 20)
aci_data_id4_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id4_20)
#### fit aci curve
fit_aci_id4_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id4_20)
summary(fit_aci_id4_20)
coef_id4_20 <- coef(fit_aci_id4_20)
#### plot
plot(fit_aci_id4_20)
#### add to dataframe
aci_data_id4_20_data <- cbind(aci_data_id4_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id4_20[,30]),
                              mean(aci_data_id4_20[,72]),
                              fit_aci_id4_20[[2]][1,1],
                              fit_aci_id4_20[[2]][1,2],
                              fit_aci_id4_20[[2]][2,1],
                              fit_aci_id4_20[[2]][2,2],
                              fit_aci_id4_20[[2]][3,1],
                              fit_aci_id4_20[[2]][3,2],
                              # fit_aci_id4_20[[2]][4,1],
                              # fit_aci_id4_20[[2]][4,2],
                              fit_aci_id4_20$RMSE,
                              fit_aci_id4_20$Ci_transition,
                              fit_aci_id4_20$citransition,
                              fit_aci_id4_20$Km,
                              fit_aci_id4_20$GammaStar,
                              fit_aci_id4_20$fitmethod,
                              fit_aci_id4_20$Tcorrect,
                              fit_aci_id4_20$fitTPU)
colnames(aci_data_id4_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id4_20_data)

################################################################################
### plant id4 tleaf25
aci_data_id4_25 = subset(aci_data, id == 4 & set_leafT == 25)
aci_data_id4_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id4_25)
aci_data_id4_25 = subset(aci_data, id == 4 & set_leafT == 25 & Ci < 1000)
plot(Adyn ~ Ci, data = aci_data_id4_25)
#### fit aci curve
fit_aci_id4_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id4_25)
summary(fit_aci_id4_25)
coef_id4_25 <- coef(fit_aci_id4_25)
#### plot
plot(fit_aci_id4_25)
#### add to dataframe
aci_data_id4_25_data <- cbind(aci_data_id4_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id4_25[,30]),
                              mean(aci_data_id4_25[,72]),
                              fit_aci_id4_25[[2]][1,1],
                              fit_aci_id4_25[[2]][1,2],
                              fit_aci_id4_25[[2]][2,1],
                              fit_aci_id4_25[[2]][2,2],
                              fit_aci_id4_25[[2]][3,1],
                              fit_aci_id4_25[[2]][3,2],
                              # fit_aci_id4_25[[2]][4,1],
                              # fit_aci_id4_25[[2]][4,2],
                              fit_aci_id4_25$RMSE,
                              fit_aci_id4_25$Ci_transition,
                              fit_aci_id4_25$citransition,
                              fit_aci_id4_25$Km,
                              fit_aci_id4_25$GammaStar,
                              fit_aci_id4_25$fitmethod,
                              fit_aci_id4_25$Tcorrect,
                              fit_aci_id4_25$fitTPU)
colnames(aci_data_id4_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id4_25_data)

################################################################################
### plant id4 tleaf31
aci_data_id4_31 = subset(aci_data, id == 4 & set_leafT == 31)
aci_data_id4_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id4_31 = subset(aci_data, id == 4 & set_leafT == 31 & Ci < 1000)
plot(Adyn ~ Ci, data = aci_data_id4_31)
#### fit aci curve
fit_aci_id4_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id4_31)
summary(fit_aci_id4_31)
coef_id4_31 <- coef(fit_aci_id4_31)
#### plot
plot(fit_aci_id4_31)
#### add to dataframe
aci_data_id4_31_data <- cbind(aci_data_id4_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id4_31[,30]),
                              mean(aci_data_id4_31[,72]),
                              fit_aci_id4_31[[2]][1,1],
                              fit_aci_id4_31[[2]][1,2],
                              fit_aci_id4_31[[2]][2,1],
                              fit_aci_id4_31[[2]][2,2],
                              fit_aci_id4_31[[2]][3,1],
                              fit_aci_id4_31[[2]][3,2],
                              # fit_aci_id4_31[[2]][4,1],
                              # fit_aci_id4_31[[2]][4,2],
                              fit_aci_id4_31$RMSE,
                              fit_aci_id4_31$Ci_transition,
                              fit_aci_id4_31$citransition,
                              fit_aci_id4_31$Km,
                              fit_aci_id4_31$GammaStar,
                              fit_aci_id4_31$fitmethod,
                              fit_aci_id4_31$Tcorrect,
                              fit_aci_id4_31$fitTPU)
colnames(aci_data_id4_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id4_31_data)

################################################################################
### plant id5 tleaf20
aci_data_id5_20 = subset(aci_data, id == 5 & set_leafT == 20)
aci_data_id5_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id5_20)
#### fit aci curve
fit_aci_id5_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id5_20)
summary(fit_aci_id5_20)
coef_id5_20 <- coef(fit_aci_id5_20)
#### plot
plot(fit_aci_id5_20)
#### add to dataframe
aci_data_id5_20_data <- cbind(aci_data_id5_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id5_20[,30]),
                              mean(aci_data_id5_20[,72]),
                              fit_aci_id5_20[[2]][1,1],
                              fit_aci_id5_20[[2]][1,2],
                              fit_aci_id5_20[[2]][2,1],
                              fit_aci_id5_20[[2]][2,2],
                              fit_aci_id5_20[[2]][3,1],
                              fit_aci_id5_20[[2]][3,2],
                              # fit_aci_id5_20[[2]][4,1],
                              # fit_aci_id5_20[[2]][4,2],
                              fit_aci_id5_20$RMSE,
                              fit_aci_id5_20$Ci_transition,
                              fit_aci_id5_20$citransition,
                              fit_aci_id5_20$Km,
                              fit_aci_id5_20$GammaStar,
                              fit_aci_id5_20$fitmethod,
                              fit_aci_id5_20$Tcorrect,
                              fit_aci_id5_20$fitTPU)
colnames(aci_data_id5_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id5_20_data)

################################################################################
### plant id5 tleaf25
aci_data_id5_25 = subset(aci_data, id == 5 & set_leafT == 25)
aci_data_id5_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id5_25)
#### fit aci curve
fit_aci_id5_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id5_25)
summary(fit_aci_id5_25)
coef_id5_25 <- coef(fit_aci_id5_25)
#### plot
plot(fit_aci_id5_25)
#### add to dataframe
aci_data_id5_25_data <- cbind(aci_data_id5_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id5_25[,30]),
                              mean(aci_data_id5_25[,72]),
                              fit_aci_id5_25[[2]][1,1],
                              fit_aci_id5_25[[2]][1,2],
                              fit_aci_id5_25[[2]][2,1],
                              fit_aci_id5_25[[2]][2,2],
                              fit_aci_id5_25[[2]][3,1],
                              fit_aci_id5_25[[2]][3,2],
                              # fit_aci_id5_25[[2]][4,1],
                              # fit_aci_id5_25[[2]][4,2],
                              fit_aci_id5_25$RMSE,
                              fit_aci_id5_25$Ci_transition,
                              fit_aci_id5_25$citransition,
                              fit_aci_id5_25$Km,
                              fit_aci_id5_25$GammaStar,
                              fit_aci_id5_25$fitmethod,
                              fit_aci_id5_25$Tcorrect,
                              fit_aci_id5_25$fitTPU)
colnames(aci_data_id5_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id5_25_data)

################################################################################
### plant id5 tleaf31
aci_data_id5_31 = subset(aci_data, id == 5 & set_leafT == 31)
aci_data_id5_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id5_31)
#### fit aci curve
fit_aci_id5_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id5_31)
summary(fit_aci_id5_31)
coef_id5_31 <- coef(fit_aci_id5_31)
#### plot
plot(fit_aci_id5_31)
#### add to dataframe
aci_data_id5_31_data <- cbind(aci_data_id5_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id5_31[,30]),
                              mean(aci_data_id5_31[,72]),
                              fit_aci_id5_31[[2]][1,1],
                              fit_aci_id5_31[[2]][1,2],
                              fit_aci_id5_31[[2]][2,1],
                              fit_aci_id5_31[[2]][2,2],
                              fit_aci_id5_31[[2]][3,1],
                              fit_aci_id5_31[[2]][3,2],
                              # fit_aci_id5_31[[2]][4,1],
                              # fit_aci_id5_31[[2]][4,2],
                              fit_aci_id5_31$RMSE,
                              fit_aci_id5_31$Ci_transition,
                              fit_aci_id5_31$citransition,
                              fit_aci_id5_31$Km,
                              fit_aci_id5_31$GammaStar,
                              fit_aci_id5_31$fitmethod,
                              fit_aci_id5_31$Tcorrect,
                              fit_aci_id5_31$fitTPU)
colnames(aci_data_id5_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id5_31_data)

################################################################################
### plant id6 tleaf20
aci_data_id6_20 = subset(aci_data, id == 6 & set_leafT == 20)
aci_data_id6_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id6_20 = subset(aci_data, id == 6 & set_leafT == 20 & Ci < 600)
plot(Adyn ~ Ci, data = aci_data_id6_20)
#### fit aci curve
fit_aci_id6_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id6_20)
summary(fit_aci_id6_20)
coef_id6_20 <- coef(fit_aci_id6_20)
#### plot
plot(fit_aci_id6_20)
#### add to dataframe
aci_data_id6_20_data <- cbind(aci_data_id6_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id6_20[,30]),
                              mean(aci_data_id6_20[,72]),
                              fit_aci_id6_20[[2]][1,1],
                              fit_aci_id6_20[[2]][1,2],
                              fit_aci_id6_20[[2]][2,1],
                              fit_aci_id6_20[[2]][2,2],
                              fit_aci_id6_20[[2]][3,1],
                              fit_aci_id6_20[[2]][3,2],
                              # fit_aci_id6_20[[2]][4,1],
                              # fit_aci_id6_20[[2]][4,2],
                              fit_aci_id6_20$RMSE,
                              fit_aci_id6_20$Ci_transition,
                              fit_aci_id6_20$citransition,
                              fit_aci_id6_20$Km,
                              fit_aci_id6_20$GammaStar,
                              fit_aci_id6_20$fitmethod,
                              fit_aci_id6_20$Tcorrect,
                              fit_aci_id6_20$fitTPU)
colnames(aci_data_id6_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id6_20_data)

################################################################################
### plant id6 tleaf25
aci_data_id6_25 = subset(aci_data, id == 6 & set_leafT == 25)
aci_data_id6_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id6_25 = subset(aci_data, id == 6 & set_leafT == 25 & Ci < 800)
plot(Adyn ~ Ci, data = aci_data_id6_25)
#### fit aci curve
fit_aci_id6_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id6_25)
summary(fit_aci_id6_25)
coef_id6_25 <- coef(fit_aci_id6_25)
#### plot
plot(fit_aci_id6_25)
#### add to dataframe
aci_data_id6_25_data <- cbind(aci_data_id6_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id6_25[,30]),
                              mean(aci_data_id6_25[,72]),
                              fit_aci_id6_25[[2]][1,1],
                              fit_aci_id6_25[[2]][1,2],
                              fit_aci_id6_25[[2]][2,1],
                              fit_aci_id6_25[[2]][2,2],
                              fit_aci_id6_25[[2]][3,1],
                              fit_aci_id6_25[[2]][3,2],
                              # fit_aci_id6_25[[2]][4,1],
                              # fit_aci_id6_25[[2]][4,2],
                              fit_aci_id6_25$RMSE,
                              fit_aci_id6_25$Ci_transition,
                              fit_aci_id6_25$citransition,
                              fit_aci_id6_25$Km,
                              fit_aci_id6_25$GammaStar,
                              fit_aci_id6_25$fitmethod,
                              fit_aci_id6_25$Tcorrect,
                              fit_aci_id6_25$fitTPU)
colnames(aci_data_id6_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id6_25_data)

################################################################################
### plant id6 tleaf31
aci_data_id6_31 = subset(aci_data, id == 6 & set_leafT == 31)
aci_data_id6_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id6_31)
#### fit aci curve
fit_aci_id6_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id6_31)
summary(fit_aci_id6_31)
coef_id6_31 <- coef(fit_aci_id6_31)
#### plot
plot(fit_aci_id6_31)
#### add to dataframe
aci_data_id6_31_data <- cbind(aci_data_id6_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id6_31[,30]),
                              mean(aci_data_id6_31[,72]),
                              fit_aci_id6_31[[2]][1,1],
                              fit_aci_id6_31[[2]][1,2],
                              fit_aci_id6_31[[2]][2,1],
                              fit_aci_id6_31[[2]][2,2],
                              fit_aci_id6_31[[2]][3,1],
                              fit_aci_id6_31[[2]][3,2],
                              # fit_aci_id6_31[[2]][4,1],
                              # fit_aci_id6_31[[2]][4,2],
                              fit_aci_id6_31$RMSE,
                              fit_aci_id6_31$Ci_transition,
                              fit_aci_id6_31$citransition,
                              fit_aci_id6_31$Km,
                              fit_aci_id6_31$GammaStar,
                              fit_aci_id6_31$fitmethod,
                              fit_aci_id6_31$Tcorrect,
                              fit_aci_id6_31$fitTPU)
colnames(aci_data_id6_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id6_31_data)

################################################################################
### plant id7 tleaf20
aci_data_id7_20 = subset(aci_data, id == 7 & set_leafT == 20)
aci_data_id7_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id7_20)
#### fit aci curve
fit_aci_id7_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 200,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id7_20)
summary(fit_aci_id7_20)
coef_id7_20 <- coef(fit_aci_id7_20)
#### plot
plot(fit_aci_id7_20)
#### add to dataframe
aci_data_id7_20_data <- cbind(aci_data_id7_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id7_20[,30]),
                              mean(aci_data_id7_20[,72]),
                              fit_aci_id7_20[[2]][1,1],
                              fit_aci_id7_20[[2]][1,2],
                              fit_aci_id7_20[[2]][2,1],
                              fit_aci_id7_20[[2]][2,2],
                              fit_aci_id7_20[[2]][3,1],
                              fit_aci_id7_20[[2]][3,2],
                              # fit_aci_id7_20[[2]][4,1],
                              # fit_aci_id7_20[[2]][4,2],
                              fit_aci_id7_20$RMSE,
                              fit_aci_id7_20$Ci_transition,
                              fit_aci_id7_20$citransition,
                              fit_aci_id7_20$Km,
                              fit_aci_id7_20$GammaStar,
                              fit_aci_id7_20$fitmethod,
                              fit_aci_id7_20$Tcorrect,
                              fit_aci_id7_20$fitTPU)
colnames(aci_data_id7_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id7_20_data)

################################################################################
### plant id7 tleaf25
aci_data_id7_25 = subset(aci_data, id == 7 & set_leafT == 25)
aci_data_id7_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id7_25)
#### fit aci curve
fit_aci_id7_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id7_25)
summary(fit_aci_id7_25)
coef_id7_25 <- coef(fit_aci_id7_25)
#### plot
plot(fit_aci_id7_25)
#### add to dataframe
aci_data_id7_25_data <- cbind(aci_data_id7_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id7_25[,30]),
                              mean(aci_data_id7_25[,72]),
                              fit_aci_id7_25[[2]][1,1],
                              fit_aci_id7_25[[2]][1,2],
                              fit_aci_id7_25[[2]][2,1],
                              fit_aci_id7_25[[2]][2,2],
                              fit_aci_id7_25[[2]][3,1],
                              fit_aci_id7_25[[2]][3,2],
                              # fit_aci_id7_25[[2]][4,1],
                              # fit_aci_id7_25[[2]][4,2],
                              fit_aci_id7_25$RMSE,
                              fit_aci_id7_25$Ci_transition,
                              fit_aci_id7_25$citransition,
                              fit_aci_id7_25$Km,
                              fit_aci_id7_25$GammaStar,
                              fit_aci_id7_25$fitmethod,
                              fit_aci_id7_25$Tcorrect,
                              fit_aci_id7_25$fitTPU)
colnames(aci_data_id7_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id7_25_data)

################################################################################
### plant id7 tleaf31
aci_data_id7_31 = subset(aci_data, id == 7 & set_leafT == 31)
aci_data_id7_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id7_31)
#### fit aci curve
fit_aci_id7_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id7_31)
summary(fit_aci_id7_31)
coef_id7_31 <- coef(fit_aci_id7_31)
#### plot
plot(fit_aci_id7_31)
#### add to dataframe
aci_data_id7_31_data <- cbind(aci_data_id7_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id7_31[,30]),
                              mean(aci_data_id7_31[,72]),
                              fit_aci_id7_31[[2]][1,1],
                              fit_aci_id7_31[[2]][1,2],
                              fit_aci_id7_31[[2]][2,1],
                              fit_aci_id7_31[[2]][2,2],
                              fit_aci_id7_31[[2]][3,1],
                              fit_aci_id7_31[[2]][3,2],
                              # fit_aci_id7_31[[2]][4,1],
                              # fit_aci_id7_31[[2]][4,2],
                              fit_aci_id7_31$RMSE,
                              fit_aci_id7_31$Ci_transition,
                              fit_aci_id7_31$citransition,
                              fit_aci_id7_31$Km,
                              fit_aci_id7_31$GammaStar,
                              fit_aci_id7_31$fitmethod,
                              fit_aci_id7_31$Tcorrect,
                              fit_aci_id7_31$fitTPU)
colnames(aci_data_id7_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id7_31_data)

################################################################################
### plant id8 tleaf20
aci_data_id8_20 = subset(aci_data, id == 8 & set_leafT == 20)
aci_data_id8_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id8_20)
#### fit aci curve
fit_aci_id8_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id8_20)
summary(fit_aci_id8_20)
coef_id8_20 <- coef(fit_aci_id8_20)
#### plot
plot(fit_aci_id8_20)
#### add to dataframe
aci_data_id8_20_data <- cbind(aci_data_id8_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id8_20[,30]),
                              mean(aci_data_id8_20[,72]),
                              fit_aci_id8_20[[2]][1,1],
                              fit_aci_id8_20[[2]][1,2],
                              fit_aci_id8_20[[2]][2,1],
                              fit_aci_id8_20[[2]][2,2],
                              fit_aci_id8_20[[2]][3,1],
                              fit_aci_id8_20[[2]][3,2],
                              # fit_aci_id8_20[[2]][4,1],
                              # fit_aci_id8_20[[2]][4,2],
                              fit_aci_id8_20$RMSE,
                              fit_aci_id8_20$Ci_transition,
                              fit_aci_id8_20$citransition,
                              fit_aci_id8_20$Km,
                              fit_aci_id8_20$GammaStar,
                              fit_aci_id8_20$fitmethod,
                              fit_aci_id8_20$Tcorrect,
                              fit_aci_id8_20$fitTPU)
colnames(aci_data_id8_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id8_20_data)

################################################################################
### plant id8 tleaf25
aci_data_id8_25 = subset(aci_data, id == 8 & set_leafT == 25)
aci_data_id8_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id8_25)
#### fit aci curve
fit_aci_id8_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id8_25)
summary(fit_aci_id8_25)
coef_id8_25 <- coef(fit_aci_id8_25)
#### plot
plot(fit_aci_id8_25)
#### add to dataframe
aci_data_id8_25_data <- cbind(aci_data_id8_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id8_25[,30]),
                              mean(aci_data_id8_25[,72]),
                              fit_aci_id8_25[[2]][1,1],
                              fit_aci_id8_25[[2]][1,2],
                              fit_aci_id8_25[[2]][2,1],
                              fit_aci_id8_25[[2]][2,2],
                              fit_aci_id8_25[[2]][3,1],
                              fit_aci_id8_25[[2]][3,2],
                              # fit_aci_id8_25[[2]][4,1],
                              # fit_aci_id8_25[[2]][4,2],
                              fit_aci_id8_25$RMSE,
                              fit_aci_id8_25$Ci_transition,
                              fit_aci_id8_25$citransition,
                              fit_aci_id8_25$Km,
                              fit_aci_id8_25$GammaStar,
                              fit_aci_id8_25$fitmethod,
                              fit_aci_id8_25$Tcorrect,
                              fit_aci_id8_25$fitTPU)
colnames(aci_data_id8_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id8_25_data)

################################################################################
### plant id8 tleaf31
aci_data_id8_31 = subset(aci_data, id == 8 & set_leafT == 31)
aci_data_id8_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id8_31)
#### fit aci curve
fit_aci_id8_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id8_31)
summary(fit_aci_id8_31)
coef_id8_31 <- coef(fit_aci_id8_31)
#### plot
plot(fit_aci_id8_31)
#### add to dataframe
aci_data_id8_31_data <- cbind(aci_data_id8_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id8_31[,30]),
                              mean(aci_data_id8_31[,72]),
                              fit_aci_id8_31[[2]][1,1],
                              fit_aci_id8_31[[2]][1,2],
                              fit_aci_id8_31[[2]][2,1],
                              fit_aci_id8_31[[2]][2,2],
                              fit_aci_id8_31[[2]][3,1],
                              fit_aci_id8_31[[2]][3,2],
                              # fit_aci_id8_31[[2]][4,1],
                              # fit_aci_id8_31[[2]][4,2],
                              fit_aci_id8_31$RMSE,
                              fit_aci_id8_31$Ci_transition,
                              fit_aci_id8_31$citransition,
                              fit_aci_id8_31$Km,
                              fit_aci_id8_31$GammaStar,
                              fit_aci_id8_31$fitmethod,
                              fit_aci_id8_31$Tcorrect,
                              fit_aci_id8_31$fitTPU)
colnames(aci_data_id8_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id8_31_data)

################################################################################
### plant id9 tleaf20
aci_data_id9_20 = subset(aci_data, id == 9 & set_leafT == 20)
aci_data_id9_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id9_20 = subset(aci_data, id == 9 & set_leafT == 20 & Ci < 1000)
plot(Adyn ~ Ci, data = aci_data_id9_20)
#### fit aci curve
fit_aci_id9_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id9_20)
summary(fit_aci_id9_20)
coef_id9_20 <- coef(fit_aci_id9_20)
#### plot
plot(fit_aci_id9_20)
#### add to dataframe
aci_data_id9_20_data <- cbind(aci_data_id9_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id9_20[,30]),
                              mean(aci_data_id9_20[,72]),
                              fit_aci_id9_20[[2]][1,1],
                              fit_aci_id9_20[[2]][1,2],
                              fit_aci_id9_20[[2]][2,1],
                              fit_aci_id9_20[[2]][2,2],
                              fit_aci_id9_20[[2]][3,1],
                              fit_aci_id9_20[[2]][3,2],
                              # fit_aci_id9_20[[2]][4,1],
                              # fit_aci_id9_20[[2]][4,2],
                              fit_aci_id9_20$RMSE,
                              fit_aci_id9_20$Ci_transition,
                              fit_aci_id9_20$citransition,
                              fit_aci_id9_20$Km,
                              fit_aci_id9_20$GammaStar,
                              fit_aci_id9_20$fitmethod,
                              fit_aci_id9_20$Tcorrect,
                              fit_aci_id9_20$fitTPU)
colnames(aci_data_id9_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id9_20_data)

################################################################################
### plant id9 tleaf25
aci_data_id9_25 = subset(aci_data, id == 9 & set_leafT == 25)
aci_data_id9_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id9_25)
#### fit aci curve
fit_aci_id9_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id9_25)
summary(fit_aci_id9_25)
coef_id9_25 <- coef(fit_aci_id9_25)
#### plot
plot(fit_aci_id9_25)
#### add to dataframe
aci_data_id9_25_data <- cbind(aci_data_id9_25[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id9_25[,30]),
                              mean(aci_data_id9_25[,72]),
                              fit_aci_id9_25[[2]][1,1],
                              fit_aci_id9_25[[2]][1,2],
                              fit_aci_id9_25[[2]][2,1],
                              fit_aci_id9_25[[2]][2,2],
                              fit_aci_id9_25[[2]][3,1],
                              fit_aci_id9_25[[2]][3,2],
                              # fit_aci_id9_25[[2]][4,1],
                              # fit_aci_id9_25[[2]][4,2],
                              fit_aci_id9_25$RMSE,
                              fit_aci_id9_25$Ci_transition,
                              fit_aci_id9_25$citransition,
                              fit_aci_id9_25$Km,
                              fit_aci_id9_25$GammaStar,
                              fit_aci_id9_25$fitmethod,
                              fit_aci_id9_25$Tcorrect,
                              fit_aci_id9_25$fitTPU)
colnames(aci_data_id9_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id9_25_data)

################################################################################
### plant id9 tleaf31
aci_data_id9_31 = subset(aci_data, id == 9 & set_leafT == 31)
aci_data_id9_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id9_31)
#### fit aci curve
fit_aci_id9_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id9_31)
summary(fit_aci_id9_31)
coef_id9_31 <- coef(fit_aci_id9_31)
#### plot
plot(fit_aci_id9_31)
#### add to dataframe
aci_data_id9_31_data <- cbind(aci_data_id9_31[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id9_31[,30]),
                              mean(aci_data_id9_31[,72]),
                              fit_aci_id9_31[[2]][1,1],
                              fit_aci_id9_31[[2]][1,2],
                              fit_aci_id9_31[[2]][2,1],
                              fit_aci_id9_31[[2]][2,2],
                              fit_aci_id9_31[[2]][3,1],
                              fit_aci_id9_31[[2]][3,2],
                              # fit_aci_id9_31[[2]][4,1],
                              # fit_aci_id9_31[[2]][4,2],
                              fit_aci_id9_31$RMSE,
                              fit_aci_id9_31$Ci_transition,
                              fit_aci_id9_31$citransition,
                              fit_aci_id9_31$Km,
                              fit_aci_id9_31$GammaStar,
                              fit_aci_id9_31$fitmethod,
                              fit_aci_id9_31$Tcorrect,
                              fit_aci_id9_31$fitTPU)
colnames(aci_data_id9_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id9_31_data)

################################################################################
### plant id10 tleaf20
aci_data_id10_20 = subset(aci_data, id == 10 & set_leafT == 20)
aci_data_id10_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id10_20 = subset(aci_data, id == 10 & set_leafT == 20 & Ci < 800)
plot(Adyn ~ Ci, data = aci_data_id10_20)
#### fit aci curve
fit_aci_id10_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                        # citransition = 300,
                        useRd = FALSE,
                        Tcorrect = FALSE,
                        # fitTPU = FALSE,
                        fitmethod = 'bilinear',
                        data = aci_data_id10_20)
summary(fit_aci_id10_20)
coef_id10_20 <- coef(fit_aci_id10_20)
#### plot
plot(fit_aci_id10_20)
#### add to dataframe
aci_data_id10_20_data <- cbind(aci_data_id10_20[1, c(6:10, 14, 16, 19)],
                              mean(aci_data_id10_20[,30]),
                              mean(aci_data_id10_20[,72]),
                              fit_aci_id10_20[[2]][1,1],
                              fit_aci_id10_20[[2]][1,2],
                              fit_aci_id10_20[[2]][2,1],
                              fit_aci_id10_20[[2]][2,2],
                              fit_aci_id10_20[[2]][3,1],
                              fit_aci_id10_20[[2]][3,2],
                              # fit_aci_id10_20[[2]][4,1],
                              # fit_aci_id10_20[[2]][4,2],
                              fit_aci_id10_20$RMSE,
                              fit_aci_id10_20$Ci_transition,
                              fit_aci_id10_20$citransition,
                              fit_aci_id10_20$Km,
                              fit_aci_id10_20$GammaStar,
                              fit_aci_id10_20$fitmethod,
                              fit_aci_id10_20$Tcorrect,
                              fit_aci_id10_20$fitTPU)
colnames(aci_data_id10_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                    'anet_420', 'ci_420', 'gsw_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id10_20_data)

################################################################################
### plant id10 tleaf25
aci_data_id10_25 = subset(aci_data, id == 10 & set_leafT == 25)
aci_data_id10_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id10_25)
#### fit aci curve
fit_aci_id10_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id10_25)
summary(fit_aci_id10_25)
coef_id10_25 <- coef(fit_aci_id10_25)
#### plot
plot(fit_aci_id10_25)
#### add to dataframe
aci_data_id10_25_data <- cbind(aci_data_id10_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id10_25[,30]),
                               mean(aci_data_id10_25[,72]),
                               fit_aci_id10_25[[2]][1,1],
                               fit_aci_id10_25[[2]][1,2],
                               fit_aci_id10_25[[2]][2,1],
                               fit_aci_id10_25[[2]][2,2],
                               fit_aci_id10_25[[2]][3,1],
                               fit_aci_id10_25[[2]][3,2],
                               # fit_aci_id10_25[[2]][4,1],
                               # fit_aci_id10_25[[2]][4,2],
                               fit_aci_id10_25$RMSE,
                               fit_aci_id10_25$Ci_transition,
                               fit_aci_id10_25$citransition,
                               fit_aci_id10_25$Km,
                               fit_aci_id10_25$GammaStar,
                               fit_aci_id10_25$fitmethod,
                               fit_aci_id10_25$Tcorrect,
                               fit_aci_id10_25$fitTPU)
colnames(aci_data_id10_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id10_25_data)

################################################################################
### plant id10 tleaf31
aci_data_id10_31 = subset(aci_data, id == 10 & set_leafT == 31)
aci_data_id10_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id10_31)
#### fit aci curve
fit_aci_id10_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id10_31)
summary(fit_aci_id10_31)
coef_id10_31 <- coef(fit_aci_id10_31)
#### plot
plot(fit_aci_id10_31)
#### add to dataframe
aci_data_id10_31_data <- cbind(aci_data_id10_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id10_31[,30]),
                               mean(aci_data_id10_31[,72]),
                               fit_aci_id10_31[[2]][1,1],
                               fit_aci_id10_31[[2]][1,2],
                               fit_aci_id10_31[[2]][2,1],
                               fit_aci_id10_31[[2]][2,2],
                               fit_aci_id10_31[[2]][3,1],
                               fit_aci_id10_31[[2]][3,2],
                               # fit_aci_id10_31[[2]][4,1],
                               # fit_aci_id10_31[[2]][4,2],
                               fit_aci_id10_31$RMSE,
                               fit_aci_id10_31$Ci_transition,
                               fit_aci_id10_31$citransition,
                               fit_aci_id10_31$Km,
                               fit_aci_id10_31$GammaStar,
                               fit_aci_id10_31$fitmethod,
                               fit_aci_id10_31$Tcorrect,
                               fit_aci_id10_31$fitTPU)
colnames(aci_data_id10_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id10_31_data)

################################################################################
### plant id11 tleaf20
aci_data_id11_20 = subset(aci_data, id == 11 & set_leafT == 20)
aci_data_id11_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_20)
#### fit aci curve
fit_aci_id11_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id11_20)
summary(fit_aci_id11_20)
coef_id11_20 <- coef(fit_aci_id11_20)
#### plot
plot(fit_aci_id11_20)
#### add to dataframe
aci_data_id11_20_data <- cbind(aci_data_id11_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id11_20[,30]),
                               mean(aci_data_id11_20[,72]),
                               fit_aci_id11_20[[2]][1,1],
                               fit_aci_id11_20[[2]][1,2],
                               fit_aci_id11_20[[2]][2,1],
                               fit_aci_id11_20[[2]][2,2],
                               fit_aci_id11_20[[2]][3,1],
                               fit_aci_id11_20[[2]][3,2],
                               # fit_aci_id11_20[[2]][4,1],
                               # fit_aci_id11_20[[2]][4,2],
                               fit_aci_id11_20$RMSE,
                               fit_aci_id11_20$Ci_transition,
                               fit_aci_id11_20$citransition,
                               fit_aci_id11_20$Km,
                               fit_aci_id11_20$GammaStar,
                               fit_aci_id11_20$fitmethod,
                               fit_aci_id11_20$Tcorrect,
                               fit_aci_id11_20$fitTPU)
colnames(aci_data_id11_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_20_data)

################################################################################
### plant id11 tleaf25
aci_data_id11_25 = subset(aci_data, id == 11 & set_leafT == 25)
aci_data_id11_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_25)
#### fit aci curve
fit_aci_id11_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id11_25)
summary(fit_aci_id11_25)
coef_id11_25 <- coef(fit_aci_id11_25)
#### plot
plot(fit_aci_id11_25)
#### add to dataframe
aci_data_id11_25_data <- cbind(aci_data_id11_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id11_25[,30]),
                               mean(aci_data_id11_25[,72]),
                               fit_aci_id11_25[[2]][1,1],
                               fit_aci_id11_25[[2]][1,2],
                               fit_aci_id11_25[[2]][2,1],
                               fit_aci_id11_25[[2]][2,2],
                               fit_aci_id11_25[[2]][3,1],
                               fit_aci_id11_25[[2]][3,2],
                               # fit_aci_id11_25[[2]][4,1],
                               # fit_aci_id11_25[[2]][4,2],
                               fit_aci_id11_25$RMSE,
                               fit_aci_id11_25$Ci_transition,
                               fit_aci_id11_25$citransition,
                               fit_aci_id11_25$Km,
                               fit_aci_id11_25$GammaStar,
                               fit_aci_id11_25$fitmethod,
                               fit_aci_id11_25$Tcorrect,
                               fit_aci_id11_25$fitTPU)
colnames(aci_data_id11_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_25_data)

################################################################################
### plant id11 tleaf31
aci_data_id11_31 = subset(aci_data, id == 11 & set_leafT == 31)
aci_data_id11_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_31)
#### fit aci curve
fit_aci_id11_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id11_31)
summary(fit_aci_id11_31)
coef_id11_31 <- coef(fit_aci_id11_31)
#### plot
plot(fit_aci_id11_31)
#### add to dataframe
aci_data_id11_31_data <- cbind(aci_data_id11_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id11_31[,30]),
                               mean(aci_data_id11_31[,72]),
                               fit_aci_id11_31[[2]][1,1],
                               fit_aci_id11_31[[2]][1,2],
                               fit_aci_id11_31[[2]][2,1],
                               fit_aci_id11_31[[2]][2,2],
                               fit_aci_id11_31[[2]][3,1],
                               fit_aci_id11_31[[2]][3,2],
                               # fit_aci_id11_31[[2]][4,1],
                               # fit_aci_id11_31[[2]][4,2],
                               fit_aci_id11_31$RMSE,
                               fit_aci_id11_31$Ci_transition,
                               fit_aci_id11_31$citransition,
                               fit_aci_id11_31$Km,
                               fit_aci_id11_31$GammaStar,
                               fit_aci_id11_31$fitmethod,
                               fit_aci_id11_31$Tcorrect,
                               fit_aci_id11_31$fitTPU)
colnames(aci_data_id11_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_31_data)

################################################################################
### plant id12 tleaf20
aci_data_id12_20 = subset(aci_data, id == 12 & set_leafT == 20)
aci_data_id12_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id12_20)
#### fit aci curve
fit_aci_id12_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id12_20)
summary(fit_aci_id12_20)
coef_id12_20 <- coef(fit_aci_id12_20)
#### plot
plot(fit_aci_id12_20)
#### add to dataframe
aci_data_id12_20_data <- cbind(aci_data_id12_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id12_20[,30]),
                               mean(aci_data_id12_20[,72]),
                               fit_aci_id12_20[[2]][1,1],
                               fit_aci_id12_20[[2]][1,2],
                               fit_aci_id12_20[[2]][2,1],
                               fit_aci_id12_20[[2]][2,2],
                               fit_aci_id12_20[[2]][3,1],
                               fit_aci_id12_20[[2]][3,2],
                               # fit_aci_id12_20[[2]][4,1],
                               # fit_aci_id12_20[[2]][4,2],
                               fit_aci_id12_20$RMSE,
                               fit_aci_id12_20$Ci_transition,
                               fit_aci_id12_20$citransition,
                               fit_aci_id12_20$Km,
                               fit_aci_id12_20$GammaStar,
                               fit_aci_id12_20$fitmethod,
                               fit_aci_id12_20$Tcorrect,
                               fit_aci_id12_20$fitTPU)
colnames(aci_data_id12_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id12_20_data)

################################################################################
### plant id12 tleaf25
aci_data_id12_25 = subset(aci_data, id == 12 & set_leafT == 25)
aci_data_id12_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id12_25)
#### fit aci curve
fit_aci_id12_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id12_25)
summary(fit_aci_id12_25)
coef_id12_25 <- coef(fit_aci_id12_25)
#### plot
plot(fit_aci_id12_25)
#### add to dataframe
aci_data_id12_25_data <- cbind(aci_data_id12_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id12_25[,30]),
                               mean(aci_data_id12_25[,72]),
                               fit_aci_id12_25[[2]][1,1],
                               fit_aci_id12_25[[2]][1,2],
                               fit_aci_id12_25[[2]][2,1],
                               fit_aci_id12_25[[2]][2,2],
                               fit_aci_id12_25[[2]][3,1],
                               fit_aci_id12_25[[2]][3,2],
                               # fit_aci_id12_25[[2]][4,1],
                               # fit_aci_id12_25[[2]][4,2],
                               fit_aci_id12_25$RMSE,
                               fit_aci_id12_25$Ci_transition,
                               fit_aci_id12_25$citransition,
                               fit_aci_id12_25$Km,
                               fit_aci_id12_25$GammaStar,
                               fit_aci_id12_25$fitmethod,
                               fit_aci_id12_25$Tcorrect,
                               fit_aci_id12_25$fitTPU)
colnames(aci_data_id12_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id12_25_data)

################################################################################
### plant id12 tleaf31
aci_data_id12_31 = subset(aci_data, id == 12 & set_leafT == 31)
aci_data_id12_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id12_31)
#### fit aci curve
fit_aci_id12_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id12_31)
summary(fit_aci_id12_31)
coef_id12_31 <- coef(fit_aci_id12_31)
#### plot
plot(fit_aci_id12_31)
#### add to dataframe
aci_data_id12_31_data <- cbind(aci_data_id12_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id12_31[,30]),
                               mean(aci_data_id12_31[,72]),
                               fit_aci_id12_31[[2]][1,1],
                               fit_aci_id12_31[[2]][1,2],
                               fit_aci_id12_31[[2]][2,1],
                               fit_aci_id12_31[[2]][2,2],
                               fit_aci_id12_31[[2]][3,1],
                               fit_aci_id12_31[[2]][3,2],
                               # fit_aci_id12_31[[2]][4,1],
                               # fit_aci_id12_31[[2]][4,2],
                               fit_aci_id12_31$RMSE,
                               fit_aci_id12_31$Ci_transition,
                               fit_aci_id12_31$citransition,
                               fit_aci_id12_31$Km,
                               fit_aci_id12_31$GammaStar,
                               fit_aci_id12_31$fitmethod,
                               fit_aci_id12_31$Tcorrect,
                               fit_aci_id12_31$fitTPU)
colnames(aci_data_id12_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id12_31_data)

################################################################################
### plant id13 tleaf20
aci_data_id13_20 = subset(aci_data, id == 13 & set_leafT == 20)
aci_data_id13_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id13_20)
#### fit aci curve
fit_aci_id13_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id13_20)
summary(fit_aci_id13_20)
coef_id13_20 <- coef(fit_aci_id13_20)
#### plot
plot(fit_aci_id13_20)
#### add to dataframe
aci_data_id13_20_data <- cbind(aci_data_id13_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id13_20[,30]),
                               mean(aci_data_id13_20[,72]),
                               fit_aci_id13_20[[2]][1,1],
                               fit_aci_id13_20[[2]][1,2],
                               fit_aci_id13_20[[2]][2,1],
                               fit_aci_id13_20[[2]][2,2],
                               fit_aci_id13_20[[2]][3,1],
                               fit_aci_id13_20[[2]][3,2],
                               # fit_aci_id13_20[[2]][4,1],
                               # fit_aci_id13_20[[2]][4,2],
                               fit_aci_id13_20$RMSE,
                               fit_aci_id13_20$Ci_transition,
                               fit_aci_id13_20$citransition,
                               fit_aci_id13_20$Km,
                               fit_aci_id13_20$GammaStar,
                               fit_aci_id13_20$fitmethod,
                               fit_aci_id13_20$Tcorrect,
                               fit_aci_id13_20$fitTPU)
colnames(aci_data_id13_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id13_20_data)

################################################################################
### plant id13 tleaf25
aci_data_id13_25 = subset(aci_data, id == 13 & set_leafT == 25)
aci_data_id13_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id13_25)
#### fit aci curve
fit_aci_id13_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id13_25)
summary(fit_aci_id13_25)
coef_id13_25 <- coef(fit_aci_id13_25)
#### plot
plot(fit_aci_id13_25)
#### add to dataframe
aci_data_id13_25_data <- cbind(aci_data_id13_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id13_25[,30]),
                               mean(aci_data_id13_25[,72]),
                               fit_aci_id13_25[[2]][1,1],
                               fit_aci_id13_25[[2]][1,2],
                               fit_aci_id13_25[[2]][2,1],
                               fit_aci_id13_25[[2]][2,2],
                               fit_aci_id13_25[[2]][3,1],
                               fit_aci_id13_25[[2]][3,2],
                               # fit_aci_id13_25[[2]][4,1],
                               # fit_aci_id13_25[[2]][4,2],
                               fit_aci_id13_25$RMSE,
                               fit_aci_id13_25$Ci_transition,
                               fit_aci_id13_25$citransition,
                               fit_aci_id13_25$Km,
                               fit_aci_id13_25$GammaStar,
                               fit_aci_id13_25$fitmethod,
                               fit_aci_id13_25$Tcorrect,
                               fit_aci_id13_25$fitTPU)
colnames(aci_data_id13_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id13_25_data)

################################################################################
### plant id13 tleaf31
aci_data_id13_31 = subset(aci_data, id == 13 & set_leafT == 31)
aci_data_id13_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id13_31)
#### fit aci curve
fit_aci_id13_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id13_31)
summary(fit_aci_id13_31)
coef_id13_31 <- coef(fit_aci_id13_31)
#### plot
plot(fit_aci_id13_31)
#### add to dataframe
aci_data_id13_31_data <- cbind(aci_data_id13_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id13_31[,30]),
                               mean(aci_data_id13_31[,72]),
                               fit_aci_id13_31[[2]][1,1],
                               fit_aci_id13_31[[2]][1,2],
                               fit_aci_id13_31[[2]][2,1],
                               fit_aci_id13_31[[2]][2,2],
                               fit_aci_id13_31[[2]][3,1],
                               fit_aci_id13_31[[2]][3,2],
                               # fit_aci_id13_31[[2]][4,1],
                               # fit_aci_id13_31[[2]][4,2],
                               fit_aci_id13_31$RMSE,
                               fit_aci_id13_31$Ci_transition,
                               fit_aci_id13_31$citransition,
                               fit_aci_id13_31$Km,
                               fit_aci_id13_31$GammaStar,
                               fit_aci_id13_31$fitmethod,
                               fit_aci_id13_31$Tcorrect,
                               fit_aci_id13_31$fitTPU)
colnames(aci_data_id13_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id13_31_data)

################################################################################
### plant id14 tleaf20
aci_data_id14_20 = subset(aci_data, id == 14 & set_leafT == 20)
aci_data_id14_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_20)
#### fit aci curve
fit_aci_id14_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id14_20)
summary(fit_aci_id14_20)
coef_id14_20 <- coef(fit_aci_id14_20)
#### plot
plot(fit_aci_id14_20)
#### add to dataframe
aci_data_id14_20_data <- cbind(aci_data_id14_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id14_20[,30]),
                               mean(aci_data_id14_20[,72]),
                               fit_aci_id14_20[[2]][1,1],
                               fit_aci_id14_20[[2]][1,2],
                               fit_aci_id14_20[[2]][2,1],
                               fit_aci_id14_20[[2]][2,2],
                               fit_aci_id14_20[[2]][3,1],
                               fit_aci_id14_20[[2]][3,2],
                               # fit_aci_id14_20[[2]][4,1],
                               # fit_aci_id14_20[[2]][4,2],
                               fit_aci_id14_20$RMSE,
                               fit_aci_id14_20$Ci_transition,
                               fit_aci_id14_20$citransition,
                               fit_aci_id14_20$Km,
                               fit_aci_id14_20$GammaStar,
                               fit_aci_id14_20$fitmethod,
                               fit_aci_id14_20$Tcorrect,
                               fit_aci_id14_20$fitTPU)
colnames(aci_data_id14_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_20_data)

################################################################################
### plant id14 tleaf25
aci_data_id14_25 = subset(aci_data, id == 14 & set_leafT == 25)
aci_data_id14_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_25)
#### fit aci curve
fit_aci_id14_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id14_25)
summary(fit_aci_id14_25)
coef_id14_25 <- coef(fit_aci_id14_25)
#### plot
plot(fit_aci_id14_25)
#### add to dataframe
aci_data_id14_25_data <- cbind(aci_data_id14_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id14_25[,30]),
                               mean(aci_data_id14_25[,72]),
                               fit_aci_id14_25[[2]][1,1],
                               fit_aci_id14_25[[2]][1,2],
                               fit_aci_id14_25[[2]][2,1],
                               fit_aci_id14_25[[2]][2,2],
                               fit_aci_id14_25[[2]][3,1],
                               fit_aci_id14_25[[2]][3,2],
                               # fit_aci_id14_25[[2]][4,1],
                               # fit_aci_id14_25[[2]][4,2],
                               fit_aci_id14_25$RMSE,
                               fit_aci_id14_25$Ci_transition,
                               fit_aci_id14_25$citransition,
                               fit_aci_id14_25$Km,
                               fit_aci_id14_25$GammaStar,
                               fit_aci_id14_25$fitmethod,
                               fit_aci_id14_25$Tcorrect,
                               fit_aci_id14_25$fitTPU)
colnames(aci_data_id14_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_25_data)

################################################################################
### plant id14 tleaf31
aci_data_id14_31 = subset(aci_data, id == 14 & set_leafT == 31)
aci_data_id14_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_31)
#### fit aci curve
fit_aci_id14_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id14_31)
summary(fit_aci_id14_31)
coef_id14_31 <- coef(fit_aci_id14_31)
#### plot
plot(fit_aci_id14_31)
#### add to dataframe
aci_data_id14_31_data <- cbind(aci_data_id14_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id14_31[,30]),
                               mean(aci_data_id14_31[,72]),
                               fit_aci_id14_31[[2]][1,1],
                               fit_aci_id14_31[[2]][1,2],
                               fit_aci_id14_31[[2]][2,1],
                               fit_aci_id14_31[[2]][2,2],
                               fit_aci_id14_31[[2]][3,1],
                               fit_aci_id14_31[[2]][3,2],
                               # fit_aci_id14_31[[2]][4,1],
                               # fit_aci_id14_31[[2]][4,2],
                               fit_aci_id14_31$RMSE,
                               fit_aci_id14_31$Ci_transition,
                               fit_aci_id14_31$citransition,
                               fit_aci_id14_31$Km,
                               fit_aci_id14_31$GammaStar,
                               fit_aci_id14_31$fitmethod,
                               fit_aci_id14_31$Tcorrect,
                               fit_aci_id14_31$fitTPU)
colnames(aci_data_id14_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_31_data)

################################################################################
### plant id15 tleaf20
aci_data_id15_20 = subset(aci_data, id == 15 & set_leafT == 20)
aci_data_id15_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id15_20 = subset(aci_data, id == 15 & set_leafT == 20 & Ci < 1300)
plot(Adyn ~ Ci, data = aci_data_id15_20)
#### fit aci curve
fit_aci_id15_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id15_20)
summary(fit_aci_id15_20)
coef_id15_20 <- coef(fit_aci_id15_20)
#### plot
plot(fit_aci_id15_20)
#### add to dataframe
aci_data_id15_20_data <- cbind(aci_data_id15_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id15_20[,30]),
                               mean(aci_data_id15_20[,72]),
                               fit_aci_id15_20[[2]][1,1],
                               fit_aci_id15_20[[2]][1,2],
                               fit_aci_id15_20[[2]][2,1],
                               fit_aci_id15_20[[2]][2,2],
                               fit_aci_id15_20[[2]][3,1],
                               fit_aci_id15_20[[2]][3,2],
                               # fit_aci_id15_20[[2]][4,1],
                               # fit_aci_id15_20[[2]][4,2],
                               fit_aci_id15_20$RMSE,
                               fit_aci_id15_20$Ci_transition,
                               fit_aci_id15_20$citransition,
                               fit_aci_id15_20$Km,
                               fit_aci_id15_20$GammaStar,
                               fit_aci_id15_20$fitmethod,
                               fit_aci_id15_20$Tcorrect,
                               fit_aci_id15_20$fitTPU)
colnames(aci_data_id15_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id15_20_data)

################################################################################
### plant id15 tleaf25
aci_data_id15_25 = subset(aci_data, id == 15 & set_leafT == 25)
aci_data_id15_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id15_25)
#### fit aci curve
fit_aci_id15_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id15_25)
summary(fit_aci_id15_25)
coef_id15_25 <- coef(fit_aci_id15_25)
#### plot
plot(fit_aci_id15_25)
#### add to dataframe
aci_data_id15_25_data <- cbind(aci_data_id15_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id15_25[,30]),
                               mean(aci_data_id15_25[,72]),
                               fit_aci_id15_25[[2]][1,1],
                               fit_aci_id15_25[[2]][1,2],
                               fit_aci_id15_25[[2]][2,1],
                               fit_aci_id15_25[[2]][2,2],
                               fit_aci_id15_25[[2]][3,1],
                               fit_aci_id15_25[[2]][3,2],
                               # fit_aci_id15_25[[2]][4,1],
                               # fit_aci_id15_25[[2]][4,2],
                               fit_aci_id15_25$RMSE,
                               fit_aci_id15_25$Ci_transition,
                               fit_aci_id15_25$citransition,
                               fit_aci_id15_25$Km,
                               fit_aci_id15_25$GammaStar,
                               fit_aci_id15_25$fitmethod,
                               fit_aci_id15_25$Tcorrect,
                               fit_aci_id15_25$fitTPU)
colnames(aci_data_id15_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id15_25_data)

################################################################################
### plant id15 tleaf31
aci_data_id15_31 = subset(aci_data, id == 15 & set_leafT == 31)
aci_data_id15_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id15_31)
#### fit aci curve
fit_aci_id15_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id15_31)
summary(fit_aci_id15_31)
coef_id15_31 <- coef(fit_aci_id15_31)
#### plot
plot(fit_aci_id15_31)
#### add to dataframe
aci_data_id15_31_data <- cbind(aci_data_id15_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id15_31[,30]),
                               mean(aci_data_id15_31[,72]),
                               fit_aci_id15_31[[2]][1,1],
                               fit_aci_id15_31[[2]][1,2],
                               fit_aci_id15_31[[2]][2,1],
                               fit_aci_id15_31[[2]][2,2],
                               fit_aci_id15_31[[2]][3,1],
                               fit_aci_id15_31[[2]][3,2],
                               # fit_aci_id15_31[[2]][4,1],
                               # fit_aci_id15_31[[2]][4,2],
                               fit_aci_id15_31$RMSE,
                               fit_aci_id15_31$Ci_transition,
                               fit_aci_id15_31$citransition,
                               fit_aci_id15_31$Km,
                               fit_aci_id15_31$GammaStar,
                               fit_aci_id15_31$fitmethod,
                               fit_aci_id15_31$Tcorrect,
                               fit_aci_id15_31$fitTPU)
colnames(aci_data_id15_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id15_31_data)

################################################################################
### plant id16 tleaf20
aci_data_id16_20 = subset(aci_data, id == 16 & set_leafT == 20)
aci_data_id16_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
aci_data_id16_20 = subset(aci_data, id == 16 & set_leafT == 20 & Ci <800)
plot(Adyn ~ Ci, data = aci_data_id16_20)
#### fit aci curve
fit_aci_id16_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id16_20)
summary(fit_aci_id16_20)
coef_id16_20 <- coef(fit_aci_id16_20)
#### plot
plot(fit_aci_id16_20)
#### add to dataframe
aci_data_id16_20_data <- cbind(aci_data_id16_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id16_20[,30]),
                               mean(aci_data_id16_20[,72]),
                               fit_aci_id16_20[[2]][1,1],
                               fit_aci_id16_20[[2]][1,2],
                               fit_aci_id16_20[[2]][2,1],
                               fit_aci_id16_20[[2]][2,2],
                               fit_aci_id16_20[[2]][3,1],
                               fit_aci_id16_20[[2]][3,2],
                               # fit_aci_id16_20[[2]][4,1],
                               # fit_aci_id16_20[[2]][4,2],
                               fit_aci_id16_20$RMSE,
                               fit_aci_id16_20$Ci_transition,
                               fit_aci_id16_20$citransition,
                               fit_aci_id16_20$Km,
                               fit_aci_id16_20$GammaStar,
                               fit_aci_id16_20$fitmethod,
                               fit_aci_id16_20$Tcorrect,
                               fit_aci_id16_20$fitTPU)
colnames(aci_data_id16_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id16_20_data)

################################################################################
### plant id16 tleaf25
aci_data_id16_25 = subset(aci_data, id == 16 & set_leafT == 25)
aci_data_id16_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id16_25)
#### fit aci curve
fit_aci_id16_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id16_25)
summary(fit_aci_id16_25)
coef_id16_25 <- coef(fit_aci_id16_25)
#### plot
plot(fit_aci_id16_25)
#### add to dataframe
aci_data_id16_25_data <- cbind(aci_data_id16_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id16_25[,30]),
                               mean(aci_data_id16_25[,72]),
                               fit_aci_id16_25[[2]][1,1],
                               fit_aci_id16_25[[2]][1,2],
                               fit_aci_id16_25[[2]][2,1],
                               fit_aci_id16_25[[2]][2,2],
                               fit_aci_id16_25[[2]][3,1],
                               fit_aci_id16_25[[2]][3,2],
                               # fit_aci_id16_25[[2]][4,1],
                               # fit_aci_id16_25[[2]][4,2],
                               fit_aci_id16_25$RMSE,
                               fit_aci_id16_25$Ci_transition,
                               fit_aci_id16_25$citransition,
                               fit_aci_id16_25$Km,
                               fit_aci_id16_25$GammaStar,
                               fit_aci_id16_25$fitmethod,
                               fit_aci_id16_25$Tcorrect,
                               fit_aci_id16_25$fitTPU)
colnames(aci_data_id16_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id16_25_data)

################################################################################
### plant id16 tleaf31
aci_data_id16_31 = subset(aci_data, id == 16 & set_leafT == 31)
aci_data_id16_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id16_31)
#### fit aci curve
fit_aci_id16_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id16_31)
summary(fit_aci_id16_31)
coef_id16_31 <- coef(fit_aci_id16_31)
#### plot
plot(fit_aci_id16_31)
#### add to dataframe
aci_data_id16_31_data <- cbind(aci_data_id16_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id16_31[,30]),
                               mean(aci_data_id16_31[,72]),
                               fit_aci_id16_31[[2]][1,1],
                               fit_aci_id16_31[[2]][1,2],
                               fit_aci_id16_31[[2]][2,1],
                               fit_aci_id16_31[[2]][2,2],
                               fit_aci_id16_31[[2]][3,1],
                               fit_aci_id16_31[[2]][3,2],
                               # fit_aci_id16_31[[2]][4,1],
                               # fit_aci_id16_31[[2]][4,2],
                               fit_aci_id16_31$RMSE,
                               fit_aci_id16_31$Ci_transition,
                               fit_aci_id16_31$citransition,
                               fit_aci_id16_31$Km,
                               fit_aci_id16_31$GammaStar,
                               fit_aci_id16_31$fitmethod,
                               fit_aci_id16_31$Tcorrect,
                               fit_aci_id16_31$fitTPU)
colnames(aci_data_id16_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id16_31_data)

################################################################################
### plant id17 tleaf20
aci_data_id17_20 = subset(aci_data, id == 17 & set_leafT == 20)
aci_data_id17_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id17_20)
#### fit aci curve
fit_aci_id17_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id17_20)
summary(fit_aci_id17_20)
coef_id17_20 <- coef(fit_aci_id17_20)
#### plot
plot(fit_aci_id17_20)
#### add to dataframe
aci_data_id17_20_data <- cbind(aci_data_id17_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id17_20[,30]),
                               mean(aci_data_id17_20[,72]),
                               fit_aci_id17_20[[2]][1,1],
                               fit_aci_id17_20[[2]][1,2],
                               fit_aci_id17_20[[2]][2,1],
                               fit_aci_id17_20[[2]][2,2],
                               fit_aci_id17_20[[2]][3,1],
                               fit_aci_id17_20[[2]][3,2],
                               # fit_aci_id17_20[[2]][4,1],
                               # fit_aci_id17_20[[2]][4,2],
                               fit_aci_id17_20$RMSE,
                               fit_aci_id17_20$Ci_transition,
                               fit_aci_id17_20$citransition,
                               fit_aci_id17_20$Km,
                               fit_aci_id17_20$GammaStar,
                               fit_aci_id17_20$fitmethod,
                               fit_aci_id17_20$Tcorrect,
                               fit_aci_id17_20$fitTPU)
colnames(aci_data_id17_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id17_20_data)

################################################################################
### plant id17 tleaf25
aci_data_id17_25 = subset(aci_data, id == 17 & set_leafT == 25)
aci_data_id17_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id17_25)
#### fit aci curve
fit_aci_id17_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 325,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id17_25)
summary(fit_aci_id17_25)
coef_id17_25 <- coef(fit_aci_id17_25)
#### plot
plot(fit_aci_id17_25)
#### add to dataframe
aci_data_id17_25_data <- cbind(aci_data_id17_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id17_25[,30]),
                               mean(aci_data_id17_25[,72]),
                               fit_aci_id17_25[[2]][1,1],
                               fit_aci_id17_25[[2]][1,2],
                               fit_aci_id17_25[[2]][2,1],
                               fit_aci_id17_25[[2]][2,2],
                               fit_aci_id17_25[[2]][3,1],
                               fit_aci_id17_25[[2]][3,2],
                               # fit_aci_id17_25[[2]][4,1],
                               # fit_aci_id17_25[[2]][4,2],
                               fit_aci_id17_25$RMSE,
                               fit_aci_id17_25$Ci_transition,
                               fit_aci_id17_25$citransition,
                               fit_aci_id17_25$Km,
                               fit_aci_id17_25$GammaStar,
                               fit_aci_id17_25$fitmethod,
                               fit_aci_id17_25$Tcorrect,
                               fit_aci_id17_25$fitTPU)
colnames(aci_data_id17_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id17_25_data)

################################################################################
### plant id17 tleaf31
aci_data_id17_31 = subset(aci_data, id == 17 & set_leafT == 31)
aci_data_id17_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id17_31)
#### fit aci curve
fit_aci_id17_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id17_31)
summary(fit_aci_id17_31)
coef_id17_31 <- coef(fit_aci_id17_31)
#### plot
plot(fit_aci_id17_31)
#### add to dataframe
aci_data_id17_31_data <- cbind(aci_data_id17_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id17_31[,30]),
                               mean(aci_data_id17_31[,72]),
                               fit_aci_id17_31[[2]][1,1],
                               fit_aci_id17_31[[2]][1,2],
                               fit_aci_id17_31[[2]][2,1],
                               fit_aci_id17_31[[2]][2,2],
                               fit_aci_id17_31[[2]][3,1],
                               fit_aci_id17_31[[2]][3,2],
                               # fit_aci_id17_31[[2]][4,1],
                               # fit_aci_id17_31[[2]][4,2],
                               fit_aci_id17_31$RMSE,
                               fit_aci_id17_31$Ci_transition,
                               fit_aci_id17_31$citransition,
                               fit_aci_id17_31$Km,
                               fit_aci_id17_31$GammaStar,
                               fit_aci_id17_31$fitmethod,
                               fit_aci_id17_31$Tcorrect,
                               fit_aci_id17_31$fitTPU)
colnames(aci_data_id17_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id17_31_data)

################################################################################
### plant id18 tleaf20
aci_data_id18_20 = subset(aci_data, id == 18 & set_leafT == 20)
aci_data_id18_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id18_20)
#### fit aci curve
fit_aci_id18_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id18_20)
summary(fit_aci_id18_20)
coef_id18_20 <- coef(fit_aci_id18_20)
#### plot
plot(fit_aci_id18_20)
#### add to dataframe
aci_data_id18_20_data <- cbind(aci_data_id18_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id18_20[,30]),
                               mean(aci_data_id18_20[,72]),
                               fit_aci_id18_20[[2]][1,1],
                               fit_aci_id18_20[[2]][1,2],
                               fit_aci_id18_20[[2]][2,1],
                               fit_aci_id18_20[[2]][2,2],
                               fit_aci_id18_20[[2]][3,1],
                               fit_aci_id18_20[[2]][3,2],
                               # fit_aci_id18_20[[2]][4,1],
                               # fit_aci_id18_20[[2]][4,2],
                               fit_aci_id18_20$RMSE,
                               fit_aci_id18_20$Ci_transition,
                               fit_aci_id18_20$citransition,
                               fit_aci_id18_20$Km,
                               fit_aci_id18_20$GammaStar,
                               fit_aci_id18_20$fitmethod,
                               fit_aci_id18_20$Tcorrect,
                               fit_aci_id18_20$fitTPU)
colnames(aci_data_id18_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id18_20_data)

################################################################################
### plant id18 tleaf25
aci_data_id18_25 = subset(aci_data, id == 18 & set_leafT == 25)
aci_data_id18_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id18_25)
#### fit aci curve
fit_aci_id18_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id18_25)
summary(fit_aci_id18_25)
coef_id18_25 <- coef(fit_aci_id18_25)
#### plot
plot(fit_aci_id18_25)
#### add to dataframe
aci_data_id18_25_data <- cbind(aci_data_id18_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id18_25[,30]),
                               mean(aci_data_id18_25[,72]),
                               fit_aci_id18_25[[2]][1,1],
                               fit_aci_id18_25[[2]][1,2],
                               fit_aci_id18_25[[2]][2,1],
                               fit_aci_id18_25[[2]][2,2],
                               fit_aci_id18_25[[2]][3,1],
                               fit_aci_id18_25[[2]][3,2],
                               # fit_aci_id18_25[[2]][4,1],
                               # fit_aci_id18_25[[2]][4,2],
                               fit_aci_id18_25$RMSE,
                               fit_aci_id18_25$Ci_transition,
                               fit_aci_id18_25$citransition,
                               fit_aci_id18_25$Km,
                               fit_aci_id18_25$GammaStar,
                               fit_aci_id18_25$fitmethod,
                               fit_aci_id18_25$Tcorrect,
                               fit_aci_id18_25$fitTPU)
colnames(aci_data_id18_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id18_25_data)

################################################################################
### plant id18 tleaf31
aci_data_id18_31 = subset(aci_data, id == 18 & set_leafT == 31)
aci_data_id18_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id18_31)
#### fit aci curve
fit_aci_id18_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id18_31)
summary(fit_aci_id18_31)
coef_id18_31 <- coef(fit_aci_id18_31)
#### plot
plot(fit_aci_id18_31)
#### add to dataframe
aci_data_id18_31_data <- cbind(aci_data_id18_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id18_31[,30]),
                               mean(aci_data_id18_31[,72]),
                               fit_aci_id18_31[[2]][1,1],
                               fit_aci_id18_31[[2]][1,2],
                               fit_aci_id18_31[[2]][2,1],
                               fit_aci_id18_31[[2]][2,2],
                               fit_aci_id18_31[[2]][3,1],
                               fit_aci_id18_31[[2]][3,2],
                               # fit_aci_id18_31[[2]][4,1],
                               # fit_aci_id18_31[[2]][4,2],
                               fit_aci_id18_31$RMSE,
                               fit_aci_id18_31$Ci_transition,
                               fit_aci_id18_31$citransition,
                               fit_aci_id18_31$Km,
                               fit_aci_id18_31$GammaStar,
                               fit_aci_id18_31$fitmethod,
                               fit_aci_id18_31$Tcorrect,
                               fit_aci_id18_31$fitTPU)
colnames(aci_data_id18_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id18_31_data)

################################################################################
### plant id19 tleaf20
aci_data_id19_20 = subset(aci_data, id == 19 & set_leafT == 20)
aci_data_id19_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id19_20)
#### fit aci curve
fit_aci_id19_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id19_20)
summary(fit_aci_id19_20)
coef_id19_20 <- coef(fit_aci_id19_20)
#### plot
plot(fit_aci_id19_20)
#### add to dataframe
aci_data_id19_20_data <- cbind(aci_data_id19_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id19_20[,30]),
                               mean(aci_data_id19_20[,72]),
                               fit_aci_id19_20[[2]][1,1],
                               fit_aci_id19_20[[2]][1,2],
                               fit_aci_id19_20[[2]][2,1],
                               fit_aci_id19_20[[2]][2,2],
                               fit_aci_id19_20[[2]][3,1],
                               fit_aci_id19_20[[2]][3,2],
                               # fit_aci_id19_20[[2]][4,1],
                               # fit_aci_id19_20[[2]][4,2],
                               fit_aci_id19_20$RMSE,
                               fit_aci_id19_20$Ci_transition,
                               fit_aci_id19_20$citransition,
                               fit_aci_id19_20$Km,
                               fit_aci_id19_20$GammaStar,
                               fit_aci_id19_20$fitmethod,
                               fit_aci_id19_20$Tcorrect,
                               fit_aci_id19_20$fitTPU)
colnames(aci_data_id19_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id19_20_data)

################################################################################
### plant id19 tleaf25
aci_data_id19_25 = subset(aci_data, id == 19 & set_leafT == 25)
aci_data_id19_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id19_25)
#### fit aci curve
fit_aci_id19_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id19_25)
summary(fit_aci_id19_25)
coef_id19_25 <- coef(fit_aci_id19_25)
#### plot
plot(fit_aci_id19_25)
#### add to dataframe
aci_data_id19_25_data <- cbind(aci_data_id19_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id19_25[,30]),
                               mean(aci_data_id19_25[,72]),
                               fit_aci_id19_25[[2]][1,1],
                               fit_aci_id19_25[[2]][1,2],
                               fit_aci_id19_25[[2]][2,1],
                               fit_aci_id19_25[[2]][2,2],
                               fit_aci_id19_25[[2]][3,1],
                               fit_aci_id19_25[[2]][3,2],
                               # fit_aci_id19_25[[2]][4,1],
                               # fit_aci_id19_25[[2]][4,2],
                               fit_aci_id19_25$RMSE,
                               fit_aci_id19_25$Ci_transition,
                               fit_aci_id19_25$citransition,
                               fit_aci_id19_25$Km,
                               fit_aci_id19_25$GammaStar,
                               fit_aci_id19_25$fitmethod,
                               fit_aci_id19_25$Tcorrect,
                               fit_aci_id19_25$fitTPU)
colnames(aci_data_id19_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id19_25_data)

################################################################################
### plant id19 tleaf31
aci_data_id19_31 = subset(aci_data, id == 19 & set_leafT == 31)
aci_data_id19_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id19_31)
#### fit aci curve
fit_aci_id19_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id19_31)
summary(fit_aci_id19_31)
coef_id19_31 <- coef(fit_aci_id19_31)
#### plot
plot(fit_aci_id19_31)
#### add to dataframe
aci_data_id19_31_data <- cbind(aci_data_id19_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id19_31[,30]),
                               mean(aci_data_id19_31[,72]),
                               fit_aci_id19_31[[2]][1,1],
                               fit_aci_id19_31[[2]][1,2],
                               fit_aci_id19_31[[2]][2,1],
                               fit_aci_id19_31[[2]][2,2],
                               fit_aci_id19_31[[2]][3,1],
                               fit_aci_id19_31[[2]][3,2],
                               # fit_aci_id19_31[[2]][4,1],
                               # fit_aci_id19_31[[2]][4,2],
                               fit_aci_id19_31$RMSE,
                               fit_aci_id19_31$Ci_transition,
                               fit_aci_id19_31$citransition,
                               fit_aci_id19_31$Km,
                               fit_aci_id19_31$GammaStar,
                               fit_aci_id19_31$fitmethod,
                               fit_aci_id19_31$Tcorrect,
                               fit_aci_id19_31$fitTPU)
colnames(aci_data_id19_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id19_31_data)

################################################################################
### plant id20 tleaf20
aci_data_id20_20 = subset(aci_data, id == 20 & set_leafT == 20)
aci_data_id20_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id20_20)
#### fit aci curve
fit_aci_id20_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id20_20)
summary(fit_aci_id20_20)
coef_id20_20 <- coef(fit_aci_id20_20)
#### plot
plot(fit_aci_id20_20)
#### add to dataframe
aci_data_id20_20_data <- cbind(aci_data_id20_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id20_20[,30]),
                               mean(aci_data_id20_20[,72]),
                               fit_aci_id20_20[[2]][1,1],
                               fit_aci_id20_20[[2]][1,2],
                               fit_aci_id20_20[[2]][2,1],
                               fit_aci_id20_20[[2]][2,2],
                               fit_aci_id20_20[[2]][3,1],
                               fit_aci_id20_20[[2]][3,2],
                               # fit_aci_id20_20[[2]][4,1],
                               # fit_aci_id20_20[[2]][4,2],
                               fit_aci_id20_20$RMSE,
                               fit_aci_id20_20$Ci_transition,
                               fit_aci_id20_20$citransition,
                               fit_aci_id20_20$Km,
                               fit_aci_id20_20$GammaStar,
                               fit_aci_id20_20$fitmethod,
                               fit_aci_id20_20$Tcorrect,
                               fit_aci_id20_20$fitTPU)
colnames(aci_data_id20_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id20_20_data)

################################################################################
### plant id20 tleaf25
aci_data_id20_25 = subset(aci_data, id == 20 & set_leafT == 25)
aci_data_id20_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id20_25)
#### fit aci curve
fit_aci_id20_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id20_25)
summary(fit_aci_id20_25)
coef_id20_25 <- coef(fit_aci_id20_25)
#### plot
plot(fit_aci_id20_25)
#### add to dataframe
aci_data_id20_25_data <- cbind(aci_data_id20_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id20_25[,30]),
                               mean(aci_data_id20_25[,72]),
                               fit_aci_id20_25[[2]][1,1],
                               fit_aci_id20_25[[2]][1,2],
                               fit_aci_id20_25[[2]][2,1],
                               fit_aci_id20_25[[2]][2,2],
                               fit_aci_id20_25[[2]][3,1],
                               fit_aci_id20_25[[2]][3,2],
                               # fit_aci_id20_25[[2]][4,1],
                               # fit_aci_id20_25[[2]][4,2],
                               fit_aci_id20_25$RMSE,
                               fit_aci_id20_25$Ci_transition,
                               fit_aci_id20_25$citransition,
                               fit_aci_id20_25$Km,
                               fit_aci_id20_25$GammaStar,
                               fit_aci_id20_25$fitmethod,
                               fit_aci_id20_25$Tcorrect,
                               fit_aci_id20_25$fitTPU)
colnames(aci_data_id20_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id20_25_data)

################################################################################
### plant id20 tleaf31
aci_data_id20_31 = subset(aci_data, id == 20 & set_leafT == 31)
aci_data_id20_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id20_31)
#### fit aci curve
fit_aci_id20_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id20_31)
summary(fit_aci_id20_31)
coef_id20_31 <- coef(fit_aci_id20_31)
#### plot
plot(fit_aci_id20_31)
#### add to dataframe
aci_data_id20_31_data <- cbind(aci_data_id20_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id20_31[,30]),
                               mean(aci_data_id20_31[,72]),
                               fit_aci_id20_31[[2]][1,1],
                               fit_aci_id20_31[[2]][1,2],
                               fit_aci_id20_31[[2]][2,1],
                               fit_aci_id20_31[[2]][2,2],
                               fit_aci_id20_31[[2]][3,1],
                               fit_aci_id20_31[[2]][3,2],
                               # fit_aci_id20_31[[2]][4,1],
                               # fit_aci_id20_31[[2]][4,2],
                               fit_aci_id20_31$RMSE,
                               fit_aci_id20_31$Ci_transition,
                               fit_aci_id20_31$citransition,
                               fit_aci_id20_31$Km,
                               fit_aci_id20_31$GammaStar,
                               fit_aci_id20_31$fitmethod,
                               fit_aci_id20_31$Tcorrect,
                               fit_aci_id20_31$fitTPU)
colnames(aci_data_id20_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id20_31_data)

################################################################################
### plant id21 tleaf20
aci_data_id21_20 = subset(aci_data, id == 21 & set_leafT == 20)
aci_data_id21_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id21_20)
#### fit aci curve
fit_aci_id21_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id21_20)
summary(fit_aci_id21_20)
coef_id21_20 <- coef(fit_aci_id21_20)
#### plot
plot(fit_aci_id21_20)
#### add to dataframe
aci_data_id21_20_data <- cbind(aci_data_id21_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id21_20[,30]),
                               mean(aci_data_id21_20[,72]),
                               fit_aci_id21_20[[2]][1,1],
                               fit_aci_id21_20[[2]][1,2],
                               fit_aci_id21_20[[2]][2,1],
                               fit_aci_id21_20[[2]][2,2],
                               fit_aci_id21_20[[2]][3,1],
                               fit_aci_id21_20[[2]][3,2],
                               # fit_aci_id21_20[[2]][4,1],
                               # fit_aci_id21_20[[2]][4,2],
                               fit_aci_id21_20$RMSE,
                               fit_aci_id21_20$Ci_transition,
                               fit_aci_id21_20$citransition,
                               fit_aci_id21_20$Km,
                               fit_aci_id21_20$GammaStar,
                               fit_aci_id21_20$fitmethod,
                               fit_aci_id21_20$Tcorrect,
                               fit_aci_id21_20$fitTPU)
colnames(aci_data_id21_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id21_20_data)

################################################################################
### plant id21 tleaf25
aci_data_id21_25 = subset(aci_data, id == 21 & set_leafT == 25)
aci_data_id21_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id21_25)
#### fit aci curve
fit_aci_id21_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id21_25)
summary(fit_aci_id21_25)
coef_id21_25 <- coef(fit_aci_id21_25)
#### plot
plot(fit_aci_id21_25)
#### add to dataframe
aci_data_id21_25_data <- cbind(aci_data_id21_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id21_25[,30]),
                               mean(aci_data_id21_25[,72]),
                               fit_aci_id21_25[[2]][1,1],
                               fit_aci_id21_25[[2]][1,2],
                               fit_aci_id21_25[[2]][2,1],
                               fit_aci_id21_25[[2]][2,2],
                               fit_aci_id21_25[[2]][3,1],
                               fit_aci_id21_25[[2]][3,2],
                               # fit_aci_id21_25[[2]][4,1],
                               # fit_aci_id21_25[[2]][4,2],
                               fit_aci_id21_25$RMSE,
                               fit_aci_id21_25$Ci_transition,
                               fit_aci_id21_25$citransition,
                               fit_aci_id21_25$Km,
                               fit_aci_id21_25$GammaStar,
                               fit_aci_id21_25$fitmethod,
                               fit_aci_id21_25$Tcorrect,
                               fit_aci_id21_25$fitTPU)
colnames(aci_data_id21_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id21_25_data)

################################################################################
### plant id21 tleaf31
aci_data_id21_31 = subset(aci_data, id == 21 & set_leafT == 31)
aci_data_id21_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id21_31)
#### fit aci curve
fit_aci_id21_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id21_31)
summary(fit_aci_id21_31)
coef_id21_31 <- coef(fit_aci_id21_31)
#### plot
plot(fit_aci_id21_31)
#### add to dataframe
aci_data_id21_31_data <- cbind(aci_data_id21_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id21_31[,30]),
                               mean(aci_data_id21_31[,72]),
                               fit_aci_id21_31[[2]][1,1],
                               fit_aci_id21_31[[2]][1,2],
                               fit_aci_id21_31[[2]][2,1],
                               fit_aci_id21_31[[2]][2,2],
                               fit_aci_id21_31[[2]][3,1],
                               fit_aci_id21_31[[2]][3,2],
                               # fit_aci_id21_31[[2]][4,1],
                               # fit_aci_id21_31[[2]][4,2],
                               fit_aci_id21_31$RMSE,
                               fit_aci_id21_31$Ci_transition,
                               fit_aci_id21_31$citransition,
                               fit_aci_id21_31$Km,
                               fit_aci_id21_31$GammaStar,
                               fit_aci_id21_31$fitmethod,
                               fit_aci_id21_31$Tcorrect,
                               fit_aci_id21_31$fitTPU)
colnames(aci_data_id21_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id21_31_data)

################################################################################
### plant id22 tleaf20
aci_data_id22_20 = subset(aci_data, id == 22 & set_leafT == 20)
aci_data_id22_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id22_20)
#### fit aci curve
fit_aci_id22_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id22_20)
summary(fit_aci_id22_20)
coef_id22_20 <- coef(fit_aci_id22_20)
#### plot
plot(fit_aci_id22_20)
#### add to dataframe
aci_data_id22_20_data <- cbind(aci_data_id22_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id22_20[,30]),
                               mean(aci_data_id22_20[,72]),
                               fit_aci_id22_20[[2]][1,1],
                               fit_aci_id22_20[[2]][1,2],
                               fit_aci_id22_20[[2]][2,1],
                               fit_aci_id22_20[[2]][2,2],
                               fit_aci_id22_20[[2]][3,1],
                               fit_aci_id22_20[[2]][3,2],
                               # fit_aci_id22_20[[2]][4,1],
                               # fit_aci_id22_20[[2]][4,2],
                               fit_aci_id22_20$RMSE,
                               fit_aci_id22_20$Ci_transition,
                               fit_aci_id22_20$citransition,
                               fit_aci_id22_20$Km,
                               fit_aci_id22_20$GammaStar,
                               fit_aci_id22_20$fitmethod,
                               fit_aci_id22_20$Tcorrect,
                               fit_aci_id22_20$fitTPU)
colnames(aci_data_id22_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id22_20_data)

################################################################################
### plant id22 tleaf25
aci_data_id22_25 = subset(aci_data, id == 22 & set_leafT == 25)
aci_data_id22_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id22_25)
#### fit aci curve
fit_aci_id22_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id22_25)
summary(fit_aci_id22_25)
coef_id22_25 <- coef(fit_aci_id22_25)
#### plot
plot(fit_aci_id22_25)
#### add to dataframe
aci_data_id22_25_data <- cbind(aci_data_id22_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id22_25[,30]),
                               mean(aci_data_id22_25[,72]),
                               fit_aci_id22_25[[2]][1,1],
                               fit_aci_id22_25[[2]][1,2],
                               fit_aci_id22_25[[2]][2,1],
                               fit_aci_id22_25[[2]][2,2],
                               fit_aci_id22_25[[2]][3,1],
                               fit_aci_id22_25[[2]][3,2],
                               # fit_aci_id22_25[[2]][4,1],
                               # fit_aci_id22_25[[2]][4,2],
                               fit_aci_id22_25$RMSE,
                               fit_aci_id22_25$Ci_transition,
                               fit_aci_id22_25$citransition,
                               fit_aci_id22_25$Km,
                               fit_aci_id22_25$GammaStar,
                               fit_aci_id22_25$fitmethod,
                               fit_aci_id22_25$Tcorrect,
                               fit_aci_id22_25$fitTPU)
colnames(aci_data_id22_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id22_25_data)

################################################################################
### plant id22 tleaf31
aci_data_id22_31 = subset(aci_data, id == 22 & set_leafT == 31)
aci_data_id22_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id22_31)
#### fit aci curve
fit_aci_id22_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id22_31)
summary(fit_aci_id22_31)
coef_id22_31 <- coef(fit_aci_id22_31)
#### plot
plot(fit_aci_id22_31)
#### add to dataframe
aci_data_id22_31_data <- cbind(aci_data_id22_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id22_31[,30]),
                               mean(aci_data_id22_31[,72]),
                               fit_aci_id22_31[[2]][1,1],
                               fit_aci_id22_31[[2]][1,2],
                               fit_aci_id22_31[[2]][2,1],
                               fit_aci_id22_31[[2]][2,2],
                               fit_aci_id22_31[[2]][3,1],
                               fit_aci_id22_31[[2]][3,2],
                               # fit_aci_id22_31[[2]][4,1],
                               # fit_aci_id22_31[[2]][4,2],
                               fit_aci_id22_31$RMSE,
                               fit_aci_id22_31$Ci_transition,
                               fit_aci_id22_31$citransition,
                               fit_aci_id22_31$Km,
                               fit_aci_id22_31$GammaStar,
                               fit_aci_id22_31$fitmethod,
                               fit_aci_id22_31$Tcorrect,
                               fit_aci_id22_31$fitTPU)
colnames(aci_data_id22_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id22_31_data)

################################################################################
### plant id23 tleaf20
aci_data_id23_20 = subset(aci_data, id == 23 & set_leafT == 20)
aci_data_id23_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id23_20)
#### fit aci curve
fit_aci_id23_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id23_20)
summary(fit_aci_id23_20)
coef_id23_20 <- coef(fit_aci_id23_20)
#### plot
plot(fit_aci_id23_20)
#### add to dataframe
aci_data_id23_20_data <- cbind(aci_data_id23_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id23_20[,30]),
                               mean(aci_data_id23_20[,72]),
                               fit_aci_id23_20[[2]][1,1],
                               fit_aci_id23_20[[2]][1,2],
                               fit_aci_id23_20[[2]][2,1],
                               fit_aci_id23_20[[2]][2,2],
                               fit_aci_id23_20[[2]][3,1],
                               fit_aci_id23_20[[2]][3,2],
                               # fit_aci_id23_20[[2]][4,1],
                               # fit_aci_id23_20[[2]][4,2],
                               fit_aci_id23_20$RMSE,
                               fit_aci_id23_20$Ci_transition,
                               fit_aci_id23_20$citransition,
                               fit_aci_id23_20$Km,
                               fit_aci_id23_20$GammaStar,
                               fit_aci_id23_20$fitmethod,
                               fit_aci_id23_20$Tcorrect,
                               fit_aci_id23_20$fitTPU)
colnames(aci_data_id23_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id23_20_data)

################################################################################
### plant id23 tleaf25
aci_data_id23_25 = subset(aci_data, id == 23 & set_leafT == 25)
aci_data_id23_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id23_25)
#### fit aci curve
fit_aci_id23_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id23_25)
summary(fit_aci_id23_25)
coef_id23_25 <- coef(fit_aci_id23_25)
#### plot
plot(fit_aci_id23_25)
#### add to dataframe
aci_data_id23_25_data <- cbind(aci_data_id23_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id23_25[,30]),
                               mean(aci_data_id23_25[,72]),
                               fit_aci_id23_25[[2]][1,1],
                               fit_aci_id23_25[[2]][1,2],
                               fit_aci_id23_25[[2]][2,1],
                               fit_aci_id23_25[[2]][2,2],
                               fit_aci_id23_25[[2]][3,1],
                               fit_aci_id23_25[[2]][3,2],
                               # fit_aci_id23_25[[2]][4,1],
                               # fit_aci_id23_25[[2]][4,2],
                               fit_aci_id23_25$RMSE,
                               fit_aci_id23_25$Ci_transition,
                               fit_aci_id23_25$citransition,
                               fit_aci_id23_25$Km,
                               fit_aci_id23_25$GammaStar,
                               fit_aci_id23_25$fitmethod,
                               fit_aci_id23_25$Tcorrect,
                               fit_aci_id23_25$fitTPU)
colnames(aci_data_id23_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id23_25_data)

################################################################################
### plant id23 tleaf31
aci_data_id23_31 = subset(aci_data, id == 23 & set_leafT == 31)
aci_data_id23_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id23_31)
#### fit aci curve
fit_aci_id23_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id23_31)
summary(fit_aci_id23_31)
coef_id23_31 <- coef(fit_aci_id23_31)
#### plot
plot(fit_aci_id23_31)
#### add to dataframe
aci_data_id23_31_data <- cbind(aci_data_id23_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id23_31[,30]),
                               mean(aci_data_id23_31[,72]),
                               fit_aci_id23_31[[2]][1,1],
                               fit_aci_id23_31[[2]][1,2],
                               fit_aci_id23_31[[2]][2,1],
                               fit_aci_id23_31[[2]][2,2],
                               fit_aci_id23_31[[2]][3,1],
                               fit_aci_id23_31[[2]][3,2],
                               # fit_aci_id23_31[[2]][4,1],
                               # fit_aci_id23_31[[2]][4,2],
                               fit_aci_id23_31$RMSE,
                               fit_aci_id23_31$Ci_transition,
                               fit_aci_id23_31$citransition,
                               fit_aci_id23_31$Km,
                               fit_aci_id23_31$GammaStar,
                               fit_aci_id23_31$fitmethod,
                               fit_aci_id23_31$Tcorrect,
                               fit_aci_id23_31$fitTPU)
colnames(aci_data_id23_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id23_31_data)

################################################################################
### plant id24 tleaf20
aci_data_id24_20 = subset(aci_data, id == 24 & set_leafT == 20)
aci_data_id24_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id24_20)
#### fit aci curve
fit_aci_id24_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id24_20)
summary(fit_aci_id24_20)
coef_id24_20 <- coef(fit_aci_id24_20)
#### plot
plot(fit_aci_id24_20)
#### add to dataframe
aci_data_id24_20_data <- cbind(aci_data_id24_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id24_20[,30]),
                               mean(aci_data_id24_20[,72]),
                               fit_aci_id24_20[[2]][1,1],
                               fit_aci_id24_20[[2]][1,2],
                               fit_aci_id24_20[[2]][2,1],
                               fit_aci_id24_20[[2]][2,2],
                               fit_aci_id24_20[[2]][3,1],
                               fit_aci_id24_20[[2]][3,2],
                               # fit_aci_id24_20[[2]][4,1],
                               # fit_aci_id24_20[[2]][4,2],
                               fit_aci_id24_20$RMSE,
                               fit_aci_id24_20$Ci_transition,
                               fit_aci_id24_20$citransition,
                               fit_aci_id24_20$Km,
                               fit_aci_id24_20$GammaStar,
                               fit_aci_id24_20$fitmethod,
                               fit_aci_id24_20$Tcorrect,
                               fit_aci_id24_20$fitTPU)
colnames(aci_data_id24_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id24_20_data)

################################################################################
### plant id24 tleaf25
aci_data_id24_25 = subset(aci_data, id == 24 & set_leafT == 25)
aci_data_id24_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id24_25)
#### fit aci curve
fit_aci_id24_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id24_25)
summary(fit_aci_id24_25)
coef_id24_25 <- coef(fit_aci_id24_25)
#### plot
plot(fit_aci_id24_25)
#### add to dataframe
aci_data_id24_25_data <- cbind(aci_data_id24_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id24_25[,30]),
                               mean(aci_data_id24_25[,72]),
                               fit_aci_id24_25[[2]][1,1],
                               fit_aci_id24_25[[2]][1,2],
                               fit_aci_id24_25[[2]][2,1],
                               fit_aci_id24_25[[2]][2,2],
                               fit_aci_id24_25[[2]][3,1],
                               fit_aci_id24_25[[2]][3,2],
                               # fit_aci_id24_25[[2]][4,1],
                               # fit_aci_id24_25[[2]][4,2],
                               fit_aci_id24_25$RMSE,
                               fit_aci_id24_25$Ci_transition,
                               fit_aci_id24_25$citransition,
                               fit_aci_id24_25$Km,
                               fit_aci_id24_25$GammaStar,
                               fit_aci_id24_25$fitmethod,
                               fit_aci_id24_25$Tcorrect,
                               fit_aci_id24_25$fitTPU)
colnames(aci_data_id24_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id24_25_data)

################################################################################
### plant id24 tleaf31
aci_data_id24_31 = subset(aci_data, id == 24 & set_leafT == 31)
aci_data_id24_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id24_31)
#### fit aci curve
fit_aci_id24_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id24_31)
summary(fit_aci_id24_31)
coef_id24_31 <- coef(fit_aci_id24_31)
#### plot
plot(fit_aci_id24_31)
#### add to dataframe
aci_data_id24_31_data <- cbind(aci_data_id24_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id24_31[,30]),
                               mean(aci_data_id24_31[,72]),
                               fit_aci_id24_31[[2]][1,1],
                               fit_aci_id24_31[[2]][1,2],
                               fit_aci_id24_31[[2]][2,1],
                               fit_aci_id24_31[[2]][2,2],
                               fit_aci_id24_31[[2]][3,1],
                               fit_aci_id24_31[[2]][3,2],
                               # fit_aci_id24_31[[2]][4,1],
                               # fit_aci_id24_31[[2]][4,2],
                               fit_aci_id24_31$RMSE,
                               fit_aci_id24_31$Ci_transition,
                               fit_aci_id24_31$citransition,
                               fit_aci_id24_31$Km,
                               fit_aci_id24_31$GammaStar,
                               fit_aci_id24_31$fitmethod,
                               fit_aci_id24_31$Tcorrect,
                               fit_aci_id24_31$fitTPU)
colnames(aci_data_id24_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id24_31_data)

################################################################################
### plant id25 tleaf20
aci_data_id25_20 = subset(aci_data, id == 25 & set_leafT == 20)
aci_data_id25_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id25_20)
#### fit aci curve
fit_aci_id25_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id25_20)
summary(fit_aci_id25_20)
coef_id25_20 <- coef(fit_aci_id25_20)
#### plot
plot(fit_aci_id25_20)
#### add to dataframe
aci_data_id25_20_data <- cbind(aci_data_id25_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id25_20[,30]),
                               mean(aci_data_id25_20[,72]),
                               fit_aci_id25_20[[2]][1,1],
                               fit_aci_id25_20[[2]][1,2],
                               fit_aci_id25_20[[2]][2,1],
                               fit_aci_id25_20[[2]][2,2],
                               fit_aci_id25_20[[2]][3,1],
                               fit_aci_id25_20[[2]][3,2],
                               # fit_aci_id25_20[[2]][4,1],
                               # fit_aci_id25_20[[2]][4,2],
                               fit_aci_id25_20$RMSE,
                               fit_aci_id25_20$Ci_transition,
                               fit_aci_id25_20$citransition,
                               fit_aci_id25_20$Km,
                               fit_aci_id25_20$GammaStar,
                               fit_aci_id25_20$fitmethod,
                               fit_aci_id25_20$Tcorrect,
                               fit_aci_id25_20$fitTPU)
colnames(aci_data_id25_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id25_20_data)

################################################################################
### plant id25 tleaf25
aci_data_id25_25 = subset(aci_data, id == 25 & set_leafT == 25)
aci_data_id25_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id25_25)
#### fit aci curve
fit_aci_id25_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id25_25)
summary(fit_aci_id25_25)
coef_id25_25 <- coef(fit_aci_id25_25)
#### plot
plot(fit_aci_id25_25)
#### add to dataframe
aci_data_id25_25_data <- cbind(aci_data_id25_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id25_25[,30]),
                               mean(aci_data_id25_25[,72]),
                               fit_aci_id25_25[[2]][1,1],
                               fit_aci_id25_25[[2]][1,2],
                               fit_aci_id25_25[[2]][2,1],
                               fit_aci_id25_25[[2]][2,2],
                               fit_aci_id25_25[[2]][3,1],
                               fit_aci_id25_25[[2]][3,2],
                               # fit_aci_id25_25[[2]][4,1],
                               # fit_aci_id25_25[[2]][4,2],
                               fit_aci_id25_25$RMSE,
                               fit_aci_id25_25$Ci_transition,
                               fit_aci_id25_25$citransition,
                               fit_aci_id25_25$Km,
                               fit_aci_id25_25$GammaStar,
                               fit_aci_id25_25$fitmethod,
                               fit_aci_id25_25$Tcorrect,
                               fit_aci_id25_25$fitTPU)
colnames(aci_data_id25_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id25_25_data)

################################################################################
### plant id25 tleaf31
aci_data_id25_31 = subset(aci_data, id == 25 & set_leafT == 31)
aci_data_id25_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id25_31)
#### fit aci curve
fit_aci_id25_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id25_31)
summary(fit_aci_id25_31)
coef_id25_31 <- coef(fit_aci_id25_31)
#### plot
plot(fit_aci_id25_31)
#### add to dataframe
aci_data_id25_31_data <- cbind(aci_data_id25_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id25_31[,30]),
                               mean(aci_data_id25_31[,72]),
                               fit_aci_id25_31[[2]][1,1],
                               fit_aci_id25_31[[2]][1,2],
                               fit_aci_id25_31[[2]][2,1],
                               fit_aci_id25_31[[2]][2,2],
                               fit_aci_id25_31[[2]][3,1],
                               fit_aci_id25_31[[2]][3,2],
                               # fit_aci_id25_31[[2]][4,1],
                               # fit_aci_id25_31[[2]][4,2],
                               fit_aci_id25_31$RMSE,
                               fit_aci_id25_31$Ci_transition,
                               fit_aci_id25_31$citransition,
                               fit_aci_id25_31$Km,
                               fit_aci_id25_31$GammaStar,
                               fit_aci_id25_31$fitmethod,
                               fit_aci_id25_31$Tcorrect,
                               fit_aci_id25_31$fitTPU)
colnames(aci_data_id25_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id25_31_data)

################################################################################
### plant id26 tleaf20
aci_data_id26_20 = subset(aci_data, id == 26 & set_leafT == 20)
aci_data_id26_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id26_20)
#### fit aci curve
fit_aci_id26_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id26_20)
summary(fit_aci_id26_20)
coef_id26_20 <- coef(fit_aci_id26_20)
#### plot
plot(fit_aci_id26_20)
#### add to dataframe
aci_data_id26_20_data <- cbind(aci_data_id26_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id26_20[,30]),
                               mean(aci_data_id26_20[,72]),
                               fit_aci_id26_20[[2]][1,1],
                               fit_aci_id26_20[[2]][1,2],
                               fit_aci_id26_20[[2]][2,1],
                               fit_aci_id26_20[[2]][2,2],
                               fit_aci_id26_20[[2]][3,1],
                               fit_aci_id26_20[[2]][3,2],
                               # fit_aci_id26_20[[2]][4,1],
                               # fit_aci_id26_20[[2]][4,2],
                               fit_aci_id26_20$RMSE,
                               fit_aci_id26_20$Ci_transition,
                               fit_aci_id26_20$citransition,
                               fit_aci_id26_20$Km,
                               fit_aci_id26_20$GammaStar,
                               fit_aci_id26_20$fitmethod,
                               fit_aci_id26_20$Tcorrect,
                               fit_aci_id26_20$fitTPU)
colnames(aci_data_id26_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id26_20_data)

################################################################################
### plant id26 tleaf25
aci_data_id26_25 = subset(aci_data, id == 26 & set_leafT == 25)
aci_data_id26_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id26_25)
#### fit aci curve
fit_aci_id26_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id26_25)
summary(fit_aci_id26_25)
coef_id26_25 <- coef(fit_aci_id26_25)
#### plot
plot(fit_aci_id26_25)
#### add to dataframe
aci_data_id26_25_data <- cbind(aci_data_id26_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id26_25[,30]),
                               mean(aci_data_id26_25[,72]),
                               fit_aci_id26_25[[2]][1,1],
                               fit_aci_id26_25[[2]][1,2],
                               fit_aci_id26_25[[2]][2,1],
                               fit_aci_id26_25[[2]][2,2],
                               fit_aci_id26_25[[2]][3,1],
                               fit_aci_id26_25[[2]][3,2],
                               # fit_aci_id26_25[[2]][4,1],
                               # fit_aci_id26_25[[2]][4,2],
                               fit_aci_id26_25$RMSE,
                               fit_aci_id26_25$Ci_transition,
                               fit_aci_id26_25$citransition,
                               fit_aci_id26_25$Km,
                               fit_aci_id26_25$GammaStar,
                               fit_aci_id26_25$fitmethod,
                               fit_aci_id26_25$Tcorrect,
                               fit_aci_id26_25$fitTPU)
colnames(aci_data_id26_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id26_25_data)

################################################################################
### plant id26 tleaf31
aci_data_id26_31 = subset(aci_data, id == 26 & set_leafT == 31)
aci_data_id26_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id26_31)
#### fit aci curve
fit_aci_id26_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id26_31)
summary(fit_aci_id26_31)
coef_id26_31 <- coef(fit_aci_id26_31)
#### plot
plot(fit_aci_id26_31)
#### add to dataframe
aci_data_id26_31_data <- cbind(aci_data_id26_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id26_31[,30]),
                               mean(aci_data_id26_31[,72]),
                               fit_aci_id26_31[[2]][1,1],
                               fit_aci_id26_31[[2]][1,2],
                               fit_aci_id26_31[[2]][2,1],
                               fit_aci_id26_31[[2]][2,2],
                               fit_aci_id26_31[[2]][3,1],
                               fit_aci_id26_31[[2]][3,2],
                               # fit_aci_id26_31[[2]][4,1],
                               # fit_aci_id26_31[[2]][4,2],
                               fit_aci_id26_31$RMSE,
                               fit_aci_id26_31$Ci_transition,
                               fit_aci_id26_31$citransition,
                               fit_aci_id26_31$Km,
                               fit_aci_id26_31$GammaStar,
                               fit_aci_id26_31$fitmethod,
                               fit_aci_id26_31$Tcorrect,
                               fit_aci_id26_31$fitTPU)
colnames(aci_data_id26_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id26_31_data)

################################################################################
### plant id27 tleaf20
aci_data_id27_20 = subset(aci_data, id == 27 & set_leafT == 20)
aci_data_id27_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id27_20)
#### fit aci curve
fit_aci_id27_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id27_20)
summary(fit_aci_id27_20)
coef_id27_20 <- coef(fit_aci_id27_20)
#### plot
plot(fit_aci_id27_20)
#### add to dataframe
aci_data_id27_20_data <- cbind(aci_data_id27_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id27_20[,30]),
                               mean(aci_data_id27_20[,72]),
                               fit_aci_id27_20[[2]][1,1],
                               fit_aci_id27_20[[2]][1,2],
                               fit_aci_id27_20[[2]][2,1],
                               fit_aci_id27_20[[2]][2,2],
                               fit_aci_id27_20[[2]][3,1],
                               fit_aci_id27_20[[2]][3,2],
                               # fit_aci_id27_20[[2]][4,1],
                               # fit_aci_id27_20[[2]][4,2],
                               fit_aci_id27_20$RMSE,
                               fit_aci_id27_20$Ci_transition,
                               fit_aci_id27_20$citransition,
                               fit_aci_id27_20$Km,
                               fit_aci_id27_20$GammaStar,
                               fit_aci_id27_20$fitmethod,
                               fit_aci_id27_20$Tcorrect,
                               fit_aci_id27_20$fitTPU)
colnames(aci_data_id27_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id27_20_data)

################################################################################
### plant id27 tleaf25
aci_data_id27_25 = subset(aci_data, id == 27 & set_leafT == 25)
aci_data_id27_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id27_25)
#### fit aci curve
fit_aci_id27_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id27_25)
summary(fit_aci_id27_25)
coef_id27_25 <- coef(fit_aci_id27_25)
#### plot
plot(fit_aci_id27_25)
#### add to dataframe
aci_data_id27_25_data <- cbind(aci_data_id27_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id27_25[,30]),
                               mean(aci_data_id27_25[,72]),
                               fit_aci_id27_25[[2]][1,1],
                               fit_aci_id27_25[[2]][1,2],
                               fit_aci_id27_25[[2]][2,1],
                               fit_aci_id27_25[[2]][2,2],
                               fit_aci_id27_25[[2]][3,1],
                               fit_aci_id27_25[[2]][3,2],
                               # fit_aci_id27_25[[2]][4,1],
                               # fit_aci_id27_25[[2]][4,2],
                               fit_aci_id27_25$RMSE,
                               fit_aci_id27_25$Ci_transition,
                               fit_aci_id27_25$citransition,
                               fit_aci_id27_25$Km,
                               fit_aci_id27_25$GammaStar,
                               fit_aci_id27_25$fitmethod,
                               fit_aci_id27_25$Tcorrect,
                               fit_aci_id27_25$fitTPU)
colnames(aci_data_id27_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id27_25_data)

################################################################################
### plant id27 tleaf31
aci_data_id27_31 = subset(aci_data, id == 27 & set_leafT == 31)
aci_data_id27_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id27_31)
#### fit aci curve
fit_aci_id27_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id27_31)
summary(fit_aci_id27_31)
coef_id27_31 <- coef(fit_aci_id27_31)
#### plot
plot(fit_aci_id27_31)
#### add to dataframe
aci_data_id27_31_data <- cbind(aci_data_id27_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id27_31[,30]),
                               mean(aci_data_id27_31[,72]),
                               fit_aci_id27_31[[2]][1,1],
                               fit_aci_id27_31[[2]][1,2],
                               fit_aci_id27_31[[2]][2,1],
                               fit_aci_id27_31[[2]][2,2],
                               fit_aci_id27_31[[2]][3,1],
                               fit_aci_id27_31[[2]][3,2],
                               # fit_aci_id27_31[[2]][4,1],
                               # fit_aci_id27_31[[2]][4,2],
                               fit_aci_id27_31$RMSE,
                               fit_aci_id27_31$Ci_transition,
                               fit_aci_id27_31$citransition,
                               fit_aci_id27_31$Km,
                               fit_aci_id27_31$GammaStar,
                               fit_aci_id27_31$fitmethod,
                               fit_aci_id27_31$Tcorrect,
                               fit_aci_id27_31$fitTPU)
colnames(aci_data_id27_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id27_31_data)

################################################################################
### plant id28 tleaf20
aci_data_id28_20 = subset(aci_data, id == 28 & set_leafT == 20)
aci_data_id28_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id28_20)
#### fit aci curve
fit_aci_id28_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id28_20)
summary(fit_aci_id28_20)

coef_id28_20 <- coef(fit_aci_id28_20)
#### plot
plot(fit_aci_id28_20)
#### add to dataframe
aci_data_id28_20_data <- cbind(aci_data_id28_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id28_20[,30]),
                               mean(aci_data_id28_20[,72]),
                               fit_aci_id28_20[[2]][1,1],
                               fit_aci_id28_20[[2]][1,2],
                               fit_aci_id28_20[[2]][2,1],
                               fit_aci_id28_20[[2]][2,2],
                               fit_aci_id28_20[[2]][3,1],
                               fit_aci_id28_20[[2]][3,2],
                               # fit_aci_id28_20[[2]][4,1],
                               # fit_aci_id28_20[[2]][4,2],
                               fit_aci_id28_20$RMSE,
                               fit_aci_id28_20$Ci_transition,
                               fit_aci_id28_20$citransition,
                               fit_aci_id28_20$Km,
                               fit_aci_id28_20$GammaStar,
                               fit_aci_id28_20$fitmethod,
                               fit_aci_id28_20$Tcorrect,
                               fit_aci_id28_20$fitTPU)
colnames(aci_data_id28_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id28_20_data)

################################################################################
### plant id28 tleaf25
aci_data_id28_25 = subset(aci_data, id == 28 & set_leafT == 25)
aci_data_id28_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id28_25)
#### fit aci curve
fit_aci_id28_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id28_25)
summary(fit_aci_id28_25)

coef_id28_25 <- coef(fit_aci_id28_25)
#### plot
plot(fit_aci_id28_25)
#### add to dataframe
aci_data_id28_25_data <- cbind(aci_data_id28_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id28_25[,30]),
                               mean(aci_data_id28_25[,72]),
                               fit_aci_id28_25[[2]][1,1],
                               fit_aci_id28_25[[2]][1,2],
                               fit_aci_id28_25[[2]][2,1],
                               fit_aci_id28_25[[2]][2,2],
                               fit_aci_id28_25[[2]][3,1],
                               fit_aci_id28_25[[2]][3,2],
                               # fit_aci_id28_25[[2]][4,1],
                               # fit_aci_id28_25[[2]][4,2],
                               fit_aci_id28_25$RMSE,
                               fit_aci_id28_25$Ci_transition,
                               fit_aci_id28_25$citransition,
                               fit_aci_id28_25$Km,
                               fit_aci_id28_25$GammaStar,
                               fit_aci_id28_25$fitmethod,
                               fit_aci_id28_25$Tcorrect,
                               fit_aci_id28_25$fitTPU)
colnames(aci_data_id28_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id28_25_data)

################################################################################
### plant id28 tleaf31
aci_data_id28_31 = subset(aci_data, id == 28 & set_leafT == 31)
aci_data_id28_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id28_31)
#### fit aci curve
fit_aci_id28_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id28_31)
summary(fit_aci_id28_31)

coef_id28_31 <- coef(fit_aci_id28_31)
#### plot
plot(fit_aci_id28_31)
#### add to dataframe
aci_data_id28_31_data <- cbind(aci_data_id28_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id28_31[,30]),
                               mean(aci_data_id28_31[,72]),
                               fit_aci_id28_31[[2]][1,1],
                               fit_aci_id28_31[[2]][1,2],
                               fit_aci_id28_31[[2]][2,1],
                               fit_aci_id28_31[[2]][2,2],
                               fit_aci_id28_31[[2]][3,1],
                               fit_aci_id28_31[[2]][3,2],
                               # fit_aci_id28_31[[2]][4,1],
                               # fit_aci_id28_31[[2]][4,2],
                               fit_aci_id28_31$RMSE,
                               fit_aci_id28_31$Ci_transition,
                               fit_aci_id28_31$citransition,
                               fit_aci_id28_31$Km,
                               fit_aci_id28_31$GammaStar,
                               fit_aci_id28_31$fitmethod,
                               fit_aci_id28_31$Tcorrect,
                               fit_aci_id28_31$fitTPU)
colnames(aci_data_id28_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id28_31_data)

################################################################################
### plant id29 tleaf20
aci_data_id29_20 = subset(aci_data, id == 29 & set_leafT == 20)
aci_data_id29_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id29_20)
#### fit aci curve
fit_aci_id29_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id29_20)
summary(fit_aci_id29_20)

coef_id29_20 <- coef(fit_aci_id29_20)
#### plot
plot(fit_aci_id29_20)
#### add to dataframe
aci_data_id29_20_data <- cbind(aci_data_id29_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id29_20[,30]),
                               mean(aci_data_id29_20[,72]),
                               fit_aci_id29_20[[2]][1,1],
                               fit_aci_id29_20[[2]][1,2],
                               fit_aci_id29_20[[2]][2,1],
                               fit_aci_id29_20[[2]][2,2],
                               fit_aci_id29_20[[2]][3,1],
                               fit_aci_id29_20[[2]][3,2],
                               # fit_aci_id29_20[[2]][4,1],
                               # fit_aci_id29_20[[2]][4,2],
                               fit_aci_id29_20$RMSE,
                               fit_aci_id29_20$Ci_transition,
                               fit_aci_id29_20$citransition,
                               fit_aci_id29_20$Km,
                               fit_aci_id29_20$GammaStar,
                               fit_aci_id29_20$fitmethod,
                               fit_aci_id29_20$Tcorrect,
                               fit_aci_id29_20$fitTPU)
colnames(aci_data_id29_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id29_20_data)

################################################################################
### plant id29 tleaf25
aci_data_id29_25 = subset(aci_data, id == 29 & set_leafT == 25)
aci_data_id29_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id29_25)
#### fit aci curve
fit_aci_id29_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id29_25)
summary(fit_aci_id29_25)

coef_id29_25 <- coef(fit_aci_id29_25)
#### plot
plot(fit_aci_id29_25)
#### add to dataframe
aci_data_id29_25_data <- cbind(aci_data_id29_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id29_25[,30]),
                               mean(aci_data_id29_25[,72]),
                               fit_aci_id29_25[[2]][1,1],
                               fit_aci_id29_25[[2]][1,2],
                               fit_aci_id29_25[[2]][2,1],
                               fit_aci_id29_25[[2]][2,2],
                               fit_aci_id29_25[[2]][3,1],
                               fit_aci_id29_25[[2]][3,2],
                               # fit_aci_id29_25[[2]][4,1],
                               # fit_aci_id29_25[[2]][4,2],
                               fit_aci_id29_25$RMSE,
                               fit_aci_id29_25$Ci_transition,
                               fit_aci_id29_25$citransition,
                               fit_aci_id29_25$Km,
                               fit_aci_id29_25$GammaStar,
                               fit_aci_id29_25$fitmethod,
                               fit_aci_id29_25$Tcorrect,
                               fit_aci_id29_25$fitTPU)
colnames(aci_data_id29_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id29_25_data)

################################################################################
### plant id29 tleaf31
aci_data_id29_31 = subset(aci_data, id == 29 & set_leafT == 31)
aci_data_id29_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id29_31)
#### fit aci curve
fit_aci_id29_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id29_31)
summary(fit_aci_id29_31)

coef_id29_31 <- coef(fit_aci_id29_31)
#### plot
plot(fit_aci_id29_31)
#### add to dataframe
aci_data_id29_31_data <- cbind(aci_data_id29_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id29_31[,30]),
                               mean(aci_data_id29_31[,72]),
                               fit_aci_id29_31[[2]][1,1],
                               fit_aci_id29_31[[2]][1,2],
                               fit_aci_id29_31[[2]][2,1],
                               fit_aci_id29_31[[2]][2,2],
                               fit_aci_id29_31[[2]][3,1],
                               fit_aci_id29_31[[2]][3,2],
                               # fit_aci_id29_31[[2]][4,1],
                               # fit_aci_id29_31[[2]][4,2],
                               fit_aci_id29_31$RMSE,
                               fit_aci_id29_31$Ci_transition,
                               fit_aci_id29_31$citransition,
                               fit_aci_id29_31$Km,
                               fit_aci_id29_31$GammaStar,
                               fit_aci_id29_31$fitmethod,
                               fit_aci_id29_31$Tcorrect,
                               fit_aci_id29_31$fitTPU)
colnames(aci_data_id29_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id29_31_data)

################################################################################
### plant id30 tleaf20
aci_data_id30_20 = subset(aci_data, id == 30 & set_leafT == 20)
aci_data_id30_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id30_20)
#### fit aci curve
fit_aci_id30_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id30_20)
summary(fit_aci_id30_20)

coef_id30_20 <- coef(fit_aci_id30_20)
#### plot
plot(fit_aci_id30_20)
#### add to dataframe
aci_data_id30_20_data <- cbind(aci_data_id30_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id30_20[,30]),
                               mean(aci_data_id30_20[,72]),
                               fit_aci_id30_20[[2]][1,1],
                               fit_aci_id30_20[[2]][1,2],
                               fit_aci_id30_20[[2]][2,1],
                               fit_aci_id30_20[[2]][2,2],
                               fit_aci_id30_20[[2]][3,1],
                               fit_aci_id30_20[[2]][3,2],
                               # fit_aci_id30_20[[2]][4,1],
                               # fit_aci_id30_20[[2]][4,2],
                               fit_aci_id30_20$RMSE,
                               fit_aci_id30_20$Ci_transition,
                               fit_aci_id30_20$citransition,
                               fit_aci_id30_20$Km,
                               fit_aci_id30_20$GammaStar,
                               fit_aci_id30_20$fitmethod,
                               fit_aci_id30_20$Tcorrect,
                               fit_aci_id30_20$fitTPU)
colnames(aci_data_id30_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id30_20_data)

################################################################################
### plant id30 tleaf25
aci_data_id30_25 = subset(aci_data, id == 30 & set_leafT == 25)
aci_data_id30_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id30_25)
#### fit aci curve
fit_aci_id30_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id30_25)
summary(fit_aci_id30_25)

coef_id30_25 <- coef(fit_aci_id30_25)
#### plot
plot(fit_aci_id30_25)
#### add to dataframe
aci_data_id30_25_data <- cbind(aci_data_id30_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id30_25[,30]),
                               mean(aci_data_id30_25[,72]),
                               fit_aci_id30_25[[2]][1,1],
                               fit_aci_id30_25[[2]][1,2],
                               fit_aci_id30_25[[2]][2,1],
                               fit_aci_id30_25[[2]][2,2],
                               fit_aci_id30_25[[2]][3,1],
                               fit_aci_id30_25[[2]][3,2],
                               # fit_aci_id30_25[[2]][4,1],
                               # fit_aci_id30_25[[2]][4,2],
                               fit_aci_id30_25$RMSE,
                               fit_aci_id30_25$Ci_transition,
                               fit_aci_id30_25$citransition,
                               fit_aci_id30_25$Km,
                               fit_aci_id30_25$GammaStar,
                               fit_aci_id30_25$fitmethod,
                               fit_aci_id30_25$Tcorrect,
                               fit_aci_id30_25$fitTPU)
colnames(aci_data_id30_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id30_25_data)

################################################################################
### plant id30 tleaf31
aci_data_id30_31 = subset(aci_data, id == 30 & set_leafT == 31)
aci_data_id30_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id30_31)
#### fit aci curve
fit_aci_id30_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id30_31)
summary(fit_aci_id30_31)

coef_id30_31 <- coef(fit_aci_id30_31)
#### plot
plot(fit_aci_id30_31)
#### add to dataframe
aci_data_id30_31_data <- cbind(aci_data_id30_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id30_31[,30]),
                               mean(aci_data_id30_31[,72]),
                               fit_aci_id30_31[[2]][1,1],
                               fit_aci_id30_31[[2]][1,2],
                               fit_aci_id30_31[[2]][2,1],
                               fit_aci_id30_31[[2]][2,2],
                               fit_aci_id30_31[[2]][3,1],
                               fit_aci_id30_31[[2]][3,2],
                               # fit_aci_id30_31[[2]][4,1],
                               # fit_aci_id30_31[[2]][4,2],
                               fit_aci_id30_31$RMSE,
                               fit_aci_id30_31$Ci_transition,
                               fit_aci_id30_31$citransition,
                               fit_aci_id30_31$Km,
                               fit_aci_id30_31$GammaStar,
                               fit_aci_id30_31$fitmethod,
                               fit_aci_id30_31$Tcorrect,
                               fit_aci_id30_31$fitTPU)
colnames(aci_data_id30_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id30_31_data)

################################################################################
### plant id31 tleaf20
aci_data_id31_20 = subset(aci_data, id == 31 & set_leafT == 20)
aci_data_id31_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id31_20)
#### fit aci curve
fit_aci_id31_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id31_20)
summary(fit_aci_id31_20)

coef_id31_20 <- coef(fit_aci_id31_20)
#### plot
plot(fit_aci_id31_20)
#### add to dataframe
aci_data_id31_20_data <- cbind(aci_data_id31_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id31_20[,30]),
                               mean(aci_data_id31_20[,72]),
                               fit_aci_id31_20[[2]][1,1],
                               fit_aci_id31_20[[2]][1,2],
                               fit_aci_id31_20[[2]][2,1],
                               fit_aci_id31_20[[2]][2,2],
                               fit_aci_id31_20[[2]][3,1],
                               fit_aci_id31_20[[2]][3,2],
                               # fit_aci_id31_20[[2]][4,1],
                               # fit_aci_id31_20[[2]][4,2],
                               fit_aci_id31_20$RMSE,
                               fit_aci_id31_20$Ci_transition,
                               fit_aci_id31_20$citransition,
                               fit_aci_id31_20$Km,
                               fit_aci_id31_20$GammaStar,
                               fit_aci_id31_20$fitmethod,
                               fit_aci_id31_20$Tcorrect,
                               fit_aci_id31_20$fitTPU)
colnames(aci_data_id31_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id31_20_data)

################################################################################
### plant id31 tleaf25
aci_data_id31_25 = subset(aci_data, id == 31 & set_leafT == 25)
aci_data_id31_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id31_25)
#### fit aci curve
fit_aci_id31_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id31_25)
summary(fit_aci_id31_25)

coef_id31_25 <- coef(fit_aci_id31_25)
#### plot
plot(fit_aci_id31_25)
#### add to dataframe
aci_data_id31_25_data <- cbind(aci_data_id31_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id31_25[,30]),
                               mean(aci_data_id31_25[,72]),
                               fit_aci_id31_25[[2]][1,1],
                               fit_aci_id31_25[[2]][1,2],
                               fit_aci_id31_25[[2]][2,1],
                               fit_aci_id31_25[[2]][2,2],
                               fit_aci_id31_25[[2]][3,1],
                               fit_aci_id31_25[[2]][3,2],
                               # fit_aci_id31_25[[2]][4,1],
                               # fit_aci_id31_25[[2]][4,2],
                               fit_aci_id31_25$RMSE,
                               fit_aci_id31_25$Ci_transition,
                               fit_aci_id31_25$citransition,
                               fit_aci_id31_25$Km,
                               fit_aci_id31_25$GammaStar,
                               fit_aci_id31_25$fitmethod,
                               fit_aci_id31_25$Tcorrect,
                               fit_aci_id31_25$fitTPU)
colnames(aci_data_id31_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id31_25_data)

################################################################################
### plant id31 tleaf31
aci_data_id31_31 = subset(aci_data, id == 31 & set_leafT == 31)
aci_data_id31_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id31_31)
#### fit aci curve
fit_aci_id31_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id31_31)
summary(fit_aci_id31_31)

coef_id31_31 <- coef(fit_aci_id31_31)
#### plot
plot(fit_aci_id31_31)
#### add to dataframe
aci_data_id31_31_data <- cbind(aci_data_id31_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id31_31[,30]),
                               mean(aci_data_id31_31[,72]),
                               fit_aci_id31_31[[2]][1,1],
                               fit_aci_id31_31[[2]][1,2],
                               fit_aci_id31_31[[2]][2,1],
                               fit_aci_id31_31[[2]][2,2],
                               fit_aci_id31_31[[2]][3,1],
                               fit_aci_id31_31[[2]][3,2],
                               # fit_aci_id31_31[[2]][4,1],
                               # fit_aci_id31_31[[2]][4,2],
                               fit_aci_id31_31$RMSE,
                               fit_aci_id31_31$Ci_transition,
                               fit_aci_id31_31$citransition,
                               fit_aci_id31_31$Km,
                               fit_aci_id31_31$GammaStar,
                               fit_aci_id31_31$fitmethod,
                               fit_aci_id31_31$Tcorrect,
                               fit_aci_id31_31$fitTPU)
colnames(aci_data_id31_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id31_31_data)

################################################################################
### plant id32 tleaf20
aci_data_id32_20 = subset(aci_data, id == 32 & set_leafT == 20)
aci_data_id32_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id32_20)
#### fit aci curve
fit_aci_id32_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id32_20)
summary(fit_aci_id32_20)

coef_id32_20 <- coef(fit_aci_id32_20)
#### plot
plot(fit_aci_id32_20)
#### add to dataframe
aci_data_id32_20_data <- cbind(aci_data_id32_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id32_20[,30]),
                               mean(aci_data_id32_20[,72]),
                               fit_aci_id32_20[[2]][1,1],
                               fit_aci_id32_20[[2]][1,2],
                               fit_aci_id32_20[[2]][2,1],
                               fit_aci_id32_20[[2]][2,2],
                               fit_aci_id32_20[[2]][3,1],
                               fit_aci_id32_20[[2]][3,2],
                               # fit_aci_id32_20[[2]][4,1],
                               # fit_aci_id32_20[[2]][4,2],
                               fit_aci_id32_20$RMSE,
                               fit_aci_id32_20$Ci_transition,
                               fit_aci_id32_20$citransition,
                               fit_aci_id32_20$Km,
                               fit_aci_id32_20$GammaStar,
                               fit_aci_id32_20$fitmethod,
                               fit_aci_id32_20$Tcorrect,
                               fit_aci_id32_20$fitTPU)
colnames(aci_data_id32_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id32_20_data)

################################################################################
### plant id32 tleaf25
aci_data_id32_25 = subset(aci_data, id == 32 & set_leafT == 25)
aci_data_id32_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id32_25)
#### fit aci curve
fit_aci_id32_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id32_25)
summary(fit_aci_id32_25)
coef_id32_25 <- coef(fit_aci_id32_25)
#### plot
plot(fit_aci_id32_25)
#### add to dataframe
aci_data_id32_25_data <- cbind(aci_data_id32_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id32_25[,30]),
                               mean(aci_data_id32_25[,72]),
                               fit_aci_id32_25[[2]][1,1],
                               fit_aci_id32_25[[2]][1,2],
                               fit_aci_id32_25[[2]][2,1],
                               fit_aci_id32_25[[2]][2,2],
                               fit_aci_id32_25[[2]][3,1],
                               fit_aci_id32_25[[2]][3,2],
                               # fit_aci_id32_25[[2]][4,1],
                               # fit_aci_id32_25[[2]][4,2],
                               fit_aci_id32_25$RMSE,
                               fit_aci_id32_25$Ci_transition,
                               fit_aci_id32_25$citransition,
                               fit_aci_id32_25$Km,
                               fit_aci_id32_25$GammaStar,
                               fit_aci_id32_25$fitmethod,
                               fit_aci_id32_25$Tcorrect,
                               fit_aci_id32_25$fitTPU)
colnames(aci_data_id32_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id32_25_data)

################################################################################
### plant id32 tleaf31
aci_data_id32_31 = subset(aci_data, id == 32 & set_leafT == 31)
aci_data_id32_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id32_31)
#### fit aci curve
fit_aci_id32_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id32_31)
summary(fit_aci_id32_31)
coef_id32_31 <- coef(fit_aci_id32_31)
#### plot
plot(fit_aci_id32_31)
#### add to dataframe
aci_data_id32_31_data <- cbind(aci_data_id32_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id32_31[,30]),
                               mean(aci_data_id32_31[,72]),
                               fit_aci_id32_31[[2]][1,1],
                               fit_aci_id32_31[[2]][1,2],
                               fit_aci_id32_31[[2]][2,1],
                               fit_aci_id32_31[[2]][2,2],
                               fit_aci_id32_31[[2]][3,1],
                               fit_aci_id32_31[[2]][3,2],
                               # fit_aci_id32_31[[2]][4,1],
                               # fit_aci_id32_31[[2]][4,2],
                               fit_aci_id32_31$RMSE,
                               fit_aci_id32_31$Ci_transition,
                               fit_aci_id32_31$citransition,
                               fit_aci_id32_31$Km,
                               fit_aci_id32_31$GammaStar,
                               fit_aci_id32_31$fitmethod,
                               fit_aci_id32_31$Tcorrect,
                               fit_aci_id32_31$fitTPU)
colnames(aci_data_id32_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id32_31_data)

################################################################################
### plant id33 tleaf20
aci_data_id33_20 = subset(aci_data, id == 33 & set_leafT == 20)
aci_data_id33_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id33_20)
#### fit aci curve
fit_aci_id33_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id33_20)
summary(fit_aci_id33_20)
coef_id33_20 <- coef(fit_aci_id33_20)
#### plot
plot(fit_aci_id33_20)
#### add to dataframe
aci_data_id33_20_data <- cbind(aci_data_id33_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id33_20[,30]),
                               mean(aci_data_id33_20[,72]),
                               fit_aci_id33_20[[2]][1,1],
                               fit_aci_id33_20[[2]][1,2],
                               fit_aci_id33_20[[2]][2,1],
                               fit_aci_id33_20[[2]][2,2],
                               fit_aci_id33_20[[2]][3,1],
                               fit_aci_id33_20[[2]][3,2],
                               # fit_aci_id33_20[[2]][4,1],
                               # fit_aci_id33_20[[2]][4,2],
                               fit_aci_id33_20$RMSE,
                               fit_aci_id33_20$Ci_transition,
                               fit_aci_id33_20$citransition,
                               fit_aci_id33_20$Km,
                               fit_aci_id33_20$GammaStar,
                               fit_aci_id33_20$fitmethod,
                               fit_aci_id33_20$Tcorrect,
                               fit_aci_id33_20$fitTPU)
colnames(aci_data_id33_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id33_20_data)

################################################################################
### plant id33 tleaf25
aci_data_id33_25 = subset(aci_data, id == 33 & set_leafT == 25)
aci_data_id33_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id33_25)
#### fit aci curve
fit_aci_id33_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id33_25)
summary(fit_aci_id33_25)
coef_id33_25 <- coef(fit_aci_id33_25)
#### plot
plot(fit_aci_id33_25)
#### add to dataframe
aci_data_id33_25_data <- cbind(aci_data_id33_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id33_25[,30]),
                               mean(aci_data_id33_25[,72]),
                               fit_aci_id33_25[[2]][1,1],
                               fit_aci_id33_25[[2]][1,2],
                               fit_aci_id33_25[[2]][2,1],
                               fit_aci_id33_25[[2]][2,2],
                               fit_aci_id33_25[[2]][3,1],
                               fit_aci_id33_25[[2]][3,2],
                               # fit_aci_id33_25[[2]][4,1],
                               # fit_aci_id33_25[[2]][4,2],
                               fit_aci_id33_25$RMSE,
                               fit_aci_id33_25$Ci_transition,
                               fit_aci_id33_25$citransition,
                               fit_aci_id33_25$Km,
                               fit_aci_id33_25$GammaStar,
                               fit_aci_id33_25$fitmethod,
                               fit_aci_id33_25$Tcorrect,
                               fit_aci_id33_25$fitTPU)
colnames(aci_data_id33_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id33_25_data)

################################################################################
### plant id33 tleaf31
aci_data_id33_31 = subset(aci_data, id == 33 & set_leafT == 31)
aci_data_id33_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id33_31)
#### fit aci curve
fit_aci_id33_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id33_31)
summary(fit_aci_id33_31)
coef_id33_31 <- coef(fit_aci_id33_31)
#### plot
plot(fit_aci_id33_31)
#### add to dataframe
aci_data_id33_31_data <- cbind(aci_data_id33_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id33_31[,30]),
                               mean(aci_data_id33_31[,72]),
                               fit_aci_id33_31[[2]][1,1],
                               fit_aci_id33_31[[2]][1,2],
                               fit_aci_id33_31[[2]][2,1],
                               fit_aci_id33_31[[2]][2,2],
                               fit_aci_id33_31[[2]][3,1],
                               fit_aci_id33_31[[2]][3,2],
                               # fit_aci_id33_31[[2]][4,1],
                               # fit_aci_id33_31[[2]][4,2],
                               fit_aci_id33_31$RMSE,
                               fit_aci_id33_31$Ci_transition,
                               fit_aci_id33_31$citransition,
                               fit_aci_id33_31$Km,
                               fit_aci_id33_31$GammaStar,
                               fit_aci_id33_31$fitmethod,
                               fit_aci_id33_31$Tcorrect,
                               fit_aci_id33_31$fitTPU)
colnames(aci_data_id33_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id33_31_data)

################################################################################
### plant id34 tleaf20
aci_data_id34_20 = subset(aci_data, id == 34 & set_leafT == 20)
aci_data_id34_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id34_20)
#### fit aci curve
fit_aci_id34_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id34_20)
summary(fit_aci_id34_20)
coef_id34_20 <- coef(fit_aci_id34_20)
#### plot
plot(fit_aci_id34_20)
#### add to dataframe
aci_data_id34_20_data <- cbind(aci_data_id34_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id34_20[,30]),
                               mean(aci_data_id34_20[,72]),
                               fit_aci_id34_20[[2]][1,1],
                               fit_aci_id34_20[[2]][1,2],
                               fit_aci_id34_20[[2]][2,1],
                               fit_aci_id34_20[[2]][2,2],
                               fit_aci_id34_20[[2]][3,1],
                               fit_aci_id34_20[[2]][3,2],
                               # fit_aci_id34_20[[2]][4,1],
                               # fit_aci_id34_20[[2]][4,2],
                               fit_aci_id34_20$RMSE,
                               fit_aci_id34_20$Ci_transition,
                               fit_aci_id34_20$citransition,
                               fit_aci_id34_20$Km,
                               fit_aci_id34_20$GammaStar,
                               fit_aci_id34_20$fitmethod,
                               fit_aci_id34_20$Tcorrect,
                               fit_aci_id34_20$fitTPU)
colnames(aci_data_id34_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id34_20_data)

################################################################################
### plant id34 tleaf25
aci_data_id34_25 = subset(aci_data, id == 34 & set_leafT == 25)
aci_data_id34_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id34_25)
#### fit aci curve
fit_aci_id34_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id34_25)
summary(fit_aci_id34_25)
coef_id34_25 <- coef(fit_aci_id34_25)
#### plot
plot(fit_aci_id34_25)
#### add to dataframe
aci_data_id34_25_data <- cbind(aci_data_id34_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id34_25[,30]),
                               mean(aci_data_id34_25[,72]),
                               fit_aci_id34_25[[2]][1,1],
                               fit_aci_id34_25[[2]][1,2],
                               fit_aci_id34_25[[2]][2,1],
                               fit_aci_id34_25[[2]][2,2],
                               fit_aci_id34_25[[2]][3,1],
                               fit_aci_id34_25[[2]][3,2],
                               # fit_aci_id34_25[[2]][4,1],
                               # fit_aci_id34_25[[2]][4,2],
                               fit_aci_id34_25$RMSE,
                               fit_aci_id34_25$Ci_transition,
                               fit_aci_id34_25$citransition,
                               fit_aci_id34_25$Km,
                               fit_aci_id34_25$GammaStar,
                               fit_aci_id34_25$fitmethod,
                               fit_aci_id34_25$Tcorrect,
                               fit_aci_id34_25$fitTPU)
colnames(aci_data_id34_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id34_25_data)

################################################################################
### plant id34 tleaf31
aci_data_id34_31 = subset(aci_data, id == 34 & set_leafT == 31)
aci_data_id34_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id34_31)
#### fit aci curve
fit_aci_id34_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id34_31)
summary(fit_aci_id34_31)
coef_id34_31 <- coef(fit_aci_id34_31)
#### plot
plot(fit_aci_id34_31)
#### add to dataframe
aci_data_id34_31_data <- cbind(aci_data_id34_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id34_31[,30]),
                               mean(aci_data_id34_31[,72]),
                               fit_aci_id34_31[[2]][1,1],
                               fit_aci_id34_31[[2]][1,2],
                               fit_aci_id34_31[[2]][2,1],
                               fit_aci_id34_31[[2]][2,2],
                               fit_aci_id34_31[[2]][3,1],
                               fit_aci_id34_31[[2]][3,2],
                               # fit_aci_id34_31[[2]][4,1],
                               # fit_aci_id34_31[[2]][4,2],
                               fit_aci_id34_31$RMSE,
                               fit_aci_id34_31$Ci_transition,
                               fit_aci_id34_31$citransition,
                               fit_aci_id34_31$Km,
                               fit_aci_id34_31$GammaStar,
                               fit_aci_id34_31$fitmethod,
                               fit_aci_id34_31$Tcorrect,
                               fit_aci_id34_31$fitTPU)
colnames(aci_data_id34_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id34_31_data)

################################################################################
### plant id35 tleaf20
aci_data_id35_20 = subset(aci_data, id == 35 & set_leafT == 20)
aci_data_id35_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id35_20)
#### fit aci curve
fit_aci_id35_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id35_20)
summary(fit_aci_id35_20)
coef_id35_20 <- coef(fit_aci_id35_20)
#### plot
plot(fit_aci_id35_20)
#### add to dataframe
aci_data_id35_20_data <- cbind(aci_data_id35_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id35_20[,30]),
                               mean(aci_data_id35_20[,72]),
                               fit_aci_id35_20[[2]][1,1],
                               fit_aci_id35_20[[2]][1,2],
                               fit_aci_id35_20[[2]][2,1],
                               fit_aci_id35_20[[2]][2,2],
                               fit_aci_id35_20[[2]][3,1],
                               fit_aci_id35_20[[2]][3,2],
                               # fit_aci_id35_20[[2]][4,1],
                               # fit_aci_id35_20[[2]][4,2],
                               fit_aci_id35_20$RMSE,
                               fit_aci_id35_20$Ci_transition,
                               fit_aci_id35_20$citransition,
                               fit_aci_id35_20$Km,
                               fit_aci_id35_20$GammaStar,
                               fit_aci_id35_20$fitmethod,
                               fit_aci_id35_20$Tcorrect,
                               fit_aci_id35_20$fitTPU)
colnames(aci_data_id35_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id35_20_data)

################################################################################
### plant id35 tleaf25
aci_data_id35_25 = subset(aci_data, id == 35 & set_leafT == 25)
aci_data_id35_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id35_25)
#### fit aci curve
fit_aci_id35_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id35_25)
summary(fit_aci_id35_25)
coef_id35_25 <- coef(fit_aci_id35_25)
#### plot
plot(fit_aci_id35_25)
#### add to dataframe
aci_data_id35_25_data <- cbind(aci_data_id35_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id35_25[,30]),
                               mean(aci_data_id35_25[,72]),
                               fit_aci_id35_25[[2]][1,1],
                               fit_aci_id35_25[[2]][1,2],
                               fit_aci_id35_25[[2]][2,1],
                               fit_aci_id35_25[[2]][2,2],
                               fit_aci_id35_25[[2]][3,1],
                               fit_aci_id35_25[[2]][3,2],
                               # fit_aci_id35_25[[2]][4,1],
                               # fit_aci_id35_25[[2]][4,2],
                               fit_aci_id35_25$RMSE,
                               fit_aci_id35_25$Ci_transition,
                               fit_aci_id35_25$citransition,
                               fit_aci_id35_25$Km,
                               fit_aci_id35_25$GammaStar,
                               fit_aci_id35_25$fitmethod,
                               fit_aci_id35_25$Tcorrect,
                               fit_aci_id35_25$fitTPU)
colnames(aci_data_id35_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id35_25_data)

################################################################################
### plant id35 tleaf31
aci_data_id35_31 = subset(aci_data, id == 35 & set_leafT == 31)
aci_data_id35_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id35_31)
#### fit aci curve
fit_aci_id35_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id35_31)
summary(fit_aci_id35_31)
coef_id35_31 <- coef(fit_aci_id35_31)
#### plot
plot(fit_aci_id35_31)
#### add to dataframe
aci_data_id35_31_data <- cbind(aci_data_id35_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id35_31[,30]),
                               mean(aci_data_id35_31[,72]),
                               fit_aci_id35_31[[2]][1,1],
                               fit_aci_id35_31[[2]][1,2],
                               fit_aci_id35_31[[2]][2,1],
                               fit_aci_id35_31[[2]][2,2],
                               fit_aci_id35_31[[2]][3,1],
                               fit_aci_id35_31[[2]][3,2],
                               # fit_aci_id35_31[[2]][4,1],
                               # fit_aci_id35_31[[2]][4,2],
                               fit_aci_id35_31$RMSE,
                               fit_aci_id35_31$Ci_transition,
                               fit_aci_id35_31$citransition,
                               fit_aci_id35_31$Km,
                               fit_aci_id35_31$GammaStar,
                               fit_aci_id35_31$fitmethod,
                               fit_aci_id35_31$Tcorrect,
                               fit_aci_id35_31$fitTPU)
colnames(aci_data_id35_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id35_31_data)

################################################################################
### plant id36 tleaf20
aci_data_id36_20 = subset(aci_data, id == 36 & set_leafT == 20)
aci_data_id36_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id36_20)
#### fit aci curve
fit_aci_id36_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id36_20)
summary(fit_aci_id36_20)
coef_id36_20 <- coef(fit_aci_id36_20)
#### plot
plot(fit_aci_id36_20)
#### add to dataframe
aci_data_id36_20_data <- cbind(aci_data_id36_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id36_20[,30]),
                               mean(aci_data_id36_20[,72]),
                               fit_aci_id36_20[[2]][1,1],
                               fit_aci_id36_20[[2]][1,2],
                               fit_aci_id36_20[[2]][2,1],
                               fit_aci_id36_20[[2]][2,2],
                               fit_aci_id36_20[[2]][3,1],
                               fit_aci_id36_20[[2]][3,2],
                               # fit_aci_id36_20[[2]][4,1],
                               # fit_aci_id36_20[[2]][4,2],
                               fit_aci_id36_20$RMSE,
                               fit_aci_id36_20$Ci_transition,
                               fit_aci_id36_20$citransition,
                               fit_aci_id36_20$Km,
                               fit_aci_id36_20$GammaStar,
                               fit_aci_id36_20$fitmethod,
                               fit_aci_id36_20$Tcorrect,
                               fit_aci_id36_20$fitTPU)
colnames(aci_data_id36_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id36_20_data)

################################################################################
### plant id36 tleaf25
aci_data_id36_25 = subset(aci_data, id == 36 & set_leafT == 25)
aci_data_id36_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id36_25)
#### fit aci curve
fit_aci_id36_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id36_25)
summary(fit_aci_id36_25)
coef_id36_25 <- coef(fit_aci_id36_25)
#### plot
plot(fit_aci_id36_25)
#### add to dataframe
aci_data_id36_25_data <- cbind(aci_data_id36_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id36_25[,30]),
                               mean(aci_data_id36_25[,72]),
                               fit_aci_id36_25[[2]][1,1],
                               fit_aci_id36_25[[2]][1,2],
                               fit_aci_id36_25[[2]][2,1],
                               fit_aci_id36_25[[2]][2,2],
                               fit_aci_id36_25[[2]][3,1],
                               fit_aci_id36_25[[2]][3,2],
                               # fit_aci_id36_25[[2]][4,1],
                               # fit_aci_id36_25[[2]][4,2],
                               fit_aci_id36_25$RMSE,
                               fit_aci_id36_25$Ci_transition,
                               fit_aci_id36_25$citransition,
                               fit_aci_id36_25$Km,
                               fit_aci_id36_25$GammaStar,
                               fit_aci_id36_25$fitmethod,
                               fit_aci_id36_25$Tcorrect,
                               fit_aci_id36_25$fitTPU)
colnames(aci_data_id36_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id36_25_data)

################################################################################
### plant id36 tleaf31
aci_data_id36_31 = subset(aci_data, id == 36 & set_leafT == 31)
aci_data_id36_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id36_31)
#### fit aci curve
fit_aci_id36_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id36_31)
summary(fit_aci_id36_31)
coef_id36_31 <- coef(fit_aci_id36_31)
#### plot
plot(fit_aci_id36_31)
#### add to dataframe
aci_data_id36_31_data <- cbind(aci_data_id36_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id36_31[,30]),
                               mean(aci_data_id36_31[,72]),
                               fit_aci_id36_31[[2]][1,1],
                               fit_aci_id36_31[[2]][1,2],
                               fit_aci_id36_31[[2]][2,1],
                               fit_aci_id36_31[[2]][2,2],
                               fit_aci_id36_31[[2]][3,1],
                               fit_aci_id36_31[[2]][3,2],
                               # fit_aci_id36_31[[2]][4,1],
                               # fit_aci_id36_31[[2]][4,2],
                               fit_aci_id36_31$RMSE,
                               fit_aci_id36_31$Ci_transition,
                               fit_aci_id36_31$citransition,
                               fit_aci_id36_31$Km,
                               fit_aci_id36_31$GammaStar,
                               fit_aci_id36_31$fitmethod,
                               fit_aci_id36_31$Tcorrect,
                               fit_aci_id36_31$fitTPU)
colnames(aci_data_id36_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id36_31_data)

################################################################################
### plant id37 tleaf20
aci_data_id37_20 = subset(aci_data, id == 37 & set_leafT == 20)
aci_data_id37_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id37_20)
#### fit aci curve
fit_aci_id37_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id37_20)
summary(fit_aci_id37_20)
coef_id37_20 <- coef(fit_aci_id37_20)
#### plot
plot(fit_aci_id37_20)
#### add to dataframe
aci_data_id37_20_data <- cbind(aci_data_id37_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id37_20[,30]),
                               mean(aci_data_id37_20[,72]),
                               fit_aci_id37_20[[2]][1,1],
                               fit_aci_id37_20[[2]][1,2],
                               fit_aci_id37_20[[2]][2,1],
                               fit_aci_id37_20[[2]][2,2],
                               fit_aci_id37_20[[2]][3,1],
                               fit_aci_id37_20[[2]][3,2],
                               # fit_aci_id37_20[[2]][4,1],
                               # fit_aci_id37_20[[2]][4,2],
                               fit_aci_id37_20$RMSE,
                               fit_aci_id37_20$Ci_transition,
                               fit_aci_id37_20$citransition,
                               fit_aci_id37_20$Km,
                               fit_aci_id37_20$GammaStar,
                               fit_aci_id37_20$fitmethod,
                               fit_aci_id37_20$Tcorrect,
                               fit_aci_id37_20$fitTPU)
colnames(aci_data_id37_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id37_20_data)

################################################################################
### plant id37 tleaf25
aci_data_id37_25 = subset(aci_data, id == 37 & set_leafT == 25)
aci_data_id37_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id37_25)
aci_data_id37_25 = subset(aci_data, id == 37 & set_leafT == 25 & Ci < 700)
plot(Adyn ~ Ci, data = aci_data_id37_25)
#### fit aci curve
fit_aci_id37_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id37_25)
summary(fit_aci_id37_25)
coef_id37_25 <- coef(fit_aci_id37_25)
#### plot
plot(fit_aci_id37_25)
#### add to dataframe
aci_data_id37_25_data <- cbind(aci_data_id37_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id37_25[,30]),
                               mean(aci_data_id37_25[,72]),
                               fit_aci_id37_25[[2]][1,1],
                               fit_aci_id37_25[[2]][1,2],
                               fit_aci_id37_25[[2]][2,1],
                               fit_aci_id37_25[[2]][2,2],
                               fit_aci_id37_25[[2]][3,1],
                               fit_aci_id37_25[[2]][3,2],
                               # fit_aci_id37_25[[2]][4,1],
                               # fit_aci_id37_25[[2]][4,2],
                               fit_aci_id37_25$RMSE,
                               fit_aci_id37_25$Ci_transition,
                               fit_aci_id37_25$citransition,
                               fit_aci_id37_25$Km,
                               fit_aci_id37_25$GammaStar,
                               fit_aci_id37_25$fitmethod,
                               fit_aci_id37_25$Tcorrect,
                               fit_aci_id37_25$fitTPU)
colnames(aci_data_id37_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id37_25_data)

################################################################################
### plant id37 tleaf31
aci_data_id37_31 = subset(aci_data, id == 37 & set_leafT == 31)
aci_data_id37_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id37_31)
#### fit aci curve
fit_aci_id37_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id37_31)
summary(fit_aci_id37_31)
coef_id37_31 <- coef(fit_aci_id37_31)
#### plot
plot(fit_aci_id37_31)
#### add to dataframe
aci_data_id37_31_data <- cbind(aci_data_id37_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id37_31[,30]),
                               mean(aci_data_id37_31[,72]),
                               fit_aci_id37_31[[2]][1,1],
                               fit_aci_id37_31[[2]][1,2],
                               fit_aci_id37_31[[2]][2,1],
                               fit_aci_id37_31[[2]][2,2],
                               fit_aci_id37_31[[2]][3,1],
                               fit_aci_id37_31[[2]][3,2],
                               # fit_aci_id37_31[[2]][4,1],
                               # fit_aci_id37_31[[2]][4,2],
                               fit_aci_id37_31$RMSE,
                               fit_aci_id37_31$Ci_transition,
                               fit_aci_id37_31$citransition,
                               fit_aci_id37_31$Km,
                               fit_aci_id37_31$GammaStar,
                               fit_aci_id37_31$fitmethod,
                               fit_aci_id37_31$Tcorrect,
                               fit_aci_id37_31$fitTPU)
colnames(aci_data_id37_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id37_31_data)

################################################################################
### plant id38 tleaf20
aci_data_id38_20 = subset(aci_data, id == 38 & set_leafT == 20)
aci_data_id38_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id38_20)
#### fit aci curve
fit_aci_id38_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id38_20)
summary(fit_aci_id38_20)
coef_id38_20 <- coef(fit_aci_id38_20)
#### plot
plot(fit_aci_id38_20)
#### add to dataframe
aci_data_id38_20_data <- cbind(aci_data_id38_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id38_20[,30]),
                               mean(aci_data_id38_20[,72]),
                               fit_aci_id38_20[[2]][1,1],
                               fit_aci_id38_20[[2]][1,2],
                               fit_aci_id38_20[[2]][2,1],
                               fit_aci_id38_20[[2]][2,2],
                               fit_aci_id38_20[[2]][3,1],
                               fit_aci_id38_20[[2]][3,2],
                               # fit_aci_id38_20[[2]][4,1],
                               # fit_aci_id38_20[[2]][4,2],
                               fit_aci_id38_20$RMSE,
                               fit_aci_id38_20$Ci_transition,
                               fit_aci_id38_20$citransition,
                               fit_aci_id38_20$Km,
                               fit_aci_id38_20$GammaStar,
                               fit_aci_id38_20$fitmethod,
                               fit_aci_id38_20$Tcorrect,
                               fit_aci_id38_20$fitTPU)
colnames(aci_data_id38_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id38_20_data)

################################################################################
### plant id38 tleaf25
aci_data_id38_25 = subset(aci_data, id == 38 & set_leafT == 25)
aci_data_id38_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id38_25)
#### fit aci curve
fit_aci_id38_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id38_25)
summary(fit_aci_id38_25)
coef_id38_25 <- coef(fit_aci_id38_25)
#### plot
plot(fit_aci_id38_25)
#### add to dataframe
aci_data_id38_25_data <- cbind(aci_data_id38_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id38_25[,30]),
                               mean(aci_data_id38_25[,72]),
                               fit_aci_id38_25[[2]][1,1],
                               fit_aci_id38_25[[2]][1,2],
                               fit_aci_id38_25[[2]][2,1],
                               fit_aci_id38_25[[2]][2,2],
                               fit_aci_id38_25[[2]][3,1],
                               fit_aci_id38_25[[2]][3,2],
                               # fit_aci_id38_25[[2]][4,1],
                               # fit_aci_id38_25[[2]][4,2],
                               fit_aci_id38_25$RMSE,
                               fit_aci_id38_25$Ci_transition,
                               fit_aci_id38_25$citransition,
                               fit_aci_id38_25$Km,
                               fit_aci_id38_25$GammaStar,
                               fit_aci_id38_25$fitmethod,
                               fit_aci_id38_25$Tcorrect,
                               fit_aci_id38_25$fitTPU)
colnames(aci_data_id38_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id38_25_data)

################################################################################
### plant id38 tleaf31
aci_data_id38_31 = subset(aci_data, id == 38 & set_leafT == 31)
aci_data_id38_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id38_31)
#### fit aci curve
fit_aci_id38_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id38_31)
summary(fit_aci_id38_31)
coef_id38_31 <- coef(fit_aci_id38_31)
#### plot
plot(fit_aci_id38_31)
#### add to dataframe
aci_data_id38_31_data <- cbind(aci_data_id38_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id38_31[,30]),
                               mean(aci_data_id38_31[,72]),
                               fit_aci_id38_31[[2]][1,1],
                               fit_aci_id38_31[[2]][1,2],
                               fit_aci_id38_31[[2]][2,1],
                               fit_aci_id38_31[[2]][2,2],
                               fit_aci_id38_31[[2]][3,1],
                               fit_aci_id38_31[[2]][3,2],
                               # fit_aci_id38_31[[2]][4,1],
                               # fit_aci_id38_31[[2]][4,2],
                               fit_aci_id38_31$RMSE,
                               fit_aci_id38_31$Ci_transition,
                               fit_aci_id38_31$citransition,
                               fit_aci_id38_31$Km,
                               fit_aci_id38_31$GammaStar,
                               fit_aci_id38_31$fitmethod,
                               fit_aci_id38_31$Tcorrect,
                               fit_aci_id38_31$fitTPU)
colnames(aci_data_id38_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id38_31_data)

################################################################################
### plant id39 tleaf20
aci_data_id39_20 = subset(aci_data, id == 39 & set_leafT == 20)
aci_data_id39_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id39_20)
#### fit aci curve
fit_aci_id39_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id39_20)
summary(fit_aci_id39_20)
coef_id39_20 <- coef(fit_aci_id39_20)
#### plot
plot(fit_aci_id39_20)
#### add to dataframe
aci_data_id39_20_data <- cbind(aci_data_id39_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id39_20[,30]),
                               mean(aci_data_id39_20[,72]),
                               fit_aci_id39_20[[2]][1,1],
                               fit_aci_id39_20[[2]][1,2],
                               fit_aci_id39_20[[2]][2,1],
                               fit_aci_id39_20[[2]][2,2],
                               fit_aci_id39_20[[2]][3,1],
                               fit_aci_id39_20[[2]][3,2],
                               # fit_aci_id39_20[[2]][4,1],
                               # fit_aci_id39_20[[2]][4,2],
                               fit_aci_id39_20$RMSE,
                               fit_aci_id39_20$Ci_transition,
                               fit_aci_id39_20$citransition,
                               fit_aci_id39_20$Km,
                               fit_aci_id39_20$GammaStar,
                               fit_aci_id39_20$fitmethod,
                               fit_aci_id39_20$Tcorrect,
                               fit_aci_id39_20$fitTPU)
colnames(aci_data_id39_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id39_20_data)

################################################################################
### plant id39 tleaf25
aci_data_id39_25 = subset(aci_data, id == 39 & set_leafT == 25)
aci_data_id39_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id39_25)
#### fit aci curve
fit_aci_id39_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id39_25)
summary(fit_aci_id39_25)
coef_id39_25 <- coef(fit_aci_id39_25)
#### plot
plot(fit_aci_id39_25)
#### add to dataframe
aci_data_id39_25_data <- cbind(aci_data_id39_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id39_25[,30]),
                               mean(aci_data_id39_25[,72]),
                               fit_aci_id39_25[[2]][1,1],
                               fit_aci_id39_25[[2]][1,2],
                               fit_aci_id39_25[[2]][2,1],
                               fit_aci_id39_25[[2]][2,2],
                               fit_aci_id39_25[[2]][3,1],
                               fit_aci_id39_25[[2]][3,2],
                               # fit_aci_id39_25[[2]][4,1],
                               # fit_aci_id39_25[[2]][4,2],
                               fit_aci_id39_25$RMSE,
                               fit_aci_id39_25$Ci_transition,
                               fit_aci_id39_25$citransition,
                               fit_aci_id39_25$Km,
                               fit_aci_id39_25$GammaStar,
                               fit_aci_id39_25$fitmethod,
                               fit_aci_id39_25$Tcorrect,
                               fit_aci_id39_25$fitTPU)
colnames(aci_data_id39_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id39_25_data)

################################################################################
### plant id39 tleaf31
aci_data_id39_31 = subset(aci_data, id == 39 & set_leafT == 31)
aci_data_id39_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id39_31)
#### fit aci curve
fit_aci_id39_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id39_31)
summary(fit_aci_id39_31)
coef_id39_31 <- coef(fit_aci_id39_31)
#### plot
plot(fit_aci_id39_31)
#### add to dataframe
aci_data_id39_31_data <- cbind(aci_data_id39_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id39_31[,30]),
                               mean(aci_data_id39_31[,72]),
                               fit_aci_id39_31[[2]][1,1],
                               fit_aci_id39_31[[2]][1,2],
                               fit_aci_id39_31[[2]][2,1],
                               fit_aci_id39_31[[2]][2,2],
                               fit_aci_id39_31[[2]][3,1],
                               fit_aci_id39_31[[2]][3,2],
                               # fit_aci_id39_31[[2]][4,1],
                               # fit_aci_id39_31[[2]][4,2],
                               fit_aci_id39_31$RMSE,
                               fit_aci_id39_31$Ci_transition,
                               fit_aci_id39_31$citransition,
                               fit_aci_id39_31$Km,
                               fit_aci_id39_31$GammaStar,
                               fit_aci_id39_31$fitmethod,
                               fit_aci_id39_31$Tcorrect,
                               fit_aci_id39_31$fitTPU)
colnames(aci_data_id39_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id39_31_data)

################################################################################
### plant id40 tleaf20
aci_data_id40_20 = subset(aci_data, id == 40 & set_leafT == 20)
aci_data_id40_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id40_20)
#### fit aci curve
fit_aci_id40_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id40_20)
summary(fit_aci_id40_20)
coef_id40_20 <- coef(fit_aci_id40_20)
#### plot
plot(fit_aci_id40_20)
#### add to dataframe
aci_data_id40_20_data <- cbind(aci_data_id40_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id40_20[,30]),
                               mean(aci_data_id40_20[,72]),
                               fit_aci_id40_20[[2]][1,1],
                               fit_aci_id40_20[[2]][1,2],
                               fit_aci_id40_20[[2]][2,1],
                               fit_aci_id40_20[[2]][2,2],
                               fit_aci_id40_20[[2]][3,1],
                               fit_aci_id40_20[[2]][3,2],
                               # fit_aci_id40_20[[2]][4,1],
                               # fit_aci_id40_20[[2]][4,2],
                               fit_aci_id40_20$RMSE,
                               fit_aci_id40_20$Ci_transition,
                               fit_aci_id40_20$citransition,
                               fit_aci_id40_20$Km,
                               fit_aci_id40_20$GammaStar,
                               fit_aci_id40_20$fitmethod,
                               fit_aci_id40_20$Tcorrect,
                               fit_aci_id40_20$fitTPU)
colnames(aci_data_id40_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id40_20_data)

################################################################################
### plant id40 tleaf25
aci_data_id40_25 = subset(aci_data, id == 40 & set_leafT == 25)
aci_data_id40_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id40_25)
#### fit aci curve
fit_aci_id40_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id40_25)
summary(fit_aci_id40_25)
coef_id40_25 <- coef(fit_aci_id40_25)
#### plot
plot(fit_aci_id40_25)
#### add to dataframe
aci_data_id40_25_data <- cbind(aci_data_id40_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id40_25[,30]),
                               mean(aci_data_id40_25[,72]),
                               fit_aci_id40_25[[2]][1,1],
                               fit_aci_id40_25[[2]][1,2],
                               fit_aci_id40_25[[2]][2,1],
                               fit_aci_id40_25[[2]][2,2],
                               fit_aci_id40_25[[2]][3,1],
                               fit_aci_id40_25[[2]][3,2],
                               # fit_aci_id40_25[[2]][4,1],
                               # fit_aci_id40_25[[2]][4,2],
                               fit_aci_id40_25$RMSE,
                               fit_aci_id40_25$Ci_transition,
                               fit_aci_id40_25$citransition,
                               fit_aci_id40_25$Km,
                               fit_aci_id40_25$GammaStar,
                               fit_aci_id40_25$fitmethod,
                               fit_aci_id40_25$Tcorrect,
                               fit_aci_id40_25$fitTPU)
colnames(aci_data_id40_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id40_25_data)

################################################################################
### plant id40 tleaf31
aci_data_id40_31 = subset(aci_data, id == 40 & set_leafT == 31)
aci_data_id40_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id40_31)
#### fit aci curve
fit_aci_id40_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id40_31)
summary(fit_aci_id40_31)
coef_id40_31 <- coef(fit_aci_id40_31)
#### plot
plot(fit_aci_id40_31)
#### add to dataframe
aci_data_id40_31_data <- cbind(aci_data_id40_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id40_31[,30]),
                               mean(aci_data_id40_31[,72]),
                               fit_aci_id40_31[[2]][1,1],
                               fit_aci_id40_31[[2]][1,2],
                               fit_aci_id40_31[[2]][2,1],
                               fit_aci_id40_31[[2]][2,2],
                               fit_aci_id40_31[[2]][3,1],
                               fit_aci_id40_31[[2]][3,2],
                               # fit_aci_id40_31[[2]][4,1],
                               # fit_aci_id40_31[[2]][4,2],
                               fit_aci_id40_31$RMSE,
                               fit_aci_id40_31$Ci_transition,
                               fit_aci_id40_31$citransition,
                               fit_aci_id40_31$Km,
                               fit_aci_id40_31$GammaStar,
                               fit_aci_id40_31$fitmethod,
                               fit_aci_id40_31$Tcorrect,
                               fit_aci_id40_31$fitTPU)
colnames(aci_data_id40_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id40_31_data)

################################################################################
### plant id41 tleaf20
aci_data_id41_20 = subset(aci_data, id == 41 & set_leafT == 20)
aci_data_id41_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id41_20)
#### fit aci curve
fit_aci_id41_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id41_20)
summary(fit_aci_id41_20)
coef_id41_20 <- coef(fit_aci_id41_20)
#### plot
plot(fit_aci_id41_20)
#### add to dataframe
aci_data_id41_20_data <- cbind(aci_data_id41_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id41_20[,30]),
                               mean(aci_data_id41_20[,72]),
                               fit_aci_id41_20[[2]][1,1],
                               fit_aci_id41_20[[2]][1,2],
                               fit_aci_id41_20[[2]][2,1],
                               fit_aci_id41_20[[2]][2,2],
                               fit_aci_id41_20[[2]][3,1],
                               fit_aci_id41_20[[2]][3,2],
                               # fit_aci_id41_20[[2]][4,1],
                               # fit_aci_id41_20[[2]][4,2],
                               fit_aci_id41_20$RMSE,
                               fit_aci_id41_20$Ci_transition,
                               fit_aci_id41_20$citransition,
                               fit_aci_id41_20$Km,
                               fit_aci_id41_20$GammaStar,
                               fit_aci_id41_20$fitmethod,
                               fit_aci_id41_20$Tcorrect,
                               fit_aci_id41_20$fitTPU)
colnames(aci_data_id41_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id41_20_data)

################################################################################
### plant id41 tleaf25
aci_data_id41_25 = subset(aci_data, id == 41 & set_leafT == 25)
aci_data_id41_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id41_25)
#### fit aci curve
fit_aci_id41_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id41_25)
summary(fit_aci_id41_25)
coef_id41_25 <- coef(fit_aci_id41_25)
#### plot
plot(fit_aci_id41_25)
#### add to dataframe
aci_data_id41_25_data <- cbind(aci_data_id41_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id41_25[,30]),
                               mean(aci_data_id41_25[,72]),
                               fit_aci_id41_25[[2]][1,1],
                               fit_aci_id41_25[[2]][1,2],
                               fit_aci_id41_25[[2]][2,1],
                               fit_aci_id41_25[[2]][2,2],
                               fit_aci_id41_25[[2]][3,1],
                               fit_aci_id41_25[[2]][3,2],
                               # fit_aci_id41_25[[2]][4,1],
                               # fit_aci_id41_25[[2]][4,2],
                               fit_aci_id41_25$RMSE,
                               fit_aci_id41_25$Ci_transition,
                               fit_aci_id41_25$citransition,
                               fit_aci_id41_25$Km,
                               fit_aci_id41_25$GammaStar,
                               fit_aci_id41_25$fitmethod,
                               fit_aci_id41_25$Tcorrect,
                               fit_aci_id41_25$fitTPU)
colnames(aci_data_id41_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id41_25_data)

################################################################################
### plant id41 tleaf31
aci_data_id41_31 = subset(aci_data, id == 41 & set_leafT == 31)
aci_data_id41_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id41_31)
#### fit aci curve
fit_aci_id41_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id41_31)
summary(fit_aci_id41_31)
coef_id41_31 <- coef(fit_aci_id41_31)
#### plot
plot(fit_aci_id41_31)
#### add to dataframe
aci_data_id41_31_data <- cbind(aci_data_id41_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id41_31[,30]),
                               mean(aci_data_id41_31[,72]),
                               fit_aci_id41_31[[2]][1,1],
                               fit_aci_id41_31[[2]][1,2],
                               fit_aci_id41_31[[2]][2,1],
                               fit_aci_id41_31[[2]][2,2],
                               fit_aci_id41_31[[2]][3,1],
                               fit_aci_id41_31[[2]][3,2],
                               # fit_aci_id41_31[[2]][4,1],
                               # fit_aci_id41_31[[2]][4,2],
                               fit_aci_id41_31$RMSE,
                               fit_aci_id41_31$Ci_transition,
                               fit_aci_id41_31$citransition,
                               fit_aci_id41_31$Km,
                               fit_aci_id41_31$GammaStar,
                               fit_aci_id41_31$fitmethod,
                               fit_aci_id41_31$Tcorrect,
                               fit_aci_id41_31$fitTPU)
colnames(aci_data_id41_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id41_31_data)

################################################################################
### plant id42 tleaf20
aci_data_id42_20 = subset(aci_data, id == 42 & set_leafT == 20)
aci_data_id42_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id42_20)
#### fit aci curve
fit_aci_id42_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id42_20)
summary(fit_aci_id42_20)
coef_id42_20 <- coef(fit_aci_id42_20)
#### plot
plot(fit_aci_id42_20)
#### add to dataframe
aci_data_id42_20_data <- cbind(aci_data_id42_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id42_20[,30]),
                               mean(aci_data_id42_20[,72]),
                               fit_aci_id42_20[[2]][1,1],
                               fit_aci_id42_20[[2]][1,2],
                               fit_aci_id42_20[[2]][2,1],
                               fit_aci_id42_20[[2]][2,2],
                               fit_aci_id42_20[[2]][3,1],
                               fit_aci_id42_20[[2]][3,2],
                               # fit_aci_id42_20[[2]][4,1],
                               # fit_aci_id42_20[[2]][4,2],
                               fit_aci_id42_20$RMSE,
                               fit_aci_id42_20$Ci_transition,
                               fit_aci_id42_20$citransition,
                               fit_aci_id42_20$Km,
                               fit_aci_id42_20$GammaStar,
                               fit_aci_id42_20$fitmethod,
                               fit_aci_id42_20$Tcorrect,
                               fit_aci_id42_20$fitTPU)
colnames(aci_data_id42_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id42_20_data)

################################################################################
### plant id42 tleaf25
aci_data_id42_25 = subset(aci_data, id == 42 & set_leafT == 25)
aci_data_id42_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id42_25)
#### fit aci curve
fit_aci_id42_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id42_25)
summary(fit_aci_id42_25)
coef_id42_25 <- coef(fit_aci_id42_25)
#### plot
plot(fit_aci_id42_25)
#### add to dataframe
aci_data_id42_25_data <- cbind(aci_data_id42_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id42_25[,30]),
                               mean(aci_data_id42_25[,72]),
                               fit_aci_id42_25[[2]][1,1],
                               fit_aci_id42_25[[2]][1,2],
                               fit_aci_id42_25[[2]][2,1],
                               fit_aci_id42_25[[2]][2,2],
                               fit_aci_id42_25[[2]][3,1],
                               fit_aci_id42_25[[2]][3,2],
                               # fit_aci_id42_25[[2]][4,1],
                               # fit_aci_id42_25[[2]][4,2],
                               fit_aci_id42_25$RMSE,
                               fit_aci_id42_25$Ci_transition,
                               fit_aci_id42_25$citransition,
                               fit_aci_id42_25$Km,
                               fit_aci_id42_25$GammaStar,
                               fit_aci_id42_25$fitmethod,
                               fit_aci_id42_25$Tcorrect,
                               fit_aci_id42_25$fitTPU)
colnames(aci_data_id42_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id42_25_data)

################################################################################
### plant id42 tleaf31
aci_data_id42_31 = subset(aci_data, id == 42 & set_leafT == 31)
aci_data_id42_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id42_31)
#### fit aci curve
fit_aci_id42_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id42_31)
summary(fit_aci_id42_31)
coef_id42_31 <- coef(fit_aci_id42_31)
#### plot
plot(fit_aci_id42_31)
#### add to dataframe
aci_data_id42_31_data <- cbind(aci_data_id42_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id42_31[,30]),
                               mean(aci_data_id42_31[,72]),
                               fit_aci_id42_31[[2]][1,1],
                               fit_aci_id42_31[[2]][1,2],
                               fit_aci_id42_31[[2]][2,1],
                               fit_aci_id42_31[[2]][2,2],
                               fit_aci_id42_31[[2]][3,1],
                               fit_aci_id42_31[[2]][3,2],
                               # fit_aci_id42_31[[2]][4,1],
                               # fit_aci_id42_31[[2]][4,2],
                               fit_aci_id42_31$RMSE,
                               fit_aci_id42_31$Ci_transition,
                               fit_aci_id42_31$citransition,
                               fit_aci_id42_31$Km,
                               fit_aci_id42_31$GammaStar,
                               fit_aci_id42_31$fitmethod,
                               fit_aci_id42_31$Tcorrect,
                               fit_aci_id42_31$fitTPU)
colnames(aci_data_id42_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id42_31_data)

################################################################################
### plant id43 tleaf20
aci_data_id43_20 = subset(aci_data, id == 43 & set_leafT == 20)
aci_data_id43_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id43_20)
#### fit aci curve
fit_aci_id43_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id43_20)
summary(fit_aci_id43_20)
coef_id43_20 <- coef(fit_aci_id43_20)
#### plot
plot(fit_aci_id43_20)
#### add to dataframe
aci_data_id43_20_data <- cbind(aci_data_id43_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id43_20[,30]),
                               mean(aci_data_id43_20[,72]),
                               fit_aci_id43_20[[2]][1,1],
                               fit_aci_id43_20[[2]][1,2],
                               fit_aci_id43_20[[2]][2,1],
                               fit_aci_id43_20[[2]][2,2],
                               fit_aci_id43_20[[2]][3,1],
                               fit_aci_id43_20[[2]][3,2],
                               # fit_aci_id43_20[[2]][4,1],
                               # fit_aci_id43_20[[2]][4,2],
                               fit_aci_id43_20$RMSE,
                               fit_aci_id43_20$Ci_transition,
                               fit_aci_id43_20$citransition,
                               fit_aci_id43_20$Km,
                               fit_aci_id43_20$GammaStar,
                               fit_aci_id43_20$fitmethod,
                               fit_aci_id43_20$Tcorrect,
                               fit_aci_id43_20$fitTPU)
colnames(aci_data_id43_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id43_20_data)

################################################################################
### plant id43 tleaf25
aci_data_id43_25 = subset(aci_data, id == 43 & set_leafT == 25)
aci_data_id43_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id43_25)
#### fit aci curve
fit_aci_id43_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id43_25)
summary(fit_aci_id43_25)
coef_id43_25 <- coef(fit_aci_id43_25)
#### plot
plot(fit_aci_id43_25)
#### add to dataframe
aci_data_id43_25_data <- cbind(aci_data_id43_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id43_25[,30]),
                               mean(aci_data_id43_25[,72]),
                               fit_aci_id43_25[[2]][1,1],
                               fit_aci_id43_25[[2]][1,2],
                               fit_aci_id43_25[[2]][2,1],
                               fit_aci_id43_25[[2]][2,2],
                               fit_aci_id43_25[[2]][3,1],
                               fit_aci_id43_25[[2]][3,2],
                               # fit_aci_id43_25[[2]][4,1],
                               # fit_aci_id43_25[[2]][4,2],
                               fit_aci_id43_25$RMSE,
                               fit_aci_id43_25$Ci_transition,
                               fit_aci_id43_25$citransition,
                               fit_aci_id43_25$Km,
                               fit_aci_id43_25$GammaStar,
                               fit_aci_id43_25$fitmethod,
                               fit_aci_id43_25$Tcorrect,
                               fit_aci_id43_25$fitTPU)
colnames(aci_data_id43_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id43_25_data)

################################################################################
### plant id43 tleaf31
aci_data_id43_31 = subset(aci_data, id == 43 & set_leafT == 31)
aci_data_id43_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id43_31)
#### fit aci curve
fit_aci_id43_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id43_31)
summary(fit_aci_id43_31)
coef_id43_31 <- coef(fit_aci_id43_31)
#### plot
plot(fit_aci_id43_31)
#### add to dataframe
aci_data_id43_31_data <- cbind(aci_data_id43_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id43_31[,30]),
                               mean(aci_data_id43_31[,72]),
                               fit_aci_id43_31[[2]][1,1],
                               fit_aci_id43_31[[2]][1,2],
                               fit_aci_id43_31[[2]][2,1],
                               fit_aci_id43_31[[2]][2,2],
                               fit_aci_id43_31[[2]][3,1],
                               fit_aci_id43_31[[2]][3,2],
                               # fit_aci_id43_31[[2]][4,1],
                               # fit_aci_id43_31[[2]][4,2],
                               fit_aci_id43_31$RMSE,
                               fit_aci_id43_31$Ci_transition,
                               fit_aci_id43_31$citransition,
                               fit_aci_id43_31$Km,
                               fit_aci_id43_31$GammaStar,
                               fit_aci_id43_31$fitmethod,
                               fit_aci_id43_31$Tcorrect,
                               fit_aci_id43_31$fitTPU)
colnames(aci_data_id43_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id43_31_data)

################################################################################
### plant id44 tleaf20
aci_data_id44_20 = subset(aci_data, id == 44 & set_leafT == 20)
aci_data_id44_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id44_20)
#### fit aci curve
fit_aci_id44_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 400,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id44_20)
summary(fit_aci_id44_20)
coef_id44_20 <- coef(fit_aci_id44_20)
#### plot
plot(fit_aci_id44_20)
#### add to dataframe
aci_data_id44_20_data <- cbind(aci_data_id44_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id44_20[,30]),
                               mean(aci_data_id44_20[,72]),
                               fit_aci_id44_20[[2]][1,1],
                               fit_aci_id44_20[[2]][1,2],
                               fit_aci_id44_20[[2]][2,1],
                               fit_aci_id44_20[[2]][2,2],
                               fit_aci_id44_20[[2]][3,1],
                               fit_aci_id44_20[[2]][3,2],
                               # fit_aci_id44_20[[2]][4,1],
                               # fit_aci_id44_20[[2]][4,2],
                               fit_aci_id44_20$RMSE,
                               fit_aci_id44_20$Ci_transition,
                               fit_aci_id44_20$citransition,
                               fit_aci_id44_20$Km,
                               fit_aci_id44_20$GammaStar,
                               fit_aci_id44_20$fitmethod,
                               fit_aci_id44_20$Tcorrect,
                               fit_aci_id44_20$fitTPU)
colnames(aci_data_id44_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id44_20_data)

################################################################################
### plant id44 tleaf25
aci_data_id44_25 = subset(aci_data, id == 44 & set_leafT == 25)
aci_data_id44_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id44_25)
#### fit aci curve
fit_aci_id44_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 400,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id44_25)
summary(fit_aci_id44_25)
coef_id44_25 <- coef(fit_aci_id44_25)
#### plot
plot(fit_aci_id44_25)
#### add to dataframe
aci_data_id44_25_data <- cbind(aci_data_id44_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id44_25[,30]),
                               mean(aci_data_id44_25[,72]),
                               fit_aci_id44_25[[2]][1,1],
                               fit_aci_id44_25[[2]][1,2],
                               fit_aci_id44_25[[2]][2,1],
                               fit_aci_id44_25[[2]][2,2],
                               fit_aci_id44_25[[2]][3,1],
                               fit_aci_id44_25[[2]][3,2],
                               # fit_aci_id44_25[[2]][4,1],
                               # fit_aci_id44_25[[2]][4,2],
                               fit_aci_id44_25$RMSE,
                               fit_aci_id44_25$Ci_transition,
                               fit_aci_id44_25$citransition,
                               fit_aci_id44_25$Km,
                               fit_aci_id44_25$GammaStar,
                               fit_aci_id44_25$fitmethod,
                               fit_aci_id44_25$Tcorrect,
                               fit_aci_id44_25$fitTPU)
colnames(aci_data_id44_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id44_25_data)

################################################################################
### plant id44 tleaf31
aci_data_id44_31 = subset(aci_data, id == 44 & set_leafT == 31)
aci_data_id44_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id44_31)
#### fit aci curve
fit_aci_id44_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 400,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id44_31)
summary(fit_aci_id44_31)
coef_id44_31 <- coef(fit_aci_id44_31)
#### plot
plot(fit_aci_id44_31)
#### add to dataframe
aci_data_id44_31_data <- cbind(aci_data_id44_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id44_31[,30]),
                               mean(aci_data_id44_31[,72]),
                               fit_aci_id44_31[[2]][1,1],
                               fit_aci_id44_31[[2]][1,2],
                               fit_aci_id44_31[[2]][2,1],
                               fit_aci_id44_31[[2]][2,2],
                               fit_aci_id44_31[[2]][3,1],
                               fit_aci_id44_31[[2]][3,2],
                               # fit_aci_id44_31[[2]][4,1],
                               # fit_aci_id44_31[[2]][4,2],
                               fit_aci_id44_31$RMSE,
                               fit_aci_id44_31$Ci_transition,
                               fit_aci_id44_31$citransition,
                               fit_aci_id44_31$Km,
                               fit_aci_id44_31$GammaStar,
                               fit_aci_id44_31$fitmethod,
                               fit_aci_id44_31$Tcorrect,
                               fit_aci_id44_31$fitTPU)
colnames(aci_data_id44_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id44_31_data)

################################################################################
### plant id45 tleaf20
aci_data_id45_20 = subset(aci_data, id == 45 & set_leafT == 20)
aci_data_id45_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id45_20)
#### fit aci curve
fit_aci_id45_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id45_20)
summary(fit_aci_id45_20)
coef_id45_20 <- coef(fit_aci_id45_20)
#### plot
plot(fit_aci_id45_20)
#### add to dataframe
aci_data_id45_20_data <- cbind(aci_data_id45_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id45_20[,30]),
                               mean(aci_data_id45_20[,72]),
                               fit_aci_id45_20[[2]][1,1],
                               fit_aci_id45_20[[2]][1,2],
                               fit_aci_id45_20[[2]][2,1],
                               fit_aci_id45_20[[2]][2,2],
                               fit_aci_id45_20[[2]][3,1],
                               fit_aci_id45_20[[2]][3,2],
                               # fit_aci_id45_20[[2]][4,1],
                               # fit_aci_id45_20[[2]][4,2],
                               fit_aci_id45_20$RMSE,
                               fit_aci_id45_20$Ci_transition,
                               fit_aci_id45_20$citransition,
                               fit_aci_id45_20$Km,
                               fit_aci_id45_20$GammaStar,
                               fit_aci_id45_20$fitmethod,
                               fit_aci_id45_20$Tcorrect,
                               fit_aci_id45_20$fitTPU)
colnames(aci_data_id45_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id45_20_data)

################################################################################
### plant id45 tleaf25
aci_data_id45_25 = subset(aci_data, id == 45 & set_leafT == 25)
aci_data_id45_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id45_25)
#### fit aci curve
fit_aci_id45_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id45_25)
summary(fit_aci_id45_25)
coef_id45_25 <- coef(fit_aci_id45_25)
#### plot
plot(fit_aci_id45_25)
#### add to dataframe
aci_data_id45_25_data <- cbind(aci_data_id45_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id45_25[,30]),
                               mean(aci_data_id45_25[,72]),
                               fit_aci_id45_25[[2]][1,1],
                               fit_aci_id45_25[[2]][1,2],
                               fit_aci_id45_25[[2]][2,1],
                               fit_aci_id45_25[[2]][2,2],
                               fit_aci_id45_25[[2]][3,1],
                               fit_aci_id45_25[[2]][3,2],
                               # fit_aci_id45_25[[2]][4,1],
                               # fit_aci_id45_25[[2]][4,2],
                               fit_aci_id45_25$RMSE,
                               fit_aci_id45_25$Ci_transition,
                               fit_aci_id45_25$citransition,
                               fit_aci_id45_25$Km,
                               fit_aci_id45_25$GammaStar,
                               fit_aci_id45_25$fitmethod,
                               fit_aci_id45_25$Tcorrect,
                               fit_aci_id45_25$fitTPU)
colnames(aci_data_id45_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id45_25_data)

################################################################################
### plant id45 tleaf31
aci_data_id45_31 = subset(aci_data, id == 45 & set_leafT == 31)
aci_data_id45_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id45_31)
#### fit aci curve
fit_aci_id45_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id45_31)
summary(fit_aci_id45_31)
coef_id45_31 <- coef(fit_aci_id45_31)
#### plot
plot(fit_aci_id45_31)
#### add to dataframe
aci_data_id45_31_data <- cbind(aci_data_id45_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id45_31[,30]),
                               mean(aci_data_id45_31[,72]),
                               fit_aci_id45_31[[2]][1,1],
                               fit_aci_id45_31[[2]][1,2],
                               fit_aci_id45_31[[2]][2,1],
                               fit_aci_id45_31[[2]][2,2],
                               fit_aci_id45_31[[2]][3,1],
                               fit_aci_id45_31[[2]][3,2],
                               # fit_aci_id45_31[[2]][4,1],
                               # fit_aci_id45_31[[2]][4,2],
                               fit_aci_id45_31$RMSE,
                               fit_aci_id45_31$Ci_transition,
                               fit_aci_id45_31$citransition,
                               fit_aci_id45_31$Km,
                               fit_aci_id45_31$GammaStar,
                               fit_aci_id45_31$fitmethod,
                               fit_aci_id45_31$Tcorrect,
                               fit_aci_id45_31$fitTPU)
colnames(aci_data_id45_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id45_31_data)

################################################################################
### plant id46 tleaf20
aci_data_id46_20 = subset(aci_data, id == 46 & set_leafT == 20)
aci_data_id46_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id46_20)
#### fit aci curve
fit_aci_id46_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id46_20)
summary(fit_aci_id46_20)
coef_id46_20 <- coef(fit_aci_id46_20)
#### plot
plot(fit_aci_id46_20)
#### add to dataframe
aci_data_id46_20_data <- cbind(aci_data_id46_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id46_20[,30]),
                               mean(aci_data_id46_20[,72]),
                               fit_aci_id46_20[[2]][1,1],
                               fit_aci_id46_20[[2]][1,2],
                               fit_aci_id46_20[[2]][2,1],
                               fit_aci_id46_20[[2]][2,2],
                               fit_aci_id46_20[[2]][3,1],
                               fit_aci_id46_20[[2]][3,2],
                               # fit_aci_id46_20[[2]][4,1],
                               # fit_aci_id46_20[[2]][4,2],
                               fit_aci_id46_20$RMSE,
                               fit_aci_id46_20$Ci_transition,
                               fit_aci_id46_20$citransition,
                               fit_aci_id46_20$Km,
                               fit_aci_id46_20$GammaStar,
                               fit_aci_id46_20$fitmethod,
                               fit_aci_id46_20$Tcorrect,
                               fit_aci_id46_20$fitTPU)
colnames(aci_data_id46_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id46_20_data)

################################################################################
### plant id46 tleaf25
aci_data_id46_25 = subset(aci_data, id == 46 & set_leafT == 25)
aci_data_id46_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id46_25)
#### fit aci curve
fit_aci_id46_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id46_25)
summary(fit_aci_id46_25)
coef_id46_25 <- coef(fit_aci_id46_25)
#### plot
plot(fit_aci_id46_25)
#### add to dataframe
aci_data_id46_25_data <- cbind(aci_data_id46_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id46_25[,30]),
                               mean(aci_data_id46_25[,72]),
                               fit_aci_id46_25[[2]][1,1],
                               fit_aci_id46_25[[2]][1,2],
                               fit_aci_id46_25[[2]][2,1],
                               fit_aci_id46_25[[2]][2,2],
                               fit_aci_id46_25[[2]][3,1],
                               fit_aci_id46_25[[2]][3,2],
                               # fit_aci_id46_25[[2]][4,1],
                               # fit_aci_id46_25[[2]][4,2],
                               fit_aci_id46_25$RMSE,
                               fit_aci_id46_25$Ci_transition,
                               fit_aci_id46_25$citransition,
                               fit_aci_id46_25$Km,
                               fit_aci_id46_25$GammaStar,
                               fit_aci_id46_25$fitmethod,
                               fit_aci_id46_25$Tcorrect,
                               fit_aci_id46_25$fitTPU)
colnames(aci_data_id46_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id46_25_data)

################################################################################
### plant id46 tleaf31
aci_data_id46_31 = subset(aci_data, id == 46 & set_leafT == 31)
aci_data_id46_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id46_31)
#### fit aci curve
fit_aci_id46_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id46_31)
summary(fit_aci_id46_31)
coef_id46_31 <- coef(fit_aci_id46_31)
#### plot
plot(fit_aci_id46_31)
#### add to dataframe
aci_data_id46_31_data <- cbind(aci_data_id46_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id46_31[,30]),
                               mean(aci_data_id46_31[,72]),
                               fit_aci_id46_31[[2]][1,1],
                               fit_aci_id46_31[[2]][1,2],
                               fit_aci_id46_31[[2]][2,1],
                               fit_aci_id46_31[[2]][2,2],
                               fit_aci_id46_31[[2]][3,1],
                               fit_aci_id46_31[[2]][3,2],
                               # fit_aci_id46_31[[2]][4,1],
                               # fit_aci_id46_31[[2]][4,2],
                               fit_aci_id46_31$RMSE,
                               fit_aci_id46_31$Ci_transition,
                               fit_aci_id46_31$citransition,
                               fit_aci_id46_31$Km,
                               fit_aci_id46_31$GammaStar,
                               fit_aci_id46_31$fitmethod,
                               fit_aci_id46_31$Tcorrect,
                               fit_aci_id46_31$fitTPU)
colnames(aci_data_id46_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id46_31_data)

################################################################################
### plant id47 tleaf20
aci_data_id47_20 = subset(aci_data, id == 47 & set_leafT == 20)
aci_data_id47_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id47_20)
#### fit aci curve
fit_aci_id47_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id47_20)
summary(fit_aci_id47_20)
coef_id47_20 <- coef(fit_aci_id47_20)
#### plot
plot(fit_aci_id47_20)
#### add to dataframe
aci_data_id47_20_data <- cbind(aci_data_id47_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id47_20[,30]),
                               mean(aci_data_id47_20[,72]),
                               fit_aci_id47_20[[2]][1,1],
                               fit_aci_id47_20[[2]][1,2],
                               fit_aci_id47_20[[2]][2,1],
                               fit_aci_id47_20[[2]][2,2],
                               fit_aci_id47_20[[2]][3,1],
                               fit_aci_id47_20[[2]][3,2],
                               # fit_aci_id47_20[[2]][4,1],
                               # fit_aci_id47_20[[2]][4,2],
                               fit_aci_id47_20$RMSE,
                               fit_aci_id47_20$Ci_transition,
                               fit_aci_id47_20$citransition,
                               fit_aci_id47_20$Km,
                               fit_aci_id47_20$GammaStar,
                               fit_aci_id47_20$fitmethod,
                               fit_aci_id47_20$Tcorrect,
                               fit_aci_id47_20$fitTPU)
colnames(aci_data_id47_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id47_20_data)

################################################################################
### plant id47 tleaf25
aci_data_id47_25 = subset(aci_data, id == 47 & set_leafT == 25)
aci_data_id47_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id47_25)
#### fit aci curve
fit_aci_id47_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id47_25)
summary(fit_aci_id47_25)
coef_id47_25 <- coef(fit_aci_id47_25)
#### plot
plot(fit_aci_id47_25)
#### add to dataframe
aci_data_id47_25_data <- cbind(aci_data_id47_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id47_25[,30]),
                               mean(aci_data_id47_25[,72]),
                               fit_aci_id47_25[[2]][1,1],
                               fit_aci_id47_25[[2]][1,2],
                               fit_aci_id47_25[[2]][2,1],
                               fit_aci_id47_25[[2]][2,2],
                               fit_aci_id47_25[[2]][3,1],
                               fit_aci_id47_25[[2]][3,2],
                               # fit_aci_id47_25[[2]][4,1],
                               # fit_aci_id47_25[[2]][4,2],
                               fit_aci_id47_25$RMSE,
                               fit_aci_id47_25$Ci_transition,
                               fit_aci_id47_25$citransition,
                               fit_aci_id47_25$Km,
                               fit_aci_id47_25$GammaStar,
                               fit_aci_id47_25$fitmethod,
                               fit_aci_id47_25$Tcorrect,
                               fit_aci_id47_25$fitTPU)
colnames(aci_data_id47_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id47_25_data)

################################################################################
### plant id47 tleaf31
aci_data_id47_31 = subset(aci_data, id == 47 & set_leafT == 31)
aci_data_id47_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id47_31)
#### fit aci curve
fit_aci_id47_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id47_31)
summary(fit_aci_id47_31)
coef_id47_31 <- coef(fit_aci_id47_31)
#### plot
plot(fit_aci_id47_31)
#### add to dataframe
aci_data_id47_31_data <- cbind(aci_data_id47_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id47_31[,30]),
                               mean(aci_data_id47_31[,72]),
                               fit_aci_id47_31[[2]][1,1],
                               fit_aci_id47_31[[2]][1,2],
                               fit_aci_id47_31[[2]][2,1],
                               fit_aci_id47_31[[2]][2,2],
                               fit_aci_id47_31[[2]][3,1],
                               fit_aci_id47_31[[2]][3,2],
                               # fit_aci_id47_31[[2]][4,1],
                               # fit_aci_id47_31[[2]][4,2],
                               fit_aci_id47_31$RMSE,
                               fit_aci_id47_31$Ci_transition,
                               fit_aci_id47_31$citransition,
                               fit_aci_id47_31$Km,
                               fit_aci_id47_31$GammaStar,
                               fit_aci_id47_31$fitmethod,
                               fit_aci_id47_31$Tcorrect,
                               fit_aci_id47_31$fitTPU)
colnames(aci_data_id47_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id47_31_data)

################################################################################
### plant id48 tleaf20
aci_data_id48_20 = subset(aci_data, id == 48 & set_leafT == 20)
aci_data_id48_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id48_20)
#### fit aci curve
fit_aci_id48_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id48_20)
summary(fit_aci_id48_20)
coef_id48_20 <- coef(fit_aci_id48_20)
#### plot
plot(fit_aci_id48_20)
#### add to dataframe
aci_data_id48_20_data <- cbind(aci_data_id48_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id48_20[,30]),
                               mean(aci_data_id48_20[,72]),
                               fit_aci_id48_20[[2]][1,1],
                               fit_aci_id48_20[[2]][1,2],
                               fit_aci_id48_20[[2]][2,1],
                               fit_aci_id48_20[[2]][2,2],
                               fit_aci_id48_20[[2]][3,1],
                               fit_aci_id48_20[[2]][3,2],
                               # fit_aci_id48_20[[2]][4,1],
                               # fit_aci_id48_20[[2]][4,2],
                               fit_aci_id48_20$RMSE,
                               fit_aci_id48_20$Ci_transition,
                               fit_aci_id48_20$citransition,
                               fit_aci_id48_20$Km,
                               fit_aci_id48_20$GammaStar,
                               fit_aci_id48_20$fitmethod,
                               fit_aci_id48_20$Tcorrect,
                               fit_aci_id48_20$fitTPU)
colnames(aci_data_id48_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id48_20_data)

################################################################################
### plant id48 tleaf25
aci_data_id48_25 = subset(aci_data, id == 48 & set_leafT == 25)
aci_data_id48_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id48_25)
#### fit aci curve
fit_aci_id48_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id48_25)
summary(fit_aci_id48_25)
coef_id48_25 <- coef(fit_aci_id48_25)
#### plot
plot(fit_aci_id48_25)
#### add to dataframe
aci_data_id48_25_data <- cbind(aci_data_id48_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id48_25[,30]),
                               mean(aci_data_id48_25[,72]),
                               fit_aci_id48_25[[2]][1,1],
                               fit_aci_id48_25[[2]][1,2],
                               fit_aci_id48_25[[2]][2,1],
                               fit_aci_id48_25[[2]][2,2],
                               fit_aci_id48_25[[2]][3,1],
                               fit_aci_id48_25[[2]][3,2],
                               # fit_aci_id48_25[[2]][4,1],
                               # fit_aci_id48_25[[2]][4,2],
                               fit_aci_id48_25$RMSE,
                               fit_aci_id48_25$Ci_transition,
                               fit_aci_id48_25$citransition,
                               fit_aci_id48_25$Km,
                               fit_aci_id48_25$GammaStar,
                               fit_aci_id48_25$fitmethod,
                               fit_aci_id48_25$Tcorrect,
                               fit_aci_id48_25$fitTPU)
colnames(aci_data_id48_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id48_25_data)

################################################################################
### plant id48 tleaf31
aci_data_id48_31 = subset(aci_data, id == 48 & set_leafT == 31)
aci_data_id48_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id48_31)
#### fit aci curve
fit_aci_id48_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id48_31)
summary(fit_aci_id48_31)
coef_id48_31 <- coef(fit_aci_id48_31)
#### plot
plot(fit_aci_id48_31)
#### add to dataframe
aci_data_id48_31_data <- cbind(aci_data_id48_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id48_31[,30]),
                               mean(aci_data_id48_31[,72]),
                               fit_aci_id48_31[[2]][1,1],
                               fit_aci_id48_31[[2]][1,2],
                               fit_aci_id48_31[[2]][2,1],
                               fit_aci_id48_31[[2]][2,2],
                               fit_aci_id48_31[[2]][3,1],
                               fit_aci_id48_31[[2]][3,2],
                               # fit_aci_id48_31[[2]][4,1],
                               # fit_aci_id48_31[[2]][4,2],
                               fit_aci_id48_31$RMSE,
                               fit_aci_id48_31$Ci_transition,
                               fit_aci_id48_31$citransition,
                               fit_aci_id48_31$Km,
                               fit_aci_id48_31$GammaStar,
                               fit_aci_id48_31$fitmethod,
                               fit_aci_id48_31$Tcorrect,
                               fit_aci_id48_31$fitTPU)
colnames(aci_data_id48_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id48_31_data)

################################################################################
### plant id49 tleaf20
aci_data_id49_20 = subset(aci_data, id == 49 & set_leafT == 20)
aci_data_id49_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id49_20)
#### fit aci curve
fit_aci_id49_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id49_20)
summary(fit_aci_id49_20)
coef_id49_20 <- coef(fit_aci_id49_20)
#### plot
plot(fit_aci_id49_20)
#### add to dataframe
aci_data_id49_20_data <- cbind(aci_data_id49_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id49_20[,30]),
                               mean(aci_data_id49_20[,72]),
                               fit_aci_id49_20[[2]][1,1],
                               fit_aci_id49_20[[2]][1,2],
                               fit_aci_id49_20[[2]][2,1],
                               fit_aci_id49_20[[2]][2,2],
                               fit_aci_id49_20[[2]][3,1],
                               fit_aci_id49_20[[2]][3,2],
                               # fit_aci_id49_20[[2]][4,1],
                               # fit_aci_id49_20[[2]][4,2],
                               fit_aci_id49_20$RMSE,
                               fit_aci_id49_20$Ci_transition,
                               fit_aci_id49_20$citransition,
                               fit_aci_id49_20$Km,
                               fit_aci_id49_20$GammaStar,
                               fit_aci_id49_20$fitmethod,
                               fit_aci_id49_20$Tcorrect,
                               fit_aci_id49_20$fitTPU)
colnames(aci_data_id49_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id49_20_data)

################################################################################
### plant id49 tleaf25
aci_data_id49_25 = subset(aci_data, id == 49 & set_leafT == 25)
aci_data_id49_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id49_25)
#### fit aci curve
fit_aci_id49_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id49_25)
summary(fit_aci_id49_25)
coef_id49_25 <- coef(fit_aci_id49_25)
#### plot
plot(fit_aci_id49_25)
#### add to dataframe
aci_data_id49_25_data <- cbind(aci_data_id49_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id49_25[,30]),
                               mean(aci_data_id49_25[,72]),
                               fit_aci_id49_25[[2]][1,1],
                               fit_aci_id49_25[[2]][1,2],
                               fit_aci_id49_25[[2]][2,1],
                               fit_aci_id49_25[[2]][2,2],
                               fit_aci_id49_25[[2]][3,1],
                               fit_aci_id49_25[[2]][3,2],
                               # fit_aci_id49_25[[2]][4,1],
                               # fit_aci_id49_25[[2]][4,2],
                               fit_aci_id49_25$RMSE,
                               fit_aci_id49_25$Ci_transition,
                               fit_aci_id49_25$citransition,
                               fit_aci_id49_25$Km,
                               fit_aci_id49_25$GammaStar,
                               fit_aci_id49_25$fitmethod,
                               fit_aci_id49_25$Tcorrect,
                               fit_aci_id49_25$fitTPU)
colnames(aci_data_id49_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id49_25_data)

################################################################################
### plant id49 tleaf31
aci_data_id49_31 = subset(aci_data, id == 49 & set_leafT == 31)
aci_data_id49_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id49_31)
#### fit aci curve
fit_aci_id49_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id49_31)
summary(fit_aci_id49_31)
coef_id49_31 <- coef(fit_aci_id49_31)
#### plot
plot(fit_aci_id49_31)
#### add to dataframe
aci_data_id49_31_data <- cbind(aci_data_id49_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id49_31[,30]),
                               mean(aci_data_id49_31[,72]),
                               fit_aci_id49_31[[2]][1,1],
                               fit_aci_id49_31[[2]][1,2],
                               fit_aci_id49_31[[2]][2,1],
                               fit_aci_id49_31[[2]][2,2],
                               fit_aci_id49_31[[2]][3,1],
                               fit_aci_id49_31[[2]][3,2],
                               # fit_aci_id49_31[[2]][4,1],
                               # fit_aci_id49_31[[2]][4,2],
                               fit_aci_id49_31$RMSE,
                               fit_aci_id49_31$Ci_transition,
                               fit_aci_id49_31$citransition,
                               fit_aci_id49_31$Km,
                               fit_aci_id49_31$GammaStar,
                               fit_aci_id49_31$fitmethod,
                               fit_aci_id49_31$Tcorrect,
                               fit_aci_id49_31$fitTPU)
colnames(aci_data_id49_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id49_31_data)

################################################################################
### plant id50 tleaf20
aci_data_id50_20 = subset(aci_data, id == 50 & set_leafT == 20)
aci_data_id50_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id50_20)
#### fit aci curve
fit_aci_id50_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id50_20)
summary(fit_aci_id50_20)
coef_id50_20 <- coef(fit_aci_id50_20)
#### plot
plot(fit_aci_id50_20)
#### add to dataframe
aci_data_id50_20_data <- cbind(aci_data_id50_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id50_20[,30]),
                               mean(aci_data_id50_20[,72]),
                               fit_aci_id50_20[[2]][1,1],
                               fit_aci_id50_20[[2]][1,2],
                               fit_aci_id50_20[[2]][2,1],
                               fit_aci_id50_20[[2]][2,2],
                               fit_aci_id50_20[[2]][3,1],
                               fit_aci_id50_20[[2]][3,2],
                               # fit_aci_id50_20[[2]][4,1],
                               # fit_aci_id50_20[[2]][4,2],
                               fit_aci_id50_20$RMSE,
                               fit_aci_id50_20$Ci_transition,
                               fit_aci_id50_20$citransition,
                               fit_aci_id50_20$Km,
                               fit_aci_id50_20$GammaStar,
                               fit_aci_id50_20$fitmethod,
                               fit_aci_id50_20$Tcorrect,
                               fit_aci_id50_20$fitTPU)
colnames(aci_data_id50_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id50_20_data)

################################################################################
### plant id50 tleaf25
aci_data_id50_25 = subset(aci_data, id == 50 & set_leafT == 25)
aci_data_id50_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id50_25)
#### fit aci curve
fit_aci_id50_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id50_25)
summary(fit_aci_id50_25)
coef_id50_25 <- coef(fit_aci_id50_25)
#### plot
plot(fit_aci_id50_25)
#### add to dataframe
aci_data_id50_25_data <- cbind(aci_data_id50_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id50_25[,30]),
                               mean(aci_data_id50_25[,72]),
                               fit_aci_id50_25[[2]][1,1],
                               fit_aci_id50_25[[2]][1,2],
                               fit_aci_id50_25[[2]][2,1],
                               fit_aci_id50_25[[2]][2,2],
                               fit_aci_id50_25[[2]][3,1],
                               fit_aci_id50_25[[2]][3,2],
                               # fit_aci_id50_25[[2]][4,1],
                               # fit_aci_id50_25[[2]][4,2],
                               fit_aci_id50_25$RMSE,
                               fit_aci_id50_25$Ci_transition,
                               fit_aci_id50_25$citransition,
                               fit_aci_id50_25$Km,
                               fit_aci_id50_25$GammaStar,
                               fit_aci_id50_25$fitmethod,
                               fit_aci_id50_25$Tcorrect,
                               fit_aci_id50_25$fitTPU)
colnames(aci_data_id50_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id50_25_data)

################################################################################
### plant id50 tleaf31
aci_data_id50_31 = subset(aci_data, id == 50 & set_leafT == 31)
aci_data_id50_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id50_31)
#### fit aci curve
fit_aci_id50_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id50_31)
summary(fit_aci_id50_31)
coef_id50_31 <- coef(fit_aci_id50_31)
#### plot
plot(fit_aci_id50_31)
#### add to dataframe
aci_data_id50_31_data <- cbind(aci_data_id50_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id50_31[,30]),
                               mean(aci_data_id50_31[,72]),
                               fit_aci_id50_31[[2]][1,1],
                               fit_aci_id50_31[[2]][1,2],
                               fit_aci_id50_31[[2]][2,1],
                               fit_aci_id50_31[[2]][2,2],
                               fit_aci_id50_31[[2]][3,1],
                               fit_aci_id50_31[[2]][3,2],
                               # fit_aci_id50_31[[2]][4,1],
                               # fit_aci_id50_31[[2]][4,2],
                               fit_aci_id50_31$RMSE,
                               fit_aci_id50_31$Ci_transition,
                               fit_aci_id50_31$citransition,
                               fit_aci_id50_31$Km,
                               fit_aci_id50_31$GammaStar,
                               fit_aci_id50_31$fitmethod,
                               fit_aci_id50_31$Tcorrect,
                               fit_aci_id50_31$fitTPU)
colnames(aci_data_id50_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id50_31_data)

################################################################################
### plant id51 tleaf20
aci_data_id51_20 = subset(aci_data, id == 51 & set_leafT == 20)
aci_data_id51_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id51_20)
#### fit aci curve
fit_aci_id51_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id51_20)
summary(fit_aci_id51_20)
coef_id51_20 <- coef(fit_aci_id51_20)
#### plot
plot(fit_aci_id51_20)
#### add to dataframe
aci_data_id51_20_data <- cbind(aci_data_id51_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id51_20[,30]),
                               mean(aci_data_id51_20[,72]),
                               fit_aci_id51_20[[2]][1,1],
                               fit_aci_id51_20[[2]][1,2],
                               fit_aci_id51_20[[2]][2,1],
                               fit_aci_id51_20[[2]][2,2],
                               fit_aci_id51_20[[2]][3,1],
                               fit_aci_id51_20[[2]][3,2],
                               # fit_aci_id51_20[[2]][4,1],
                               # fit_aci_id51_20[[2]][4,2],
                               fit_aci_id51_20$RMSE,
                               fit_aci_id51_20$Ci_transition,
                               fit_aci_id51_20$citransition,
                               fit_aci_id51_20$Km,
                               fit_aci_id51_20$GammaStar,
                               fit_aci_id51_20$fitmethod,
                               fit_aci_id51_20$Tcorrect,
                               fit_aci_id51_20$fitTPU)
colnames(aci_data_id51_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id51_20_data)

################################################################################
### plant id51 tleaf25
aci_data_id51_25 = subset(aci_data, id == 51 & set_leafT == 25)
aci_data_id51_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id51_25)
#### fit aci curve
fit_aci_id51_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id51_25)
summary(fit_aci_id51_25)
coef_id51_25 <- coef(fit_aci_id51_25)
#### plot
plot(fit_aci_id51_25)
#### add to dataframe
aci_data_id51_25_data <- cbind(aci_data_id51_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id51_25[,30]),
                               mean(aci_data_id51_25[,72]),
                               fit_aci_id51_25[[2]][1,1],
                               fit_aci_id51_25[[2]][1,2],
                               fit_aci_id51_25[[2]][2,1],
                               fit_aci_id51_25[[2]][2,2],
                               fit_aci_id51_25[[2]][3,1],
                               fit_aci_id51_25[[2]][3,2],
                               # fit_aci_id51_25[[2]][4,1],
                               # fit_aci_id51_25[[2]][4,2],
                               fit_aci_id51_25$RMSE,
                               fit_aci_id51_25$Ci_transition,
                               fit_aci_id51_25$citransition,
                               fit_aci_id51_25$Km,
                               fit_aci_id51_25$GammaStar,
                               fit_aci_id51_25$fitmethod,
                               fit_aci_id51_25$Tcorrect,
                               fit_aci_id51_25$fitTPU)
colnames(aci_data_id51_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id51_25_data)

################################################################################
### plant id51 tleaf31
aci_data_id51_31 = subset(aci_data, id == 51 & set_leafT == 31)
aci_data_id51_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id51_31)
#### fit aci curve
fit_aci_id51_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id51_31)
summary(fit_aci_id51_31)
coef_id51_31 <- coef(fit_aci_id51_31)
#### plot
plot(fit_aci_id51_31)
#### add to dataframe
aci_data_id51_31_data <- cbind(aci_data_id51_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id51_31[,30]),
                               mean(aci_data_id51_31[,72]),
                               fit_aci_id51_31[[2]][1,1],
                               fit_aci_id51_31[[2]][1,2],
                               fit_aci_id51_31[[2]][2,1],
                               fit_aci_id51_31[[2]][2,2],
                               fit_aci_id51_31[[2]][3,1],
                               fit_aci_id51_31[[2]][3,2],
                               # fit_aci_id51_31[[2]][4,1],
                               # fit_aci_id51_31[[2]][4,2],
                               fit_aci_id51_31$RMSE,
                               fit_aci_id51_31$Ci_transition,
                               fit_aci_id51_31$citransition,
                               fit_aci_id51_31$Km,
                               fit_aci_id51_31$GammaStar,
                               fit_aci_id51_31$fitmethod,
                               fit_aci_id51_31$Tcorrect,
                               fit_aci_id51_31$fitTPU)
colnames(aci_data_id51_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id51_31_data)

################################################################################
### plant id52 tleaf20
aci_data_id52_20 = subset(aci_data, id == 52 & set_leafT == 20)
aci_data_id52_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id52_20)
#### fit aci curve
fit_aci_id52_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id52_20)
summary(fit_aci_id52_20)
coef_id52_20 <- coef(fit_aci_id52_20)
#### plot
plot(fit_aci_id52_20)
#### add to dataframe
aci_data_id52_20_data <- cbind(aci_data_id52_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id52_20[,30]),
                               mean(aci_data_id52_20[,72]),
                               fit_aci_id52_20[[2]][1,1],
                               fit_aci_id52_20[[2]][1,2],
                               fit_aci_id52_20[[2]][2,1],
                               fit_aci_id52_20[[2]][2,2],
                               fit_aci_id52_20[[2]][3,1],
                               fit_aci_id52_20[[2]][3,2],
                               # fit_aci_id52_20[[2]][4,1],
                               # fit_aci_id52_20[[2]][4,2],
                               fit_aci_id52_20$RMSE,
                               fit_aci_id52_20$Ci_transition,
                               fit_aci_id52_20$citransition,
                               fit_aci_id52_20$Km,
                               fit_aci_id52_20$GammaStar,
                               fit_aci_id52_20$fitmethod,
                               fit_aci_id52_20$Tcorrect,
                               fit_aci_id52_20$fitTPU)
colnames(aci_data_id52_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id52_20_data)

################################################################################
### plant id52 tleaf25
aci_data_id52_25 = subset(aci_data, id == 52 & set_leafT == 25)
aci_data_id52_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id52_25)
#### fit aci curve
fit_aci_id52_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id52_25)
summary(fit_aci_id52_25)
coef_id52_25 <- coef(fit_aci_id52_25)
#### plot
plot(fit_aci_id52_25)
#### add to dataframe
aci_data_id52_25_data <- cbind(aci_data_id52_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id52_25[,30]),
                               mean(aci_data_id52_25[,72]),
                               fit_aci_id52_25[[2]][1,1],
                               fit_aci_id52_25[[2]][1,2],
                               fit_aci_id52_25[[2]][2,1],
                               fit_aci_id52_25[[2]][2,2],
                               fit_aci_id52_25[[2]][3,1],
                               fit_aci_id52_25[[2]][3,2],
                               # fit_aci_id52_25[[2]][4,1],
                               # fit_aci_id52_25[[2]][4,2],
                               fit_aci_id52_25$RMSE,
                               fit_aci_id52_25$Ci_transition,
                               fit_aci_id52_25$citransition,
                               fit_aci_id52_25$Km,
                               fit_aci_id52_25$GammaStar,
                               fit_aci_id52_25$fitmethod,
                               fit_aci_id52_25$Tcorrect,
                               fit_aci_id52_25$fitTPU)
colnames(aci_data_id52_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id52_25_data)

################################################################################
### plant id52 tleaf31
aci_data_id52_31 = subset(aci_data, id == 52 & set_leafT == 31)
aci_data_id52_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id52_31)
#### fit aci curve
fit_aci_id52_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id52_31)
summary(fit_aci_id52_31)
coef_id52_31 <- coef(fit_aci_id52_31)
#### plot
plot(fit_aci_id52_31)
#### add to dataframe
aci_data_id52_31_data <- cbind(aci_data_id52_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id52_31[,30]),
                               mean(aci_data_id52_31[,72]),
                               fit_aci_id52_31[[2]][1,1],
                               fit_aci_id52_31[[2]][1,2],
                               fit_aci_id52_31[[2]][2,1],
                               fit_aci_id52_31[[2]][2,2],
                               fit_aci_id52_31[[2]][3,1],
                               fit_aci_id52_31[[2]][3,2],
                               # fit_aci_id52_31[[2]][4,1],
                               # fit_aci_id52_31[[2]][4,2],
                               fit_aci_id52_31$RMSE,
                               fit_aci_id52_31$Ci_transition,
                               fit_aci_id52_31$citransition,
                               fit_aci_id52_31$Km,
                               fit_aci_id52_31$GammaStar,
                               fit_aci_id52_31$fitmethod,
                               fit_aci_id52_31$Tcorrect,
                               fit_aci_id52_31$fitTPU)
colnames(aci_data_id52_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id52_31_data)

################################################################################
### plant id53 tleaf20
aci_data_id53_20 = subset(aci_data, id == 53 & set_leafT == 20)
aci_data_id53_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id53_20)
#### fit aci curve
fit_aci_id53_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id53_20)
summary(fit_aci_id53_20)
coef_id53_20 <- coef(fit_aci_id53_20)
#### plot
plot(fit_aci_id53_20)
#### add to dataframe
aci_data_id53_20_data <- cbind(aci_data_id53_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id53_20[,30]),
                               mean(aci_data_id53_20[,72]),
                               fit_aci_id53_20[[2]][1,1],
                               fit_aci_id53_20[[2]][1,2],
                               fit_aci_id53_20[[2]][2,1],
                               fit_aci_id53_20[[2]][2,2],
                               fit_aci_id53_20[[2]][3,1],
                               fit_aci_id53_20[[2]][3,2],
                               # fit_aci_id53_20[[2]][4,1],
                               # fit_aci_id53_20[[2]][4,2],
                               fit_aci_id53_20$RMSE,
                               fit_aci_id53_20$Ci_transition,
                               fit_aci_id53_20$citransition,
                               fit_aci_id53_20$Km,
                               fit_aci_id53_20$GammaStar,
                               fit_aci_id53_20$fitmethod,
                               fit_aci_id53_20$Tcorrect,
                               fit_aci_id53_20$fitTPU)
colnames(aci_data_id53_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id53_20_data)

################################################################################
### plant id53 tleaf25
aci_data_id53_25 = subset(aci_data, id == 53 & set_leafT == 25)
aci_data_id53_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id53_25)
#### fit aci curve
fit_aci_id53_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id53_25)
summary(fit_aci_id53_25)
coef_id53_25 <- coef(fit_aci_id53_25)
#### plot
plot(fit_aci_id53_25)
#### add to dataframe
aci_data_id53_25_data <- cbind(aci_data_id53_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id53_25[,30]),
                               mean(aci_data_id53_25[,72]),
                               fit_aci_id53_25[[2]][1,1],
                               fit_aci_id53_25[[2]][1,2],
                               fit_aci_id53_25[[2]][2,1],
                               fit_aci_id53_25[[2]][2,2],
                               fit_aci_id53_25[[2]][3,1],
                               fit_aci_id53_25[[2]][3,2],
                               # fit_aci_id53_25[[2]][4,1],
                               # fit_aci_id53_25[[2]][4,2],
                               fit_aci_id53_25$RMSE,
                               fit_aci_id53_25$Ci_transition,
                               fit_aci_id53_25$citransition,
                               fit_aci_id53_25$Km,
                               fit_aci_id53_25$GammaStar,
                               fit_aci_id53_25$fitmethod,
                               fit_aci_id53_25$Tcorrect,
                               fit_aci_id53_25$fitTPU)
colnames(aci_data_id53_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id53_25_data)

################################################################################
### plant id53 tleaf31
aci_data_id53_31 = subset(aci_data, id == 53 & set_leafT == 31)
aci_data_id53_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id53_31)
#### fit aci curve
fit_aci_id53_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id53_31)
summary(fit_aci_id53_31)
coef_id53_31 <- coef(fit_aci_id53_31)
#### plot
plot(fit_aci_id53_31)
#### add to dataframe
aci_data_id53_31_data <- cbind(aci_data_id53_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id53_31[,30]),
                               mean(aci_data_id53_31[,72]),
                               fit_aci_id53_31[[2]][1,1],
                               fit_aci_id53_31[[2]][1,2],
                               fit_aci_id53_31[[2]][2,1],
                               fit_aci_id53_31[[2]][2,2],
                               fit_aci_id53_31[[2]][3,1],
                               fit_aci_id53_31[[2]][3,2],
                               # fit_aci_id53_31[[2]][4,1],
                               # fit_aci_id53_31[[2]][4,2],
                               fit_aci_id53_31$RMSE,
                               fit_aci_id53_31$Ci_transition,
                               fit_aci_id53_31$citransition,
                               fit_aci_id53_31$Km,
                               fit_aci_id53_31$GammaStar,
                               fit_aci_id53_31$fitmethod,
                               fit_aci_id53_31$Tcorrect,
                               fit_aci_id53_31$fitTPU)
colnames(aci_data_id53_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id53_31_data)

################################################################################
### plant id54 tleaf20
aci_data_id54_20 = subset(aci_data, id == 54 & set_leafT == 20)
aci_data_id54_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id54_20)
#### fit aci curve
fit_aci_id54_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id54_20)
summary(fit_aci_id54_20)
coef_id54_20 <- coef(fit_aci_id54_20)
#### plot
plot(fit_aci_id54_20)
#### add to dataframe
aci_data_id54_20_data <- cbind(aci_data_id54_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id54_20[,30]),
                               mean(aci_data_id54_20[,72]),
                               fit_aci_id54_20[[2]][1,1],
                               fit_aci_id54_20[[2]][1,2],
                               fit_aci_id54_20[[2]][2,1],
                               fit_aci_id54_20[[2]][2,2],
                               fit_aci_id54_20[[2]][3,1],
                               fit_aci_id54_20[[2]][3,2],
                               # fit_aci_id54_20[[2]][4,1],
                               # fit_aci_id54_20[[2]][4,2],
                               fit_aci_id54_20$RMSE,
                               fit_aci_id54_20$Ci_transition,
                               fit_aci_id54_20$citransition,
                               fit_aci_id54_20$Km,
                               fit_aci_id54_20$GammaStar,
                               fit_aci_id54_20$fitmethod,
                               fit_aci_id54_20$Tcorrect,
                               fit_aci_id54_20$fitTPU)
colnames(aci_data_id54_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id54_20_data)

################################################################################
### plant id54 tleaf25
aci_data_id54_25 = subset(aci_data, id == 54 & set_leafT == 25)
aci_data_id54_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id54_25)
#### fit aci curve
fit_aci_id54_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id54_25)
summary(fit_aci_id54_25)
coef_id54_25 <- coef(fit_aci_id54_25)
#### plot
plot(fit_aci_id54_25)
#### add to dataframe
aci_data_id54_25_data <- cbind(aci_data_id54_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id54_25[,30]),
                               mean(aci_data_id54_25[,72]),
                               fit_aci_id54_25[[2]][1,1],
                               fit_aci_id54_25[[2]][1,2],
                               fit_aci_id54_25[[2]][2,1],
                               fit_aci_id54_25[[2]][2,2],
                               fit_aci_id54_25[[2]][3,1],
                               fit_aci_id54_25[[2]][3,2],
                               # fit_aci_id54_25[[2]][4,1],
                               # fit_aci_id54_25[[2]][4,2],
                               fit_aci_id54_25$RMSE,
                               fit_aci_id54_25$Ci_transition,
                               fit_aci_id54_25$citransition,
                               fit_aci_id54_25$Km,
                               fit_aci_id54_25$GammaStar,
                               fit_aci_id54_25$fitmethod,
                               fit_aci_id54_25$Tcorrect,
                               fit_aci_id54_25$fitTPU)
colnames(aci_data_id54_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id54_25_data)

################################################################################
### plant id54 tleaf31
aci_data_id54_31 = subset(aci_data, id == 54 & set_leafT == 31)
aci_data_id54_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id54_31)
#### fit aci curve
fit_aci_id54_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id54_31)
summary(fit_aci_id54_31)
coef_id54_31 <- coef(fit_aci_id54_31)
#### plot
plot(fit_aci_id54_31)
#### add to dataframe
aci_data_id54_31_data <- cbind(aci_data_id54_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id54_31[,30]),
                               mean(aci_data_id54_31[,72]),
                               fit_aci_id54_31[[2]][1,1],
                               fit_aci_id54_31[[2]][1,2],
                               fit_aci_id54_31[[2]][2,1],
                               fit_aci_id54_31[[2]][2,2],
                               fit_aci_id54_31[[2]][3,1],
                               fit_aci_id54_31[[2]][3,2],
                               # fit_aci_id54_31[[2]][4,1],
                               # fit_aci_id54_31[[2]][4,2],
                               fit_aci_id54_31$RMSE,
                               fit_aci_id54_31$Ci_transition,
                               fit_aci_id54_31$citransition,
                               fit_aci_id54_31$Km,
                               fit_aci_id54_31$GammaStar,
                               fit_aci_id54_31$fitmethod,
                               fit_aci_id54_31$Tcorrect,
                               fit_aci_id54_31$fitTPU)
colnames(aci_data_id54_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id54_31_data)

################################################################################
### plant id55 tleaf20
aci_data_id55_20 = subset(aci_data, id == 55 & set_leafT == 20)
aci_data_id55_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id55_20)
#### fit aci curve
fit_aci_id55_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id55_20)
summary(fit_aci_id55_20)
coef_id55_20 <- coef(fit_aci_id55_20)
#### plot
plot(fit_aci_id55_20)
#### add to dataframe
aci_data_id55_20_data <- cbind(aci_data_id55_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id55_20[,30]),
                               mean(aci_data_id55_20[,72]),
                               fit_aci_id55_20[[2]][1,1],
                               fit_aci_id55_20[[2]][1,2],
                               fit_aci_id55_20[[2]][2,1],
                               fit_aci_id55_20[[2]][2,2],
                               fit_aci_id55_20[[2]][3,1],
                               fit_aci_id55_20[[2]][3,2],
                               # fit_aci_id55_20[[2]][4,1],
                               # fit_aci_id55_20[[2]][4,2],
                               fit_aci_id55_20$RMSE,
                               fit_aci_id55_20$Ci_transition,
                               fit_aci_id55_20$citransition,
                               fit_aci_id55_20$Km,
                               fit_aci_id55_20$GammaStar,
                               fit_aci_id55_20$fitmethod,
                               fit_aci_id55_20$Tcorrect,
                               fit_aci_id55_20$fitTPU)
colnames(aci_data_id55_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id55_20_data)

################################################################################
### plant id55 tleaf25
aci_data_id55_25 = subset(aci_data, id == 55 & set_leafT == 25)
aci_data_id55_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id55_25)
#### fit aci curve
fit_aci_id55_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id55_25)
summary(fit_aci_id55_25)
coef_id55_25 <- coef(fit_aci_id55_25)
#### plot
plot(fit_aci_id55_25)
#### add to dataframe
aci_data_id55_25_data <- cbind(aci_data_id55_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id55_25[,30]),
                               mean(aci_data_id55_25[,72]),
                               fit_aci_id55_25[[2]][1,1],
                               fit_aci_id55_25[[2]][1,2],
                               fit_aci_id55_25[[2]][2,1],
                               fit_aci_id55_25[[2]][2,2],
                               fit_aci_id55_25[[2]][3,1],
                               fit_aci_id55_25[[2]][3,2],
                               # fit_aci_id55_25[[2]][4,1],
                               # fit_aci_id55_25[[2]][4,2],
                               fit_aci_id55_25$RMSE,
                               fit_aci_id55_25$Ci_transition,
                               fit_aci_id55_25$citransition,
                               fit_aci_id55_25$Km,
                               fit_aci_id55_25$GammaStar,
                               fit_aci_id55_25$fitmethod,
                               fit_aci_id55_25$Tcorrect,
                               fit_aci_id55_25$fitTPU)
colnames(aci_data_id55_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id55_25_data)

################################################################################
### plant id55 tleaf31
aci_data_id55_31 = subset(aci_data, id == 55 & set_leafT == 31)
aci_data_id55_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id55_31)
#### fit aci curve
fit_aci_id55_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id55_31)
summary(fit_aci_id55_31)
coef_id55_31 <- coef(fit_aci_id55_31)
#### plot
plot(fit_aci_id55_31)
#### add to dataframe
aci_data_id55_31_data <- cbind(aci_data_id55_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id55_31[,30]),
                               mean(aci_data_id55_31[,72]),
                               fit_aci_id55_31[[2]][1,1],
                               fit_aci_id55_31[[2]][1,2],
                               fit_aci_id55_31[[2]][2,1],
                               fit_aci_id55_31[[2]][2,2],
                               fit_aci_id55_31[[2]][3,1],
                               fit_aci_id55_31[[2]][3,2],
                               # fit_aci_id55_31[[2]][4,1],
                               # fit_aci_id55_31[[2]][4,2],
                               fit_aci_id55_31$RMSE,
                               fit_aci_id55_31$Ci_transition,
                               fit_aci_id55_31$citransition,
                               fit_aci_id55_31$Km,
                               fit_aci_id55_31$GammaStar,
                               fit_aci_id55_31$fitmethod,
                               fit_aci_id55_31$Tcorrect,
                               fit_aci_id55_31$fitTPU)
colnames(aci_data_id55_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id55_31_data)

################################################################################
### plant id56 tleaf20
aci_data_id56_20 = subset(aci_data, id == 56 & set_leafT == 20)
aci_data_id56_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id56_20)
#### fit aci curve
fit_aci_id56_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id56_20)
summary(fit_aci_id56_20)
coef_id56_20 <- coef(fit_aci_id56_20)
#### plot
plot(fit_aci_id56_20)
#### add to dataframe
aci_data_id56_20_data <- cbind(aci_data_id56_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id56_20[,30]),
                               mean(aci_data_id56_20[,72]),
                               fit_aci_id56_20[[2]][1,1],
                               fit_aci_id56_20[[2]][1,2],
                               fit_aci_id56_20[[2]][2,1],
                               fit_aci_id56_20[[2]][2,2],
                               fit_aci_id56_20[[2]][3,1],
                               fit_aci_id56_20[[2]][3,2],
                               # fit_aci_id56_20[[2]][4,1],
                               # fit_aci_id56_20[[2]][4,2],
                               fit_aci_id56_20$RMSE,
                               fit_aci_id56_20$Ci_transition,
                               fit_aci_id56_20$citransition,
                               fit_aci_id56_20$Km,
                               fit_aci_id56_20$GammaStar,
                               fit_aci_id56_20$fitmethod,
                               fit_aci_id56_20$Tcorrect,
                               fit_aci_id56_20$fitTPU)
colnames(aci_data_id56_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id56_20_data)

################################################################################
### plant id56 tleaf25
aci_data_id56_25 = subset(aci_data, id == 56 & set_leafT == 25)
aci_data_id56_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id56_25)
aci_data_id56_25 = subset(aci_data, id == 56 & set_leafT == 25 & Ci < 1200)
plot(Adyn ~ Ci, data = aci_data_id56_25)
#### fit aci curve
fit_aci_id56_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id56_25)
summary(fit_aci_id56_25)
coef_id56_25 <- coef(fit_aci_id56_25)
#### plot
plot(fit_aci_id56_25)
#### add to dataframe
aci_data_id56_25_data <- cbind(aci_data_id56_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id56_25[,30]),
                               mean(aci_data_id56_25[,72]),
                               fit_aci_id56_25[[2]][1,1],
                               fit_aci_id56_25[[2]][1,2],
                               fit_aci_id56_25[[2]][2,1],
                               fit_aci_id56_25[[2]][2,2],
                               fit_aci_id56_25[[2]][3,1],
                               fit_aci_id56_25[[2]][3,2],
                               # fit_aci_id56_25[[2]][4,1],
                               # fit_aci_id56_25[[2]][4,2],
                               fit_aci_id56_25$RMSE,
                               fit_aci_id56_25$Ci_transition,
                               fit_aci_id56_25$citransition,
                               fit_aci_id56_25$Km,
                               fit_aci_id56_25$GammaStar,
                               fit_aci_id56_25$fitmethod,
                               fit_aci_id56_25$Tcorrect,
                               fit_aci_id56_25$fitTPU)
colnames(aci_data_id56_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id56_25_data)

################################################################################
### plant id56 tleaf31
aci_data_id56_31 = subset(aci_data, id == 56 & set_leafT == 31)
aci_data_id56_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id56_31)
#### fit aci curve
fit_aci_id56_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id56_31)
summary(fit_aci_id56_31)
coef_id56_31 <- coef(fit_aci_id56_31)
#### plot
plot(fit_aci_id56_31)
#### add to dataframe
aci_data_id56_31_data <- cbind(aci_data_id56_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id56_31[,30]),
                               mean(aci_data_id56_31[,72]),
                               fit_aci_id56_31[[2]][1,1],
                               fit_aci_id56_31[[2]][1,2],
                               fit_aci_id56_31[[2]][2,1],
                               fit_aci_id56_31[[2]][2,2],
                               fit_aci_id56_31[[2]][3,1],
                               fit_aci_id56_31[[2]][3,2],
                               # fit_aci_id56_31[[2]][4,1],
                               # fit_aci_id56_31[[2]][4,2],
                               fit_aci_id56_31$RMSE,
                               fit_aci_id56_31$Ci_transition,
                               fit_aci_id56_31$citransition,
                               fit_aci_id56_31$Km,
                               fit_aci_id56_31$GammaStar,
                               fit_aci_id56_31$fitmethod,
                               fit_aci_id56_31$Tcorrect,
                               fit_aci_id56_31$fitTPU)
colnames(aci_data_id56_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id56_31_data)

################################################################################
### plant id57 tleaf20
aci_data_id57_20 = subset(aci_data, id == 57 & set_leafT == 20)
aci_data_id57_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id57_20)
#### fit aci curve
fit_aci_id57_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id57_20)
summary(fit_aci_id57_20)
coef_id57_20 <- coef(fit_aci_id57_20)
#### plot
plot(fit_aci_id57_20)
#### add to dataframe
aci_data_id57_20_data <- cbind(aci_data_id57_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id57_20[,30]),
                               mean(aci_data_id57_20[,72]),
                               fit_aci_id57_20[[2]][1,1],
                               fit_aci_id57_20[[2]][1,2],
                               fit_aci_id57_20[[2]][2,1],
                               fit_aci_id57_20[[2]][2,2],
                               fit_aci_id57_20[[2]][3,1],
                               fit_aci_id57_20[[2]][3,2],
                               # fit_aci_id57_20[[2]][4,1],
                               # fit_aci_id57_20[[2]][4,2],
                               fit_aci_id57_20$RMSE,
                               fit_aci_id57_20$Ci_transition,
                               fit_aci_id57_20$citransition,
                               fit_aci_id57_20$Km,
                               fit_aci_id57_20$GammaStar,
                               fit_aci_id57_20$fitmethod,
                               fit_aci_id57_20$Tcorrect,
                               fit_aci_id57_20$fitTPU)
colnames(aci_data_id57_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id57_20_data)

################################################################################
### plant id57 tleaf25
aci_data_id57_25 = subset(aci_data, id == 57 & set_leafT == 25)
aci_data_id57_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id57_25)
#### fit aci curve
fit_aci_id57_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id57_25)
summary(fit_aci_id57_25)
coef_id57_25 <- coef(fit_aci_id57_25)
#### plot
plot(fit_aci_id57_25)
#### add to dataframe
aci_data_id57_25_data <- cbind(aci_data_id57_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id57_25[,30]),
                               mean(aci_data_id57_25[,72]),
                               fit_aci_id57_25[[2]][1,1],
                               fit_aci_id57_25[[2]][1,2],
                               fit_aci_id57_25[[2]][2,1],
                               fit_aci_id57_25[[2]][2,2],
                               fit_aci_id57_25[[2]][3,1],
                               fit_aci_id57_25[[2]][3,2],
                               # fit_aci_id57_25[[2]][4,1],
                               # fit_aci_id57_25[[2]][4,2],
                               fit_aci_id57_25$RMSE,
                               fit_aci_id57_25$Ci_transition,
                               fit_aci_id57_25$citransition,
                               fit_aci_id57_25$Km,
                               fit_aci_id57_25$GammaStar,
                               fit_aci_id57_25$fitmethod,
                               fit_aci_id57_25$Tcorrect,
                               fit_aci_id57_25$fitTPU)
colnames(aci_data_id57_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id57_25_data)

################################################################################
### plant id57 tleaf31
aci_data_id57_31 = subset(aci_data, id == 57 & set_leafT == 31)
aci_data_id57_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id57_31)
aci_data_id57_31 = subset(aci_data, id == 57 & set_leafT == 31 & Ci > 0)
plot(Adyn ~ Ci, data = aci_data_id57_31)
#### fit aci curve
fit_aci_id57_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id57_31)
summary(fit_aci_id57_31)
coef_id57_31 <- coef(fit_aci_id57_31)
#### plot
plot(fit_aci_id57_31)
#### add to dataframe
aci_data_id57_31_data <- cbind(aci_data_id57_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id57_31[,30]),
                               mean(aci_data_id57_31[,72]),
                               fit_aci_id57_31[[2]][1,1],
                               fit_aci_id57_31[[2]][1,2],
                               fit_aci_id57_31[[2]][2,1],
                               fit_aci_id57_31[[2]][2,2],
                               fit_aci_id57_31[[2]][3,1],
                               fit_aci_id57_31[[2]][3,2],
                               # fit_aci_id57_31[[2]][4,1],
                               # fit_aci_id57_31[[2]][4,2],
                               fit_aci_id57_31$RMSE,
                               fit_aci_id57_31$Ci_transition,
                               fit_aci_id57_31$citransition,
                               fit_aci_id57_31$Km,
                               fit_aci_id57_31$GammaStar,
                               fit_aci_id57_31$fitmethod,
                               fit_aci_id57_31$Tcorrect,
                               fit_aci_id57_31$fitTPU)
colnames(aci_data_id57_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id57_31_data)

################################################################################
### plant id58 tleaf20
aci_data_id58_20 = subset(aci_data, id == 58 & set_leafT == 20)
aci_data_id58_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id58_20)
#### fit aci curve
fit_aci_id58_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id58_20)
summary(fit_aci_id58_20)
coef_id58_20 <- coef(fit_aci_id58_20)
#### plot
plot(fit_aci_id58_20)
#### add to dataframe
aci_data_id58_20_data <- cbind(aci_data_id58_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id58_20[,30]),
                               mean(aci_data_id58_20[,72]),
                               fit_aci_id58_20[[2]][1,1],
                               fit_aci_id58_20[[2]][1,2],
                               fit_aci_id58_20[[2]][2,1],
                               fit_aci_id58_20[[2]][2,2],
                               fit_aci_id58_20[[2]][3,1],
                               fit_aci_id58_20[[2]][3,2],
                               # fit_aci_id58_20[[2]][4,1],
                               # fit_aci_id58_20[[2]][4,2],
                               fit_aci_id58_20$RMSE,
                               fit_aci_id58_20$Ci_transition,
                               fit_aci_id58_20$citransition,
                               fit_aci_id58_20$Km,
                               fit_aci_id58_20$GammaStar,
                               fit_aci_id58_20$fitmethod,
                               fit_aci_id58_20$Tcorrect,
                               fit_aci_id58_20$fitTPU)
colnames(aci_data_id58_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id58_20_data)

################################################################################
### plant id58 tleaf25
aci_data_id58_25 = subset(aci_data, id == 58 & set_leafT == 25)
aci_data_id58_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id58_25)
#### fit aci curve
fit_aci_id58_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 500,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id58_25)
summary(fit_aci_id58_25)
coef_id58_25 <- coef(fit_aci_id58_25)
#### plot
plot(fit_aci_id58_25)
#### add to dataframe
aci_data_id58_25_data <- cbind(aci_data_id58_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id58_25[,30]),
                               mean(aci_data_id58_25[,72]),
                               fit_aci_id58_25[[2]][1,1],
                               fit_aci_id58_25[[2]][1,2],
                               fit_aci_id58_25[[2]][2,1],
                               fit_aci_id58_25[[2]][2,2],
                               fit_aci_id58_25[[2]][3,1],
                               fit_aci_id58_25[[2]][3,2],
                               # fit_aci_id58_25[[2]][4,1],
                               # fit_aci_id58_25[[2]][4,2],
                               fit_aci_id58_25$RMSE,
                               fit_aci_id58_25$Ci_transition,
                               fit_aci_id58_25$citransition,
                               fit_aci_id58_25$Km,
                               fit_aci_id58_25$GammaStar,
                               fit_aci_id58_25$fitmethod,
                               fit_aci_id58_25$Tcorrect,
                               fit_aci_id58_25$fitTPU)
colnames(aci_data_id58_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id58_25_data)

################################################################################
### plant id58 tleaf31
aci_data_id58_31 = subset(aci_data, id == 58 & set_leafT == 31)
aci_data_id58_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id58_31)
#### fit aci curve
fit_aci_id58_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id58_31)
summary(fit_aci_id58_31)
coef_id58_31 <- coef(fit_aci_id58_31)
#### plot
plot(fit_aci_id58_31)
#### add to dataframe
aci_data_id58_31_data <- cbind(aci_data_id58_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id58_31[,30]),
                               mean(aci_data_id58_31[,72]),
                               fit_aci_id58_31[[2]][1,1],
                               fit_aci_id58_31[[2]][1,2],
                               fit_aci_id58_31[[2]][2,1],
                               fit_aci_id58_31[[2]][2,2],
                               fit_aci_id58_31[[2]][3,1],
                               fit_aci_id58_31[[2]][3,2],
                               # fit_aci_id58_31[[2]][4,1],
                               # fit_aci_id58_31[[2]][4,2],
                               fit_aci_id58_31$RMSE,
                               fit_aci_id58_31$Ci_transition,
                               fit_aci_id58_31$citransition,
                               fit_aci_id58_31$Km,
                               fit_aci_id58_31$GammaStar,
                               fit_aci_id58_31$fitmethod,
                               fit_aci_id58_31$Tcorrect,
                               fit_aci_id58_31$fitTPU)
colnames(aci_data_id58_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id58_31_data)

################################################################################
### plant id59 tleaf20
aci_data_id59_20 = subset(aci_data, id == 59 & set_leafT == 20)
aci_data_id59_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id59_20)
#### fit aci curve
fit_aci_id59_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id59_20)
summary(fit_aci_id59_20)
coef_id59_20 <- coef(fit_aci_id59_20)
#### plot
plot(fit_aci_id59_20)
#### add to dataframe
aci_data_id59_20_data <- cbind(aci_data_id59_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id59_20[,30]),
                               mean(aci_data_id59_20[,72]),
                               fit_aci_id59_20[[2]][1,1],
                               fit_aci_id59_20[[2]][1,2],
                               fit_aci_id59_20[[2]][2,1],
                               fit_aci_id59_20[[2]][2,2],
                               fit_aci_id59_20[[2]][3,1],
                               fit_aci_id59_20[[2]][3,2],
                               # fit_aci_id59_20[[2]][4,1],
                               # fit_aci_id59_20[[2]][4,2],
                               fit_aci_id59_20$RMSE,
                               fit_aci_id59_20$Ci_transition,
                               fit_aci_id59_20$citransition,
                               fit_aci_id59_20$Km,
                               fit_aci_id59_20$GammaStar,
                               fit_aci_id59_20$fitmethod,
                               fit_aci_id59_20$Tcorrect,
                               fit_aci_id59_20$fitTPU)
colnames(aci_data_id59_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id59_20_data)

################################################################################
### plant id59 tleaf25
aci_data_id59_25 = subset(aci_data, id == 59 & set_leafT == 25)
aci_data_id59_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id59_25)
#### fit aci curve
fit_aci_id59_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id59_25)
summary(fit_aci_id59_25)
coef_id59_25 <- coef(fit_aci_id59_25)
#### plot
plot(fit_aci_id59_25)
#### add to dataframe
aci_data_id59_25_data <- cbind(aci_data_id59_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id59_25[,30]),
                               mean(aci_data_id59_25[,72]),
                               fit_aci_id59_25[[2]][1,1],
                               fit_aci_id59_25[[2]][1,2],
                               fit_aci_id59_25[[2]][2,1],
                               fit_aci_id59_25[[2]][2,2],
                               fit_aci_id59_25[[2]][3,1],
                               fit_aci_id59_25[[2]][3,2],
                               # fit_aci_id59_25[[2]][4,1],
                               # fit_aci_id59_25[[2]][4,2],
                               fit_aci_id59_25$RMSE,
                               fit_aci_id59_25$Ci_transition,
                               fit_aci_id59_25$citransition,
                               fit_aci_id59_25$Km,
                               fit_aci_id59_25$GammaStar,
                               fit_aci_id59_25$fitmethod,
                               fit_aci_id59_25$Tcorrect,
                               fit_aci_id59_25$fitTPU)
colnames(aci_data_id59_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id59_25_data)

################################################################################
### plant id59 tleaf31
aci_data_id59_31 = subset(aci_data, id == 59 & set_leafT == 31)
aci_data_id59_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id59_31)
#### fit aci curve
fit_aci_id59_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id59_31)
summary(fit_aci_id59_31)
coef_id59_31 <- coef(fit_aci_id59_31)
#### plot
plot(fit_aci_id59_31)
#### add to dataframe
aci_data_id59_31_data <- cbind(aci_data_id59_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id59_31[,30]),
                               mean(aci_data_id59_31[,72]),
                               fit_aci_id59_31[[2]][1,1],
                               fit_aci_id59_31[[2]][1,2],
                               fit_aci_id59_31[[2]][2,1],
                               fit_aci_id59_31[[2]][2,2],
                               fit_aci_id59_31[[2]][3,1],
                               fit_aci_id59_31[[2]][3,2],
                               # fit_aci_id59_31[[2]][4,1],
                               # fit_aci_id59_31[[2]][4,2],
                               fit_aci_id59_31$RMSE,
                               fit_aci_id59_31$Ci_transition,
                               fit_aci_id59_31$citransition,
                               fit_aci_id59_31$Km,
                               fit_aci_id59_31$GammaStar,
                               fit_aci_id59_31$fitmethod,
                               fit_aci_id59_31$Tcorrect,
                               fit_aci_id59_31$fitTPU)
colnames(aci_data_id59_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id59_31_data)

################################################################################
### plant id60 tleaf20
aci_data_id60_20 = subset(aci_data, id == 60 & set_leafT == 20)
aci_data_id60_20[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id60_20)
#### fit aci curve
fit_aci_id60_20 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id60_20)
summary(fit_aci_id60_20)
coef_id60_20 <- coef(fit_aci_id60_20)
#### plot
plot(fit_aci_id60_20)
#### add to dataframe
aci_data_id60_20_data <- cbind(aci_data_id60_20[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id60_20[,30]),
                               mean(aci_data_id60_20[,72]),
                               fit_aci_id60_20[[2]][1,1],
                               fit_aci_id60_20[[2]][1,2],
                               fit_aci_id60_20[[2]][2,1],
                               fit_aci_id60_20[[2]][2,2],
                               fit_aci_id60_20[[2]][3,1],
                               fit_aci_id60_20[[2]][3,2],
                               # fit_aci_id60_20[[2]][4,1],
                               # fit_aci_id60_20[[2]][4,2],
                               fit_aci_id60_20$RMSE,
                               fit_aci_id60_20$Ci_transition,
                               fit_aci_id60_20$citransition,
                               fit_aci_id60_20$Km,
                               fit_aci_id60_20$GammaStar,
                               fit_aci_id60_20$fitmethod,
                               fit_aci_id60_20$Tcorrect,
                               fit_aci_id60_20$fitTPU)
colnames(aci_data_id60_20_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id60_20_data)

################################################################################
### plant id60 tleaf25
aci_data_id60_25 = subset(aci_data, id == 60 & set_leafT == 25)
aci_data_id60_25[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id60_25)
#### fit aci curve
fit_aci_id60_25 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id60_25)
summary(fit_aci_id60_25)
coef_id60_25 <- coef(fit_aci_id60_25)
#### plot
plot(fit_aci_id60_25)
#### add to dataframe
aci_data_id60_25_data <- cbind(aci_data_id60_25[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id60_25[,30]),
                               mean(aci_data_id60_25[,72]),
                               fit_aci_id60_25[[2]][1,1],
                               fit_aci_id60_25[[2]][1,2],
                               fit_aci_id60_25[[2]][2,1],
                               fit_aci_id60_25[[2]][2,2],
                               fit_aci_id60_25[[2]][3,1],
                               fit_aci_id60_25[[2]][3,2],
                               # fit_aci_id60_25[[2]][4,1],
                               # fit_aci_id60_25[[2]][4,2],
                               fit_aci_id60_25$RMSE,
                               fit_aci_id60_25$Ci_transition,
                               fit_aci_id60_25$citransition,
                               fit_aci_id60_25$Km,
                               fit_aci_id60_25$GammaStar,
                               fit_aci_id60_25$fitmethod,
                               fit_aci_id60_25$Tcorrect,
                               fit_aci_id60_25$fitTPU)
colnames(aci_data_id60_25_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id60_25_data)

################################################################################
### plant id60 tleaf31
aci_data_id60_31 = subset(aci_data, id == 60 & set_leafT == 31)
aci_data_id60_31[, c(4, 7:9)]

#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id60_31)
#### fit aci curve
fit_aci_id60_31 = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         citransition = 371,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id60_31)
summary(fit_aci_id60_31)
coef_id60_31 <- coef(fit_aci_id60_31)
#### plot
plot(fit_aci_id60_31)
#### add to dataframe
aci_data_id60_31_data <- cbind(aci_data_id60_31[1, c(6:10, 14, 16, 19)],
                               mean(aci_data_id60_31[,30]),
                               mean(aci_data_id60_31[,72]),
                               fit_aci_id60_31[[2]][1,1],
                               fit_aci_id60_31[[2]][1,2],
                               fit_aci_id60_31[[2]][2,1],
                               fit_aci_id60_31[[2]][2,2],
                               fit_aci_id60_31[[2]][3,1],
                               fit_aci_id60_31[[2]][3,2],
                               # fit_aci_id60_31[[2]][4,1],
                               # fit_aci_id60_31[[2]][4,2],
                               fit_aci_id60_31$RMSE,
                               fit_aci_id60_31$Ci_transition,
                               fit_aci_id60_31$citransition,
                               fit_aci_id60_31$Km,
                               fit_aci_id60_31$GammaStar,
                               fit_aci_id60_31$fitmethod,
                               fit_aci_id60_31$Tcorrect,
                               fit_aci_id60_31$fitTPU)
colnames(aci_data_id60_31_data) <- c('averaging', 'machine', 'id', 'set_leafT', 'photo_resp',
                                     'anet_420', 'ci_420', 'gsw_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id60_31_data)


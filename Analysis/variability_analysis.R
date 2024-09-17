# placeholder for description

## load libraries
library(lme4)
library(car)
library(emmeans)


## load data
all_data <- read.csv('Git/variability_ms_thesis/Data/all_data.csv')

## separate treatments
all_data$TV[all_data$treatment == 'HTVHLV'] <- 'HTV'
all_data$TV[all_data$treatment == 'HTVLLV'] <- 'HTV'
all_data$TV[all_data$treatment == 'LTVHLV'] <- 'LTV'
all_data$TV[all_data$treatment == 'LTVLLV'] <- 'LTV'

all_data$LV[all_data$treatment == 'HTVHLV'] <- 'HLV'
all_data$LV[all_data$treatment == 'HTVLLV'] <- 'LLV'
all_data$LV[all_data$treatment == 'LTVHLV'] <- 'HLV'
all_data$LV[all_data$treatment == 'LTVLLV'] <- 'LLV'

class(all_data$chamber)
all_data$chamber_fac <- as.factor(all_data$chamber)

## fit lme models

################################################################################
## Photosynthesis traits
### vcmax_tleaf_20
hist(all_data$vcmax_tleaf_20)
vcmax_tleaf_20_lmer <- lmer(vcmax_tleaf_20 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_20_lmer) ~ fitted(vcmax_tleaf_20_lmer))
summary(vcmax_tleaf_20_lmer)
Anova(vcmax_tleaf_20_lmer)
emmeans(vcmax_tleaf_20_lmer, ~TV)
emmeans(vcmax_tleaf_20_lmer, ~TV*LV)

### vcmax_tleaf_25
vcmax_tleaf_25_lmer <- lmer(vcmax_tleaf_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_25_lmer) ~ fitted(vcmax_tleaf_25_lmer))
summary(vcmax_tleaf_25_lmer)
Anova(vcmax_tleaf_25_lmer)
emmeans(vcmax_tleaf_25_lmer, ~TV)
emmeans(vcmax_tleaf_25_lmer, ~TV*LV)

### vcmax_tleaf_31
vcmax_tleaf_31_lmer <- lmer(log(vcmax_tleaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_31_lmer) ~ fitted(vcmax_tleaf_31_lmer))
summary(vcmax_tleaf_31_lmer)
Anova(vcmax_tleaf_31_lmer)
emmeans(vcmax_tleaf_31_lmer, ~TV)
emmeans(vcmax_tleaf_31_lmer, ~TV*LV)


### jmax_tleaf_20
jmax_tleaf_20_lmer <- lmer(jmax_tleaf_20 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_20_lmer) ~ fitted(jmax_tleaf_20_lmer))
summary(jmax_tleaf_20_lmer)
Anova(jmax_tleaf_20_lmer)
emmeans(jmax_tleaf_20_lmer, ~TV)
emmeans(jmax_tleaf_20_lmer, ~TV*LV)

### jmax_tleaf_25
jmax_tleaf_25_lmer <- lmer(jmax_tleaf_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_25_lmer) ~ fitted(jmax_tleaf_25_lmer))
summary(jmax_tleaf_25_lmer)
Anova(jmax_tleaf_25_lmer)
emmeans(jmax_tleaf_25_lmer, ~TV)
emmeans(jmax_tleaf_25_lmer, ~TV*LV)

### jmax_tleaf_31
jmax_tleaf_31_lmer <- lmer(jmax_tleaf_31 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_31_lmer) ~ fitted(jmax_tleaf_31_lmer))
summary(jmax_tleaf_31_lmer)
Anova(jmax_tleaf_31_lmer)
emmeans(jmax_tleaf_31_lmer, ~TV)
emmeans(jmax_tleaf_31_lmer, ~TV*LV)

### stomatal conductance (gsw_420_20)
hist(all_data$gsw_420_20)
hist(log(all_data$gsw_420_20))
gsw_420_20_lmer <- lmer(log(gsw_420_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_20_lmer) ~ fitted(gsw_420_20_lmer))
summary(gsw_420_20_lmer)
Anova(gsw_420_20_lmer)
emmeans(gsw_420_20_lmer, ~TV)
emmeans(gsw_420_20_lmer, ~TV*LV)

### stomatal conductance (gsw_420_25)
hist(all_data$gsw_420_25)
gsw_420_25_lmer <- lmer(gsw_420_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_25_lmer) ~ fitted(gsw_420_25_lmer))
summary(gsw_420_25_lmer)
Anova(gsw_420_25_lmer)
emmeans(gsw_420_25_lmer, ~TV)
emmeans(gsw_420_25_lmer, ~TV*LV)

### stomatal conductance (gsw_420_31)
hist(all_data$gsw_420_31)
gsw_420_31_lmer <- lmer(gsw_420_31 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_31_lmer) ~ fitted(gsw_420_31_lmer))
summary(gsw_420_31_lmer)
Anova(gsw_420_31_lmer)
emmeans(gsw_420_31_lmer, ~TV)
emmeans(gsw_420_31_lmer, ~TV*LV)


### vapor pressure deficit (vpd_leaf_20)
hist(all_data$vpd_leaf_20)
vpd_leaf_20_lmer <- lmer(vpd_leaf_20 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vpd_leaf_20_lmer) ~ fitted(vpd_leaf_20_lmer))
summary(vpd_leaf_20_lmer)
Anova(vpd_leaf_20_lmer)
emmeans(vpd_leaf_20_lmer, ~TV)
emmeans(vpd_leaf_20_lmer, ~TV*LV)

### vapor pressure deficit (vpd_leaf_25)
hist(all_data$vpd_leaf_25)
vpd_leaf_25_lmer <- lmer(vpd_leaf_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vpd_leaf_25_lmer) ~ fitted(vpd_leaf_25_lmer))
summary(vpd_leaf_25_lmer)
Anova(vpd_leaf_25_lmer)
emmeans(vpd_leaf_25_lmer, ~TV)
emmeans(vpd_leaf_25_lmer, ~TV*LV)

### vapor pressure deficit (vpd_leaf_31)
hist(all_data$vpd_leaf_31)
hist(log(all_data$vpd_leaf_31))
vpd_leaf_31_lmer <- lmer(log(vpd_leaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vpd_leaf_31_lmer) ~ fitted(vpd_leaf_31_lmer))
summary(vpd_leaf_31_lmer)
Anova(vpd_leaf_31_lmer)
emmeans(vpd_leaf_31_lmer, ~TV)
emmeans(vpd_leaf_31_lmer, ~TV*LV)



###############################################################################
## Multispeq
### Phi2_light
Phi2_light_lmer <- lmer(Phi2_light ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(Phi2_light_lmer) ~ fitted(Phi2_light_lmer))
summary(Phi2_light_lmer)
Anova(Phi2_light_lmer)
emmeans(Phi2_light_lmer, ~TV)
emmeans(Phi2_light_lmer, ~TV*LV)

### Phi2_dark
Phi2_dark_lmer <- lmer(Phi2_dark ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(Phi2_dark_lmer) ~ fitted(Phi2_dark_lmer))
summary(Phi2_dark_lmer)
Anova(Phi2_dark_lmer)
emmeans(Phi2_dark_lmer, ~TV)
emmeans(Phi2_dark_lmer, ~TV*LV)



################################################################################
## Chlorophyll
### total chlorophyll content (chl.mmolm2)
chl.mmolm2_lmer <- lmer(chl.mmolm2 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(chl.mmolm2_lmer) ~ fitted(chl.mmolm2_lmer))
summary(chl.mmolm2_lmer)
Anova(chl.mmolm2_lmer)
emmeans(chl.mmolm2_lmer, ~TV)
emmeans(chl.mmolm2_lmer, ~TV*LV)

### chlorophyll A (chlA.mmolm2)
chlA.mmolm2_lmer <- lmer(chlA.mmolm2 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(chlA.mmolm2_lmer) ~ fitted(chlA.mmolm2_lmer))
summary(chlA.mmolm2_lmer)
Anova(chlA.mmolm2_lmer)
emmeans(chlA.mmolm2_lmer, ~TV)
emmeans(chlA.mmolm2_lmer, ~TV*LV)

### chlorophyll B (chlB.mmolm2)
chlB.mmolm2_lmer <- lmer(chlB.mmolm2 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(chlB.mmolm2_lmer) ~ fitted(chlB.mmolm2_lmer))
summary(chlB.mmolm2_lmer)
Anova(chlB.mmolm2_lmer)
emmeans(chlB.mmolm2_lmer, ~TV)
emmeans(chlB.mmolm2_lmer, ~TV*LV)



###############################################################################
## Structural Traits
### above_biomass_dry_weight
above_biomass_dry_weight_lmer <- lmer(above_biomass_dry_weight ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(above_biomass_dry_weight_lmer) ~ fitted(above_biomass_dry_weight_lmer))
summary(above_biomass_dry_weight_lmer)
Anova(above_biomass_dry_weight_lmer)


### SLA_focal
SLA_focal_lmer <- lmer(SLA_focal ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(SLA_focal_lmer) ~ fitted(SLA_focal_lmer))
summary(SLA_focal_lmer)
Anova(SLA_focal_lmer)


###############################################################################
## 13C and 15N


###############################################################################
## Dark Respiration



# placeholder for description

# TO DOs:
## graphs to summarize how light is changing and temperature
## aggregate PAR data over a larger time window
## look for marginal sig less than 0.1
## plot raw data
## lok at carbon gain == biomass
## compare values at different temps


# different sections of code
## Photosynthesis traits
## Multispeq
## Chlorophyll
## Structural Traits
## Dark Respiration
## 13C and 15N
# **high significance
# **significance

## load libraries
library(lme4)
library(car)
library(emmeans)
library(ggplot2)


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
hist(log(all_data$vcmax_tleaf_20))
vcmax_tleaf_20_lmer <- lmer(log(vcmax_tleaf_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_20_lmer) ~ fitted(vcmax_tleaf_20_lmer))
summary(vcmax_tleaf_20_lmer)
Anova(vcmax_tleaf_20_lmer)
emmeans(vcmax_tleaf_20_lmer, ~TV)
emmeans(vcmax_tleaf_20_lmer, ~TV*LV)

vcmax_tleaf_20_plot <- ggplot(aes(y=(vcmax_tleaf_20), x = treatment), data = all_data) +
  geom_boxplot()
vcmax_tleaf_20_plot

### vcmax_tleaf_25
hist(all_data$vcmax_tleaf_25)
hist(log(all_data$vcmax_tleaf_25))
vcmax_tleaf_25_lmer <- lmer(log(vcmax_tleaf_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_25_lmer) ~ fitted(vcmax_tleaf_25_lmer))
summary(vcmax_tleaf_25_lmer)
Anova(vcmax_tleaf_25_lmer)
emmeans(vcmax_tleaf_25_lmer, ~TV)
emmeans(vcmax_tleaf_25_lmer, ~TV*LV)

### vcmax_tleaf_31
hist(all_data$vcmax_tleaf_31)
hist(log(all_data$vcmax_tleaf_31))
vcmax_tleaf_31_lmer <- lmer(log(vcmax_tleaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_31_lmer) ~ fitted(vcmax_tleaf_31_lmer))
summary(vcmax_tleaf_31_lmer)
Anova(vcmax_tleaf_31_lmer)
emmeans(vcmax_tleaf_31_lmer, ~TV)
emmeans(vcmax_tleaf_31_lmer, ~TV*LV)


### jmax_tleaf_20
hist(all_data$jmax_tleaf_20)
hist(log(all_data$jmax_tleaf_20))
jmax_tleaf_20_lmer <- lmer(log(jmax_tleaf_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_20_lmer) ~ fitted(jmax_tleaf_20_lmer))
summary(jmax_tleaf_20_lmer)
Anova(jmax_tleaf_20_lmer)
emmeans(jmax_tleaf_20_lmer, ~TV)
emmeans(jmax_tleaf_20_lmer, ~TV*LV)

### jmax_tleaf_25
hist(all_data$jmax_tleaf_25)
hist(log(all_data$jmax_tleaf_25))
jmax_tleaf_25_lmer <- lmer(log(jmax_tleaf_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_25_lmer) ~ fitted(jmax_tleaf_25_lmer))
summary(jmax_tleaf_25_lmer)
Anova(jmax_tleaf_25_lmer)
emmeans(jmax_tleaf_25_lmer, ~TV)
emmeans(jmax_tleaf_25_lmer, ~TV*LV)

### jmax_tleaf_31
hist(all_data$jmax_tleaf_31)
hist(log(all_data$jmax_tleaf_31))
jmax_tleaf_31_lmer <- lmer(log(jmax_tleaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
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



###############################################################################
## Multispeq

### Phi2_dark
Phi2_dark_lmer <- lmer(Phi2_dark ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(Phi2_dark_lmer) ~ fitted(Phi2_dark_lmer))
summary(Phi2_dark_lmer)
Anova(Phi2_dark_lmer)
emmeans(Phi2_dark_lmer, ~TV)
emmeans(Phi2_dark_lmer, ~TV*LV)

### chlorophyll florescence (NPQt_dark)
hist(all_data$NPQt_dark)
NPQt_dark_lmer <- lmer(NPQt_dark ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(NPQt_dark_lmer) ~ fitted(NPQt_dark_lmer))
summary(NPQt_dark_lmer)
Anova(NPQt_dark_lmer)
emmeans(NPQt_dark_lmer, ~TV)
emmeans(NPQt_dark_lmer, ~TV*LV)

### PhiNPQ_dark
hist(all_data$PhiNPQ_dark)
PhiNPQ_dark_lmer <- lmer(log(PhiNPQ_dark) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(PhiNPQ_dark_lmer) ~ fitted(PhiNPQ_dark_lmer))
summary(PhiNPQ_dark_lmer)
Anova(PhiNPQ_dark_lmer)
emmeans(PhiNPQ_dark_lmer, ~TV)
emmeans(PhiNPQ_dark_lmer, ~TV*LV)

### PhiNO_dark **significance
hist(all_data$PhiNO_dark)
PhiNO_dark_lmer <- lmer(log(PhiNO_dark) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(PhiNO_dark_lmer) ~ fitted(PhiNO_dark_lmer))
summary(PhiNO_dark_lmer)
Anova(PhiNO_dark_lmer)
emmeans(PhiNO_dark_lmer, ~TV)
emmeans(PhiNO_dark_lmer, ~TV*LV)



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

### chlorophyll B (resp_lmer)
resp_lmer_lmer <- lmer(resp_lmer ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(resp_lmer_lmer) ~ fitted(resp_lmer_lmer))
summary(resp_lmer_lmer)
Anova(resp_lmer_lmer)
emmeans(resp_lmer_lmer, ~TV)
emmeans(resp_lmer_lmer, ~TV*LV)



###############################################################################
## Structural Traits
### above_biomass_dry_weight
above_biomass_dry_weight_lmer <- lmer(above_biomass_dry_weight ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(above_biomass_dry_weight_lmer) ~ fitted(above_biomass_dry_weight_lmer))
summary(above_biomass_dry_weight_lmer)
Anova(above_biomass_dry_weight_lmer)
emmeans(above_biomass_dry_weight_lmer, ~TV)
emmeans(above_biomass_dry_weight_lmer, ~TV*LV)

above_biomass_dry_weight_plot <- ggplot(aes(y=(above_biomass_dry_weight), x = treatment), data = all_data) +
  geom_boxplot()
above_biomass_dry_weight_plot

### SLA_focal
SLA_focal_lmer <- lmer(SLA_focal ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(SLA_focal_lmer) ~ fitted(SLA_focal_lmer))
summary(SLA_focal_lmer)
Anova(SLA_focal_lmer)
emmeans(SLA_focal_lmer, ~TV)
emmeans(SLA_focal_lmer, ~TV*LV)



###############################################################################
## Dark Respiration
hist(all_data$resp)
resp_lmer <- lmer(resp ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~TV)
emmeans(resp_lmer, ~TV*LV)



###############################################################################
## 13C and 15N




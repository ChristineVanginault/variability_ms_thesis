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

### vcmax_tleaf_25
vcmax_tleaf_25_lmer <- lmer(vcmax_tleaf_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_25_lmer) ~ fitted(vcmax_tleaf_25_lmer))
summary(vcmax_tleaf_25_lmer)
Anova(vcmax_tleaf_25_lmer)
emmeans(vcmax_tleaf_25_lmer, ~TV)
emmeans(vcmax_tleaf_25_lmer, ~TV*LV)

###phi2
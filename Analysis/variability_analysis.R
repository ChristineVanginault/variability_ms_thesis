# placeholder for description

# TO DOs:
## graphs to summarize how light is changing and temperature
## aggregate PAR data over a larger time window
## look for marginal sig less than 0.1
## plot raw data
## compare values at different temps


## load libraries
library(lme4)
library(car)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(grid)

## load data
all_data <- read.csv('Git/variability_ms_thesis/Data/all_data.csv')

  # for carbon and nitrogen use data below
all_data2 <- read.csv("Git/variability_ms_thesis/Data/all_data2.csv")

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

## seperate treatments for all_data2
all_data2$TV[all_data2$treatment == 'HTVHLV'] <- 'HTV'
all_data2$TV[all_data2$treatment == 'HTVLLV'] <- 'HTV'
all_data2$TV[all_data2$treatment == 'LTVHLV'] <- 'LTV'
all_data2$TV[all_data2$treatment == 'LTVLLV'] <- 'LTV'

all_data2$LV[all_data2$treatment == 'HTVHLV'] <- 'HLV'
all_data2$LV[all_data2$treatment == 'HTVLLV'] <- 'LLV'
all_data2$LV[all_data2$treatment == 'LTVHLV'] <- 'HLV'
all_data2$LV[all_data2$treatment == 'LTVLLV'] <- 'LLV'

class(all_data2$chamber)
all_data2$chamber_fac <- as.factor(all_data2$chamber)

# fit lme models################################################################

#### Photosynthesis traits #####################################################
### vcmax_tleaf_20
hist(all_data$vcmax_tleaf_20)
hist(log(all_data$vcmax_tleaf_20))
vcmax_tleaf_20_lmer <- lmer(log(vcmax_tleaf_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_20_lmer) ~ fitted(vcmax_tleaf_20_lmer))
summary(vcmax_tleaf_20_lmer)
Anova(vcmax_tleaf_20_lmer)
vcmax_20_anova <- data.frame(Anova(vcmax_tleaf_20_lmer))
vcmax_20_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/vcmax20.csv")
emmeans(vcmax_tleaf_20_lmer, ~TV)
emmeans(vcmax_tleaf_20_lmer, ~TV*LV)
vcmax_20_means <- data.frame(emmeans(vcmax_tleaf_20_lmer, ~TV*LV))
vcmax_20_means$treatment <- paste(vcmax_20_means$TV, vcmax_20_means$LV, sep = "")
vcmax_20_means


vcmax_tleaf_20_plot <- ggplot(aes(y=vcmax_tleaf_20, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
vcmax_tleaf_20_plot


### vcmax_tleaf_25 
hist(all_data$vcmax_tleaf_25)
hist(log(all_data$vcmax_tleaf_25))
vcmax_tleaf_25_lmer <- lmer(log(vcmax_tleaf_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_25_lmer) ~ fitted(vcmax_tleaf_25_lmer))
summary(vcmax_tleaf_25_lmer)
Anova(vcmax_tleaf_25_lmer)
vcmax_25_anova <- data.frame(Anova(vcmax_tleaf_25_lmer))
vcmax_25_anova
write.csv(vcmax_25_anova, "Git/variability_ms_thesis/Tables/vcmax25.csv")
emmeans(vcmax_tleaf_25_lmer, ~TV)
emmeans(vcmax_tleaf_25_lmer, ~TV*LV)
vcmax_25_means <- data.frame(emmeans(vcmax_tleaf_25_lmer, ~TV*LV))
vcmax_25_means$treatment <- paste(vcmax_25_means$TV, vcmax_25_means$LV, sep = "")
vcmax_25_means

vcmax_tleaf_25_plot <- ggplot(aes(y=vcmax_tleaf_25, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
vcmax_tleaf_25_plot

### vcmax_tleaf_31
hist(all_data$vcmax_tleaf_31)
hist(log(all_data$vcmax_tleaf_31))
vcmax_tleaf_31_lmer <- lmer(log(vcmax_tleaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(vcmax_tleaf_31_lmer) ~ fitted(vcmax_tleaf_31_lmer))
summary(vcmax_tleaf_31_lmer)
Anova(vcmax_tleaf_31_lmer)
vcmax_31_anova <- data.frame(Anova(vcmax_tleaf_31_lmer))
vcmax_31_anova
write.csv(vcmax_31_anova, "Git/variability_ms_thesis/Tables/vcmax31.csv")
emmeans(vcmax_tleaf_31_lmer, ~TV)
emmeans(vcmax_tleaf_31_lmer, ~TV*LV)
vcmax_31_means <- data.frame(emmeans(vcmax_tleaf_31_lmer, ~TV*LV))
vcmax_31_means$treatment <- paste(vcmax_31_means$TV, vcmax_31_means$LV, sep = "")
vcmax_31_means


vcmax_tleaf_31_plot <- ggplot(aes(y=vcmax_tleaf_31, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
vcmax_tleaf_31_plot


### jmax_tleaf_20
hist(all_data$jmax_tleaf_20)
hist(log(all_data$jmax_tleaf_20))
jmax_tleaf_20_lmer <- lmer(log(jmax_tleaf_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_20_lmer) ~ fitted(jmax_tleaf_20_lmer))
summary(jmax_tleaf_20_lmer)
Anova(jmax_tleaf_20_lmer)
jmax_20_anova <- data.frame(Anova(jmax_tleaf_20_lmer))
jmax_20_anova
write.csv(jmax_20_anova, "Git/variability_ms_thesis/Tables/jmax20.csv")
emmeans(jmax_tleaf_20_lmer, ~TV)
emmeans(jmax_tleaf_20_lmer, ~TV*LV)
jmax_20_means <- data.frame(emmeans(jmax_tleaf_20_lmer, ~TV*LV))
jmax_20_means$treatment <- paste(jmax_20_means$TV, jmax_20_means$LV, sep = "")
jmax_20_means

jmax_tleaf_20_plot <- ggplot(aes(y=jmax_tleaf_20, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
jmax_tleaf_20_plot

### jmax_tleaf_25
hist(all_data$jmax_tleaf_25)
hist(log(all_data$jmax_tleaf_25))
jmax_tleaf_25_lmer <- lmer(log(jmax_tleaf_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_25_lmer) ~ fitted(jmax_tleaf_25_lmer))
summary(jmax_tleaf_25_lmer)
Anova(jmax_tleaf_25_lmer)
jmax_25_anova <- data.frame(Anova(jmax_tleaf_25_lmer))
jmax_25_anova
write.csv(jmax_25_anova, "Git/variability_ms_thesis/Tables/jmax25.csv")
emmeans(jmax_tleaf_25_lmer, ~TV)
emmeans(jmax_tleaf_25_lmer, ~TV*LV)
jmax_25_means <- data.frame(emmeans(jmax_tleaf_25_lmer, ~TV*LV))
jmax_25_means$treatment <- paste(jmax_25_means$TV, jmax_25_means$LV, sep = "")
jmax_25_means

jmax_tleaf_25_plot <- ggplot(aes(y=jmax_tleaf_25, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
jmax_tleaf_25_plot


### jmax_tleaf_31
hist(all_data$jmax_tleaf_31)
hist(log(all_data$jmax_tleaf_31))
jmax_tleaf_31_lmer <- lmer(log(jmax_tleaf_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(jmax_tleaf_31_lmer) ~ fitted(jmax_tleaf_31_lmer))
summary(jmax_tleaf_31_lmer)
Anova(jmax_tleaf_31_lmer)
jmax_31_anova <- data.frame(Anova(jmax_tleaf_31_lmer))
jmax_31_anova
write.csv(jmax_31_anova, "Git/variability_ms_thesis/Tables/jmax31.csv")
emmeans(jmax_tleaf_31_lmer, ~TV)
emmeans(jmax_tleaf_31_lmer, ~TV*LV)
jmax_31_means <- data.frame(emmeans(jmax_tleaf_31_lmer, ~TV*LV))
jmax_31_means$treatment <- paste(jmax_31_means$TV, jmax_31_means$LV, sep = "")
jmax_31_means

jmax_tleaf_31_plot <- ggplot(aes(y=jmax_tleaf_31, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
jmax_tleaf_31_plot

### stomatal conductance (gsw_420_20)
hist(all_data$gsw_420_20)
hist(log(all_data$gsw_420_20))
gsw_420_20_lmer <- lmer(log(gsw_420_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_20_lmer) ~ fitted(gsw_420_20_lmer))
summary(gsw_420_20_lmer)
Anova(gsw_420_20_lmer)
emmeans(gsw_420_20_lmer, ~TV)
emmeans(gsw_420_20_lmer, ~TV*LV)
gsw_20_means <- data.frame(emmeans(gsw_420_20_lmer, ~TV*LV))
gsw_20_means$treatment <- paste(gsw_20_means$TV, gsw_20_means$LV, sep = "")
gsw_20_means

### stomatal conductance (gsw_420_25)
hist(all_data$gsw_420_25)
gsw_420_25_lmer <- lmer(log(gsw_420_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_25_lmer) ~ fitted(gsw_420_25_lmer))
summary(gsw_420_25_lmer)
Anova(gsw_420_25_lmer)
emmeans(gsw_420_25_lmer, ~TV)
emmeans(gsw_420_25_lmer, ~TV*LV)
gsw_25_means <- data.frame(emmeans(gsw_420_25_lmer, ~TV*LV))
gsw_25_means$treatment <- paste(gsw_25_means$TV, gsw_25_means$LV, sep = "")
gsw_25_means


### stomatal conductance (gsw_420_31)
hist(all_data$gsw_420_31)
gsw_420_31_lmer <- lmer(log(gsw_420_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(gsw_420_31_lmer) ~ fitted(gsw_420_31_lmer))
summary(gsw_420_31_lmer)
Anova(gsw_420_31_lmer)
emmeans(gsw_420_31_lmer, ~TV)
emmeans(gsw_420_31_lmer, ~TV*LV)
gsw_31_means <- data.frame(emmeans(gsw_420_31_lmer, ~TV*LV))
gsw_31_means$treatment <- paste(gsw_31_means$TV, gsw_31_means$LV, sep = "")
gsw_31_means

### net photosynthesis (anet_420_20)
hist(all_data$anet_420_20)
hist(log(all_data$anet_420_20))
anet_420_20_lmer <- lmer(log(anet_420_20) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(anet_420_20_lmer) ~ fitted(anet_420_20_lmer))
summary(anet_420_20_lmer)
Anova(anet_420_20_lmer)
emmeans(anet_420_20_lmer, ~TV)
emmeans(anet_420_20_lmer, ~TV*LV)
anet_20_means <- data.frame(emmeans(anet_420_20_lmer, ~TV*LV))
anet_20_means$treatment <- paste(anet_20_means$TV, anet_20_means$LV, sep = "")
anet_20_means

### net photosynthesis (anet_420_25)
hist(all_data$anet_420_25)
hist(log(all_data$anet_420_25))
anet_420_25_lmer <- lmer(log(anet_420_25) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(anet_420_25_lmer) ~ fitted(anet_420_25_lmer))
summary(anet_420_25_lmer)
Anova(anet_420_25_lmer)
emmeans(anet_420_25_lmer, ~TV)
emmeans(anet_420_25_lmer, ~TV*LV)
anet_25_means <- data.frame(emmeans(anet_420_25_lmer, ~TV*LV))
anet_25_means$treatment <- paste(anet_25_means$TV, anet_25_means$LV, sep = "")
anet_25_means

### net photosynthesis (anet_420_31)
hist(all_data$anet_420_31)
hist(log(all_data$anet_420_31))
anet_420_31_lmer <- lmer(log(anet_420_31) ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(anet_420_31_lmer) ~ fitted(anet_420_31_lmer))
summary(anet_420_31_lmer)
Anova(anet_420_31_lmer)
emmeans(anet_420_31_lmer, ~TV)
emmeans(anet_420_31_lmer, ~TV*LV)
anet_31_means <- data.frame(emmeans(anet_420_31_lmer, ~TV*LV))
anet_31_means$treatment <- paste(anet_31_means$TV, anet_31_means$LV, sep = "")
anet_31_means




#### Dark Respiration ##########################################################
### resp_20
hist(all_data$resp_20)
resp_20_lmer <- lmer(resp_20 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(resp_20_lmer) ~ fitted(resp_20_lmer))
summary(resp_20_lmer)
Anova(resp_20_lmer)
emmeans(resp_20_lmer, ~TV)
emmeans(resp_20_lmer, ~TV*LV)

### resp_25
hist(all_data$resp_25)
resp_25_lmer <- lmer(resp_25 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(resp_25_lmer) ~ fitted(resp_25_lmer))
summary(resp_25_lmer)
Anova(resp_25_lmer)
emmeans(resp_25_lmer, ~TV)
emmeans(resp_25_lmer, ~TV*LV)

### resp_31
hist(all_data$resp_31)
resp_31_lmer <- lmer(resp_31 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(resp_31_lmer) ~ fitted(resp_31_lmer))
summary(resp_31_lmer)
Anova(resp_31_lmer)
emmeans(resp_31_lmer, ~TV)
emmeans(resp_31_lmer, ~TV*LV)


#### Multispeq #################################################################

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



#### Chlorophyll ###############################################################
### total chlorophyll content (chl.mmolm2)
chl.mmolm2_lmer <- lmer(chl.mmolm2 ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(chl.mmolm2_lmer) ~ fitted(chl.mmolm2_lmer))
summary(chl.mmolm2_lmer)
Anova(chl.mmolm2_lmer)
total_chloro_anova <- data.frame(Anova(chl.mmolm2_lmer))
total_chloro_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/total_chloro_anova.csv")
emmeans(chl.mmolm2_lmer, ~TV)
emmeans(chl.mmolm2_lmer, ~TV*LV)
total_chloro_means <- data.frame(emmeans(chl.mmolm2_lmer, ~TV*LV))
total_chloro_means$treatment <- paste(total_chloro_means$TV, total_chloro_means$LV, sep = "")
total_chloro_means

chl.mmolm2_lmer_plot <- ggplot(aes(y=chl.mmolm2, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
chl.mmolm2_lmer_plot

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



#### Structural Traits #########################################################
### above_biomass_dry_weight
above_biomass_dry_weight_lmer <- lmer(above_biomass_dry_weight ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(above_biomass_dry_weight_lmer) ~ fitted(above_biomass_dry_weight_lmer))
summary(above_biomass_dry_weight_lmer)
Anova(above_biomass_dry_weight_lmer)
biomass_anova <- data.frame(Anova(above_biomass_dry_weight_lmer))
biomass_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/biomass_anova.csv")
emmeans(above_biomass_dry_weight_lmer, ~TV)
biomass_means <- data.frame(emmeans(above_biomass_dry_weight_lmer, ~TV*LV))
biomass_means$treatment <- paste(biomass_means$TV, biomass_means$LV, sep = "")
biomass_means

above_biomass_dry_weight_plot <- ggplot(aes(y=above_biomass_dry_weight, x = treatment), data = all_data) +
  geom_boxplot() + theme_bw()
above_biomass_dry_weight_plot

### SLA_focal
SLA_focal_lmer <- lmer(SLA_focal ~ TV*LV + (1|chamber_fac), data = all_data)
plot(resid(SLA_focal_lmer) ~ fitted(SLA_focal_lmer))
summary(SLA_focal_lmer)
Anova(SLA_focal_lmer)
sla_anova <- data.frame(Anova(SLA_focal_lmer))
sla_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/sla_anova.csv")
emmeans(SLA_focal_lmer, ~TV)
emmeans(SLA_focal_lmer, ~TV*LV)
sla_means <- data.frame(emmeans(SLA_focal_lmer, ~TV*LV))
sla_means$treatment <- paste(sla_means$TV, sla_means$LV, sep = "")
sla_means



#### 13C and 15N ##############################################################
### carbon 13 (c13)
c13_lmer <- lmer(c13 ~ TV*LV + (1|chamber_fac), data = all_data2)
plot(resid(c13_lmer) ~ fitted(c13_lmer))
summary(c13_lmer)
Anova(c13_lmer)
c13_anova <- data.frame(Anova(c13_lmer))
c13_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/c13_anova.csv")
emmeans(c13_lmer, ~TV)
emmeans(c13_lmer, ~TV*LV)
c13_means <- data.frame(emmeans(c13_lmer, ~TV*LV))
c13_means$treatment <- paste(c13_means$TV, c13_means$LV, sep = "")
c13_means

### nitrogen 15 (n15)
n15_lmer <- lmer(n15 ~ TV*LV + (1|chamber_fac), data = all_data2)
plot(resid(n15_lmer) ~ fitted(n15_lmer))
summary(n15_lmer)
Anova(n15_lmer)
emmeans(n15_lmer, ~TV)
emmeans(n15_lmer, ~TV*LV)


#### Total carbon and nitrogen ################################################
### total carbon (total_c)
total_c_lmer <- lmer(total_c ~ TV*LV + (1|chamber_fac), data = all_data2)
plot(resid(total_c_lmer) ~ fitted(total_c_lmer))
summary(total_c_lmer)
Anova(total_c_lmer)
emmeans(total_c_lmer, ~TV)
emmeans(total_c_lmer, ~TV*LV)

### total nitrogen (total_n)
total_n_lmer <- lmer(total_n ~ TV*LV + (1|chamber_fac), data = all_data2)
plot(resid(total_n_lmer) ~ fitted(total_n_lmer))
summary(total_n_lmer)
Anova(total_n_lmer)
total_n_anova <- data.frame(Anova(total_n_lmer))
total_n_anova
write.csv(vcmax_20_anova, "Git/variability_ms_thesis/Tables/total_n_anova.csv")
emmeans(total_n_lmer, ~TV)
emmeans(total_n_lmer, ~TV*LV)
total_n_means <- data.frame(emmeans(total_n_lmer, ~TV*LV))
total_n_means$treatment <- paste(total_n_means$TV, total_n_means$LV, sep = "")
total_n_means


# try new lme for photosynthesis traits #######################################
  # trait increased/decreased with increasing temperature but no significance in treatments
  # at each temperature

### vcmax ** tleaf has a sig effect **
vcmax20_df <- all_data[ , c("chamber_fac", "unique_id", "vcmax_tleaf_20", "TV", "LV")]
vcmax20_df$tleaf <- rep(20, each = 60)
colnames(vcmax20_df)[3] <- "vcmax"

vcmax25_df <- all_data[ , c("chamber_fac", "unique_id", "vcmax_tleaf_25", "TV", "LV")]
vcmax25_df$tleaf <- rep(25, each = 60)
colnames(vcmax25_df)[3] <- "vcmax"

vcmax31_df <- all_data[ , c("chamber_fac", "unique_id", "vcmax_tleaf_31", "TV", "LV")]
vcmax31_df$tleaf <- rep(31, each = 60)
colnames(vcmax31_df)[3] <- "vcmax"

all_vcmax <- rbind(vcmax20_df, vcmax25_df)
all_vcmax <- rbind(all_vcmax, vcmax31_df)

all_vcmax$tleaf_fac <- as.factor(all_vcmax$tleaf)

## new  vcmax lme #########
hist(all_vcmax$vcmax)
vcmax_lmer <- lmer(log(vcmax) ~ TV*LV*tleaf_fac + (1|chamber_fac) + (1|unique_id), data = all_vcmax)
plot(resid(vcmax_lmer) ~ fitted(vcmax_lmer))
summary(vcmax_lmer)
Anova(vcmax_lmer)
all_vcmax_means <- data.frame(emmeans(vcmax_lmer, ~TV*LV))
all_vcmax_means
emmeans(vcmax_lmer, ~tleaf_fac)

### jmax ** tleaf has a significant effect **
jmax20_df <- all_data[ , c("chamber_fac", "unique_id", "jmax_tleaf_20", "TV", "LV")]
jmax20_df$tleaf <- rep(20, each = 60)
colnames(jmax20_df)[3] <- "jmax"

jmax25_df <- all_data[ , c("chamber_fac", "unique_id", "jmax_tleaf_25", "TV", "LV")]
jmax25_df$tleaf <- rep(25, each = 60)
colnames(jmax25_df)[3] <- "jmax"

jmax31_df <- all_data[ , c("chamber_fac", "unique_id", "jmax_tleaf_31", "TV", "LV")]
jmax31_df$tleaf <- rep(31, each = 60)
colnames(jmax31_df)[3] <- "jmax"

all_jmax <- rbind(jmax20_df, jmax25_df)
all_jmax <- rbind(all_jmax, jmax31_df)

all_jmax$tleaf_fac <- as.factor(all_jmax$tleaf)

## new  jmax lme #########
hist(all_jmax$jmax)
jmax_lmer <- lmer(log(jmax) ~ TV*LV*tleaf_fac + (1|chamber_fac) + (1|unique_id), data = all_jmax)
plot(resid(jmax_lmer) ~ fitted(jmax_lmer))
summary(jmax_lmer)
Anova(jmax_lmer)
emmeans(jmax_lmer, ~TV*LV)
emmeans(jmax_lmer, ~tleaf_fac)

### gsw_420 ** tleaf has a significant effect **
gsw_20_df <- all_data[ , c("chamber_fac", "unique_id", "gsw_420_20", "TV", "LV")]
gsw_20_df$tleaf <- rep(20, each = 60)
colnames(gsw_20_df)[3] <- "gsw_420"

gsw_25_df <- all_data[ , c("chamber_fac", "unique_id", "gsw_420_25", "TV", "LV")]
gsw_25_df$tleaf <- rep(25, each = 60)
colnames(gsw_25_df)[3] <- "gsw_420"

gsw_31_df <- all_data[ , c("chamber_fac", "unique_id", "gsw_420_31", "TV", "LV")]
gsw_31_df$tleaf <- rep(31, each = 60)
colnames(gsw_31_df)[3] <- "gsw_420"

all_gsw_420 <- rbind(gsw_20_df, gsw_25_df)
all_gsw_420 <- rbind(all_gsw_420, gsw_31_df)

all_gsw_420$tleaf_fac <- as.factor(all_gsw_420$tleaf)

## new  gsw_420 lme #########
hist(all_gsw_420$gsw_420)
gsw_420_lmer <- lmer(log(gsw_420) ~ TV*LV*tleaf_fac + (1|chamber_fac) + (1|unique_id), data = all_gsw_420)
plot(resid(gsw_420_lmer) ~ fitted(gsw_420_lmer))
summary(gsw_420_lmer)
Anova(gsw_420_lmer)
emmeans(gsw_420_lmer, ~TV*LV)
emmeans(gsw_420_lmer, ~tleaf_fac)

### anet_420 ** tleaf has a significant effect **
anet_20_df <- all_data[ , c("chamber_fac", "unique_id", "anet_420_20", "TV", "LV")]
anet_20_df$tleaf <- rep(20, each = 60)
colnames(anet_20_df)[3] <- "anet_420"

anet_25_df <- all_data[ , c("chamber_fac", "unique_id", "anet_420_25", "TV", "LV")]
anet_25_df$tleaf <- rep(25, each = 60)
colnames(anet_25_df)[3] <- "anet_420"

anet_31_df <- all_data[ , c("chamber_fac", "unique_id", "anet_420_31", "TV", "LV")]
anet_31_df$tleaf <- rep(31, each = 60)
colnames(anet_31_df)[3] <- "anet_420"

all_anet_420 <- rbind(anet_20_df, anet_25_df)
all_anet_420 <- rbind(all_anet_420, anet_31_df)

all_anet_420$tleaf_fac <- as.factor(all_anet_420$tleaf)

## new anet_420 lme #########
hist(all_anet_420$anet_420)
anet_420_lmer <- lmer(log(anet_420) ~ TV*LV*tleaf_fac + (1|chamber_fac) + (1|unique_id), data = all_anet_420)
plot(resid(anet_420_lmer) ~ fitted(anet_420_lmer))
summary(anet_420_lmer)
Anova(anet_420_lmer)
emmeans(anet_420_lmer, ~TV*LV)
emmeans(anet_420_lmer, ~tleaf_fac)

### resp ** tleaf has significant impact **
resp_20_df <- all_data[ , c("chamber_fac", "unique_id", "resp_20", "TV", "LV")]
resp_20_df$tleaf <- rep(20, each = 60)
colnames(resp_20_df)[3] <- "resp"

resp_25_df <- all_data[ , c("chamber_fac", "unique_id", "resp_25", "TV", "LV")]
resp_25_df$tleaf <- rep(25, each = 60)
colnames(resp_25_df)[3] <- "resp"

resp_31_df <- all_data[ , c("chamber_fac", "unique_id", "resp_31", "TV", "LV")]
resp_31_df$tleaf <- rep(31, each = 60)
colnames(resp_31_df)[3] <- "resp"

all_resp <- rbind(resp_20_df, resp_25_df)
all_resp <- rbind(all_resp, resp_31_df)

all_resp$tleaf_fac <- as.factor(all_resp$tleaf)

## new resp lme ######### 
# if I want to log transform here do I need to take absolute value and then log??
hist(all_resp$resp)
resp_lmer <- lmer(log(abs(resp)) ~ TV*LV*tleaf_fac + (1|chamber_fac) + (1|unique_id), data = all_resp)
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~TV*LV)
emmeans(resp_lmer, ~tleaf_fac)


# graph results ###############################################################
### biomass ####
above_biomass_plot <- ggplot(aes(x = treatment, y = emmean), 
                             data = biomass_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=above_biomass_dry_weight, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  labs(x = expression(bold("Treatment")), y = expression(bold("Aboveground Biomass Dry Weight (g)")))
above_biomass_plot

### biomass 2 #####
biomass_plot2 <- ggplot(aes(x = treatment, y = emmean), 
                             data = biomass_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=above_biomass_dry_weight, x = treatment, 
                  color = LV), data = all_data, alpha = 0.6, size = 2) +
  theme_bw() + scale_x_discrete(labels = c("HTV", "HTV", "LTV", "LTV")) +
  labs(x = expression(bold("Temperature Variability")), y = expression(bold("Aboveground Biomass Dry Weight (g)")), 
       col = "Light Variability")
biomass_plot2

### sla #####
sla_plot <- ggplot(aes(x = treatment, y = emmean), 
                             data = sla_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=SLA_focal, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  labs(x = expression(bold("Treatment")), y = expression(bold("SLA (cm"^"2"*"/g)")))
sla_plot

### total chlorophyll ####
total_chloro_plot <- ggplot(aes(x = treatment, y = emmean), 
                   data = total_chloro_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=chl.mmolm2, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("Chl")["area"]*" (mmol m"^"-2"*")")))
total_chloro_plot

### carbon 13 #########
c13_plot <- ggplot(aes(x = treatment, y = emmean), 
                            data = c13_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=c13, x = treatment, 
                  color = treatment), data = all_data2, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  labs(x = expression(bold("Treatment")), y = expression(bold("13 C")*" ("*mu*"g"*")"))
c13_plot

### total nitrogen #########
total_n_plot <- ggplot(aes(x = treatment, y = emmean), 
                   data = total_n_means) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=total_n, x = treatment, 
                  color = treatment), data = all_data2, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  labs(x = expression(bold("Treatment")), y = expression(bold("N")["total"]*" ("*mu*"g"*")"))
total_n_plot

graph1 <- ggarrange(c13_plot, total_n_plot, total_chloro_plot, sla_plot, labels = c("a)", "b)", "c)", "d)"))
graph1

# photosynthesis graph #######################################################
### vcmax ##### 
# (make same y axis; separate temp and light variability)

vcmax_20_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                             data = vcmax_20_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=vcmax_tleaf_20, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(20, 180)) +
  scale_y_continuous(breaks = seq(20, 180, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("V")["cmax20"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
vcmax_20_plot

jpeg(filename = "Git/variability_ms_thesis/Graphs/vcmax20.jpg",
     width = 3, height = 3, units = "in", res = 300)
grid.newpage()
grid.draw(vcmax_20_plot)
dev.off()

vcmax_25_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                        data = vcmax_25_means) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=vcmax_tleaf_25, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(20, 140)) +
  scale_y_continuous(breaks = seq(20, 140, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("V")["cmax25"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
vcmax_25_plot

jpeg(filename = "Git/variability_ms_thesis/Graphs/vcmax25.jpg",
     width = 3, height = 3, units = "in", res = 300)
grid.newpage()
grid.draw(vcmax_25_plot)
dev.off()

vcmax_31_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                        data = vcmax_31_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.31,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=vcmax_tleaf_31, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(20, 180)) +
  scale_y_continuous(breaks = seq(20, 180, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("V")["cmax31"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
vcmax_31_plot

jpeg(filename = "Git/variability_ms_thesis/Graphs/vcmax31.jpg",
     width = 3, height = 3, units = "in", res = 300)
grid.newpage()
grid.draw(vcmax_31_plot)
dev.off()

vcmax <- ggarrange(vcmax_20_plot, vcmax_25_plot, vcmax_31_plot, ncol = 3, labels = c("a)", "b)", "c)"))
vcmax
jpeg(filename = "Git/variability_ms_thesis/Graphs/3vcmax.jpg",
     width = 8, height = 3, units = "in", res = 300)
grid.newpage()
grid.draw(vcmax)
dev.off()

## vcmax 25 plot 2 #######
vcmax_25_plot2 <- ggplot(aes(x = TV, y = exp(emmean)),
                     data = vcmax_25_means) +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.4,
                size = .7, position = position_dodge2()) +
  geom_point(aes(shape = LV), size = 2, position = position_dodge2(width = .4)) +
  
  geom_jitter(aes(x = TV, y=vcmax_tleaf_25,
                  color = TV, shape = LV), data =  all_data, alpha = 0.6, size = 1.5) +
  
  scale_shape_manual(values = c(17, 19)) +
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.title = element_text(size = 1)) +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(20, 140)) +
  scale_y_continuous(breaks = seq(20, 140, by = 20)) +
  
  labs(x = expression(bold("Temperature Variability Treatment")), 
       y = expression(bold(italic("V")["cmax25"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")),
       color = "Temperature Variability", shape = "Light Variability") +
  theme_bw()
  
vcmax_25_plot2

jpeg(filename = "Git/variability_ms_thesis/Graphs/vcmax25_2.jpg",
     width = 5, height = 3, units = "in", res = 300)
grid.newpage()
grid.draw(vcmax_25_plot2)
dev.off()

theme(legend.title = element_text(size))

### jmax ########
jmax_20_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                        data = jmax_20_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=jmax_tleaf_20, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(35, 195)) +
  scale_y_continuous(breaks = seq(35, 195, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("J")["max20"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
jmax_20_plot

jmax_25_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                        data = jmax_25_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=jmax_tleaf_25, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(35, 195)) +
  scale_y_continuous(breaks = seq(35, 195, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("J")["mmax25"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
jmax_25_plot

jmax_31_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                        data = jmax_31_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.31,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=jmax_tleaf_31, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(35, 195)) +
  scale_y_continuous(breaks = seq(35, 195, by = 20)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("J")["max31"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
jmax_31_plot

jmax <- ggarrange(jmax_20_plot, jmax_25_plot, jmax_31_plot, ncol = 3, labels = c("a)", "b)", "c)"))

## gsw_420 ########
gsw_20_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                       data = gsw_20_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=gsw_420_20, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("gsw")["420_20"]*" (mol m"^"-2"*" s"^"-1"*")")))
gsw_20_plot

gsw_25_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                      data = gsw_25_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=gsw_420_25, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("gsw")["420_25"]*" (mol m"^"-2"*" s"^"-1"*")")))
gsw_25_plot

gsw_31_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                      data = gsw_31_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.31,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=gsw_420_31, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("gsw")["420_31"]*" (mol m"^"-2"*" s"^"-1"*")")))
gsw_31_plot

gsw <- ggarrange(gsw_20_plot, gsw_25_plot, gsw_31_plot, ncol = 3, labels = c("a)", "b)", "c)"))

## anet_420 ########
anet_20_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                      data = anet_20_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=anet_420_20, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 25)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("anet")["420_20"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
anet_20_plot

anet_25_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                      data = anet_25_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.25,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=anet_420_25, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 25)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("anet")["420_25"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
anet_25_plot

anet_31_plot <- ggplot(aes(x = treatment, y = exp(emmean)), 
                      data = anet_31_means) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(emmean - SE), ymax = exp(emmean + SE)), width = 0.31,
                position = position_dodge(width = 0.5)) +
  geom_jitter(aes(y=anet_420_31, x = treatment, 
                  color = treatment), data = all_data, alpha = 0.6, size = 1.5) +
  theme_bw() + theme(legend.position="none") +
  theme(axis.text = element_text(size = 7)) +
  coord_cartesian(ylim = c(0, 25)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs(x = expression(bold("Treatment")), y = expression(bold(italic("anet")["420_31"]*" ("*mu*"mol m"^"-2"*" s"^"-1"*")")))
anet_31_plot

anet <- ggarrange(anet_20_plot, anet_25_plot, anet_31_plot, ncol = 3, labels = c("a)", "b)", "c)"))


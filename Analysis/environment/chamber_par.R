# script to average chamber PAR conditions

## load packages
library(tidyverse)
library(reshape2)
library(ggpubr)
library(grid)

## load data
par_data <- read.csv("Git/variability_ms_thesis/Data/environmental/par_sensors/cleaned_par.csv")
head(par_data)

## Separate date.time column ###################################################
# pre: before ppf 1, 4, 5, 6 switched to other chambers
# post: after ppf 1, 4, 5, 6 switched to other chambers
par_data$date.pre <- as.Date(par_data$date.time.pre, format = "%m/%d/%Y %H:%M")
par_data$time.pre <- format(as.POSIXct(par_data$date.time.pre, format="%m/%d/%Y %H:%M"), "%H:%M")

par_data$date.post <- as.Date(par_data$date.time.post, format = "%m/%d/%Y %H:%M")
par_data$time.post <- format(as.POSIXct(par_data$date.time.post, format="%m/%d/%Y %H:%M"), "%H:%M")

## Script below gets mean, sd, se for each hour per chamber ##################
par_data$hour <- format(as.POSIXct(par_data$time.pre, format = " %H:%M"), "%H")

select_par_data <- par_data[ , c(2:9, 11:14, 19)]

par_data_long <- melt(select_par_data, id = "hour", variable.name = "treatment", value.name = "par_value")

par_data_groupby <- par_data_long %>%
  group_by(hour, treatment) %>%
  summarise(par_mean = mean(par_value, na.rm = TRUE), par_sd = sd(par_value, na.rm = TRUE), 
            par_se = sd(par_value, na.rm = T)/sqrt(length(par_value)))
par_data_groupby

## Graph each treatment in each chamber ########################################
par_avg_hour_cham_plot <- ggplot(par_data_groupby, aes(x = as.numeric(hour), y = par_mean, color = treatment)) + 
  geom_line() + theme_bw() + scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "Hours in a Day (00-23)", y = "Average PAR")
par_avg_hour_cham_plot


## Combine treatment averages per hour #########################################
select_par_data$all_par_mean_HTVHLV <- rowMeans(select_par_data[ ,c(1, 2, 11)])
select_par_data$all_par_mean_HTVLLV <- rowMeans(select_par_data[ ,c(3, 5, 9)])
select_par_data$all_par_mean_LTVHLV <- rowMeans(select_par_data[ ,c(6, 7, 12)])
select_par_data$all_par_mean_LTVLLV <- rowMeans(select_par_data[ ,c(4, 8, 10)])

par_hour_avg <- select_par_data[, c(13:17)]
par_hour_avg

par_hour_avg_long <- melt(par_hour_avg, id = "hour", variable.name = "treatment", value.name = "par_value")

## Script below gets mean, sd, se for each hour per treatment ##################
par_hour_avg_groupby <- par_hour_avg_long %>%
  group_by(hour, treatment) %>%
  summarise(par_mean = mean(par_value, na.rm = TRUE), par_sd = sd(par_value, na.rm = TRUE), 
            par_se = sd(par_value, na.rm = T)/sqrt(length(par_value)))
par_hour_avg_groupby

# seperate hours for use in FvCB
HTVHLV_hour <- par_hour_avg_groupby %>% filter(grepl("all_par_mean_HTVHLV", treatment))
write.csv(HTVHLV_hour, "Git/variability_ms_thesis/Data/environmental/par_sensors/HTVHLV_hour.csv")

HTVLLV_hour <- par_hour_avg_groupby %>% filter(grepl("all_par_mean_HTVLLV", treatment))
write.csv(HTVLLV_hour, "Git/variability_ms_thesis/Data/environmental/par_sensors/HTVLLV_hour.csv")

LTVHLV_hour <- par_hour_avg_groupby %>% filter(grepl("all_par_mean_LTVHLV", treatment))
write.csv(LTVHLV_hour, "Git/variability_ms_thesis/Data/environmental/par_sensors/LTVHLV_hour.csv")

LTVLLV_hour <- par_hour_avg_groupby %>% filter(grepl("all_par_mean_LTVLLV", treatment))
write.csv(LTVLLV_hour, "Git/variability_ms_thesis/Data/environmental/par_sensors/LTVLLV_hour.csv")

## Graph each treatment ########################################################
par_avg_hour_plot <- ggplot(par_hour_avg_groupby, aes(x = as.numeric(hour), y = par_mean, color = treatment)) + 
  geom_line() + theme_bw() + scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "Hours in a Day (00-23)", y = expression("PAR (µmol m"^"-2"*"s"^"-1"*")"), color = "Treatment") +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  scale_color_manual(values = c("red", "orange", "blue", "lightseagreen"),labels = c("HTVHLV", "HTVLLV", "LTVHLV", "LTVLLV") ) +
  geom_hline(aes(yintercept=386.97), color="red", linetype="dashed")+
  geom_hline(aes(yintercept=347.26), color="orange", linetype="longdash")+
  geom_hline(aes(yintercept=370.39), color="blue", linetype="dashed")+
  geom_hline(aes(yintercept=347.88), color="lightseagreen", linetype="dashed")+
  theme(legend.title  = element_text(size = 7))+
  theme(legend.text = element_text(size = 5))
par_avg_hour_plot

jpeg(filename = "Git/variability_ms_thesis/Graphs/par.jpg",
     width = 5, height = 2.7, units = "in", res = 300)
grid.newpage()
grid.draw(par_avg_hour_plot)
dev.off()



##############################################################################
# data for average by day ######
pre_par <- read.csv("Git/variability_ms_thesis/Data/environmental/par_sensors/pre_switch_cleaned_par.csv")
post_par <- read.csv("Git/variability_ms_thesis/Data/environmental/par_sensors/post_switch_cleaned_par.csv")
# Script below gets mean, sd, se for each day per treatment ##################
# add day and hour column
pre_par$day <- format(as.POSIXct(pre_par$date.time.pre, format = "%m/%d/%Y %H:%M"), "%m/%d")
pre_par$hour <- format(as.POSIXct(pre_par$date.time.pre, format = "%m/%d/%Y %H:%M"), "%H")
pre_par_daytime <- pre_par %>% filter(hour >= "06" & hour <= "22")

post_par$day <- format(as.POSIXct(post_par$date.time.post, format = "%m/%d/%Y %H:%M"), "%m/%d")
post_par$hour <- format(as.POSIXct(post_par$date.time.post, format = "%m/%d/%Y %H:%M"), "%H")
post_par_daytime <- post_par %>% filter(hour >= "06" & hour <= "22")

# long format
select_pre_par <- pre_par_daytime[ , c(2:10)]
pre_par_long <- melt(select_pre_par, id = "day", variable.name = "treatment", value.name = "par_value")

pre_par_day_groupby <- pre_par_long %>%
  group_by(day, treatment) %>%
  summarise(par_mean = mean(par_value, na.rm = TRUE), par_sd = sd(par_value, na.rm = TRUE), 
            par_se = sd(par_value, na.rm = T)/sqrt(length(par_value)))
pre_par_day_groupby

select_post_par <- post_par_daytime[ , c(2:6)]
post_par_long <- melt(select_post_par, id = "day", variable.name = "treatment", value.name = "par_value")
post_par_day_groupby <- post_par_long %>%
  group_by(day, treatment) %>%
  summarise(par_mean = mean(par_value, na.rm = TRUE), par_sd = sd(par_value, na.rm = TRUE), 
            par_se = sd(par_value, na.rm = T)/sqrt(length(par_value)))
post_par_day_groupby

# now need to average by day for each treatment ##########
daytime_par <- full_join(pre_par_day_groupby, post_par_day_groupby)
daytime_par

# separate by treatment and get average, sd, and se
  # HTVHLV (chambers 1, 5, 6)
htvhlv_par1 <- daytime_par[grep("ppf1.ch5.hlv", daytime_par$treatment), ]
htvhlv_par2 <- daytime_par[grep("ppf2.ch6.hlv", daytime_par$treatment), ]
htvhlv_par3 <- daytime_par[grep("ppf5.ch1.hlv", daytime_par$treatment), ]
htvhlv_par <- full_join(htvhlv_par1, htvhlv_par2)
htvhlv_par <- full_join(htvhlv_par, htvhlv_par3)

HTVHLV.par.mean <- mean(htvhlv_par$par_mean)
HTVHLV.par.sd <- sd(htvhlv_par$par_mean)
HTVHLV.par.se <- sd(htvhlv_par$par_mean)/sqrt(length(htvhlv_par$par_mean))

  # HTVLLV (chambers 1, 5, 6)
htvllv_par1 <- daytime_par[grep("ppf1.ch1.llv", daytime_par$treatment), ]
htvllv_par2 <- daytime_par[grep("ppf3.ch6.llv", daytime_par$treatment), ]
htvllv_par3 <- daytime_par[grep("ppf5.ch5.llv", daytime_par$treatment), ]
htvllv_par <- full_join(htvllv_par1, htvllv_par2)
htvllv_par <- full_join(htvllv_par, htvllv_par3)

HTVLLV.par.mean <- mean(htvllv_par$par_mean)
HTVLLV.par.sd <- sd(htvllv_par$par_mean)
HTVLLV.par.se <- sd(htvllv_par$par_mean)/sqrt(length(htvllv_par$par_mean))

  # LTVHLV (chambers 2, 3, 4)
ltvhlv_par1 <- daytime_par[grep("ppf6.ch2.hlv", daytime_par$treatment), ]
ltvhlv_par2 <- daytime_par[grep("ppf7.ch3.hlv", daytime_par$treatment), ]
ltvhlv_par3 <- daytime_par[grep("ppf6.ch4.hlv", daytime_par$treatment), ]
ltvhlv_par <- full_join(ltvhlv_par1, ltvhlv_par2)
ltvhlv_par <- full_join(ltvhlv_par, ltvhlv_par3)

LTVHLV.par.mean <- mean(ltvhlv_par$par_mean)
LTVHLV.par.sd <- sd(ltvhlv_par$par_mean)
LTVHLV.par.se <- sd(ltvhlv_par$par_mean)/sqrt(length(ltvhlv_par$par_mean))

  # LTVLLV (chambers 2, 3, 4)
ltvllv_par1 <- daytime_par[grep("ppf4.ch2.llv", daytime_par$treatment), ]
ltvllv_par2 <- daytime_par[grep("ppf8.ch3.llv", daytime_par$treatment), ]
ltvllv_par3 <- daytime_par[grep("ppf4.ch4.llv", daytime_par$treatment), ]
ltvllv_par <- full_join(ltvllv_par1, ltvllv_par2)
ltvllv_par <- full_join(ltvllv_par, ltvllv_par3)

LTVLLV.par.mean <- mean(ltvllv_par$par_mean)
LTVLLV.par.sd <- sd(ltvllv_par$par_mean)
LTVLLV.par.se <- sd(ltvllv_par$par_mean)/sqrt(length(ltvllv_par$par_mean))


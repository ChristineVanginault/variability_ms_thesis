# script to average chamber PAR conditions

## load packages
library(tidyverse)

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
select_par_data$all_par_mean_HLV <- rowMeans(select_par_data[ ,c(1, 2, 6, 7, 11, 12)])
select_par_data$all_par_mean_LLV <- rowMeans(select_par_data[ ,c(3, 4, 5, 8, 9, 10)])

par_hour_avg <- select_par_data[, c(13:15)]
par_hour_avg

par_hour_avg_long <- melt(par_hour_avg, id = "hour", variable.name = "treatment", value.name = "par_value")

## Script below gets mean, sd, se for each hour per treatment ##################
par_hour_avg_groupby <- par_hour_avg_long %>%
  group_by(hour, treatment) %>%
  summarise(par_mean = mean(par_value, na.rm = TRUE), par_sd = sd(par_value, na.rm = TRUE), 
            par_se = sd(par_value, na.rm = T)/sqrt(length(par_value)))
par_hour_avg_groupby

## Graph each treatment ########################################################
par_avg_hour_plot <- ggplot(par_hour_avg_groupby, aes(x = as.numeric(hour), y = par_mean, color = treatment)) + 
  geom_line() + theme_bw() + scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "Hours in a Day (00-23)", y = "Average PAR")
par_avg_hour_plot


## Still need to find the daily average #######################################
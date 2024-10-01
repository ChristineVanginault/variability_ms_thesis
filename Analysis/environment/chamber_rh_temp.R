# script to average chamber rh & temp 

## load packages
library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)



## load data
chamber_1_HLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 1 2024-07-25 11_10_01 CDT (Data CDT).xlsx")
chamber_1_LLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 1 LLV 2024-07-25 11_19_43 CDT (Data CDT).xlsx")

chamber_2_HLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 2 2024-07-25 11_18_53 CDT (Data CDT).xlsx")
chamber_2_LLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 2 LLV 2024-07-25 11_19_31 CDT (Data CDT).xlsx")

chamber_3_HLV_1 <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 3 2024-06-21 10_34_44 CDT (Data CDT).xlsx")
chamber_3_HLV_2 <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 3 2024-07-09 15_45_33 CDT (Data CDT).xlsx")
chamber_3_HLV_3 <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 3 2024-07-25 11_20_56 CDT (Data CDT).xlsx")
chamber_3_LLV <-  read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 3 LLV 2024-07-25 11_20_11 CDT (Data CDT).xlsx")

chamber_4_HLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 4 2024-07-25 11_20_23 CDT (Data CDT).xlsx")
chamber_4_LLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 4 LLV 2024-07-25 11_18_40 CDT (Data CDT).xlsx")

chamber_5_HLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 5 2024-07-25 11_20_00 CDT (Data CDT).xlsx")
chamber_5_LLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 5 LLV 2024-07-25 11_19_18 CDT (Data CDT).xlsx")

chamber_6_HLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 6 2024-07-25 11_21_11 CDT (Data CDT)(1).xlsx")
chamber_6_LLV <- read_excel("Git/variability_ms_thesis/Data/environmental/rh_temp/Chamber 6 LLV 2024-07-25 11_19_07 CDT (Data CDT).xlsx")

## Change column names #########################################################
colnames(chamber_1_HLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_1_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")

colnames(chamber_2_HLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_2_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")

colnames(chamber_3_HLV_1)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_3_HLV_2)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_3_HLV_3)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_3_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")

colnames(chamber_4_HLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_4_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")

colnames(chamber_5_HLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_5_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")

colnames(chamber_6_HLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")
colnames(chamber_6_LLV)[c(2, 3, 4, 5)] <- c("Date.Time","Temperature", "RH", "Dew_Point")


## Separate Date-Time column ###################################################
chamber_1_HLV$Date <- as.Date(chamber_1_HLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_1_HLV$Time <- format(as.POSIXct(chamber_1_HLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_1_LLV$Date <- as.Date(chamber_1_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_1_LLV$Time <- format(as.POSIXct(chamber_1_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_2_HLV$Date <- as.Date(chamber_2_HLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_2_HLV$Time <- format(as.POSIXct(chamber_2_HLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_2_LLV$Date <- as.Date(chamber_2_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_2_LLV$Time <- format(as.POSIXct(chamber_2_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_3_HLV_1$Date <- as.Date(chamber_3_HLV_1$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_3_HLV_1$Time <- format(as.POSIXct(chamber_3_HLV_1$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")
chamber_3_HLV_2$Date <- as.Date(chamber_3_HLV_2$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_3_HLV_2$Time <- format(as.POSIXct(chamber_3_HLV_2$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")
chamber_3_HLV_3$Date <- as.Date(chamber_3_HLV_3$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_3_HLV_3$Time <- format(as.POSIXct(chamber_3_HLV_3$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_3_LLV$Date <- as.Date(chamber_3_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_3_LLV$Time <- format(as.POSIXct(chamber_3_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_4_HLV$Date <- as.Date(chamber_4_HLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_4_HLV$Time <- format(as.POSIXct(chamber_4_HLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_4_LLV$Date <- as.Date(chamber_4_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_4_LLV$Time <- format(as.POSIXct(chamber_4_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_5_HLV$Date <- as.Date(chamber_5_HLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_5_HLV$Time <- format(as.POSIXct(chamber_5_HLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_5_LLV$Date <- as.Date(chamber_5_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_5_LLV$Time <- format(as.POSIXct(chamber_5_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_6_HLV$Date <- as.Date(chamber_6_HLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_6_HLV$Time <- format(as.POSIXct(chamber_6_HLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

chamber_6_LLV$Date <- as.Date(chamber_6_LLV$Date.Time, format = "%Y-%m-%d %H:%M:%S")
chamber_6_LLV$Time <- format(as.POSIXct(chamber_6_LLV$Date.Time, format="%Y-%m-%d %H:%M:%S"), "%H:%M")

## Filter data for exp. dates ##################################################
## Experiment ran from June 3rd 2024 to July 17th 2024; need to filter data for
## those dates
chamber_1_HLV_exp <- chamber_1_HLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_1_LLV_exp <- chamber_1_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

chamber_2_HLV_exp <- chamber_2_HLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_2_LLV_exp <- chamber_2_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

chamber_3_HLV_1_exp <- chamber_3_HLV_1 %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_3_HLV_2_exp <- chamber_3_HLV_2 %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_3_HLV_3_exp <- chamber_3_HLV_3 %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_3_LLV_exp <- chamber_3_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

chamber_4_HLV_exp <- chamber_4_HLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_4_LLV_exp <- chamber_4_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

chamber_5_HLV_exp <- chamber_5_HLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_5_LLV_exp <- chamber_5_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

chamber_6_HLV_exp <- chamber_6_HLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")
chamber_6_LLV_exp <- chamber_6_LLV %>% filter(Date >= "2024-06-03" & Date <= "2024-07-17")

## Now we can merge chamber 3 HLV together
chamber_3_HLV_exp <- full_join(chamber_3_HLV_1_exp, chamber_3_HLV_2_exp)
chamber_3_HLV_exp <- full_join(chamber_3_HLV_exp, chamber_3_HLV_3_exp)


## Script below gets mean, sd, se for each hour per treatment ##################
class(chamber_1_HLV_exp$Date.Time) # R does not recognize "Time" as time

chamber_1_HLV_exp$hour <- format(as.POSIXct(chamber_1_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_1_HLV_exp_groupby <- group_by(chamber_1_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_1_LLV_exp$hour <- format(as.POSIXct(chamber_1_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_1_LLV_exp_groupby <- group_by(chamber_1_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))
            
chamber_2_HLV_exp$hour <- format(as.POSIXct(chamber_2_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_2_HLV_exp_groupby <- group_by(chamber_2_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_2_LLV_exp$hour <- format(as.POSIXct(chamber_2_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_2_LLV_exp_groupby <- group_by(chamber_2_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_3_HLV_exp$hour <- format(as.POSIXct(chamber_3_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_3_HLV_exp_groupby <- group_by(chamber_3_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_3_LLV_exp$hour <- format(as.POSIXct(chamber_3_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_3_LLV_exp_groupby <- group_by(chamber_3_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_4_HLV_exp$hour <- format(as.POSIXct(chamber_4_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_4_HLV_exp_groupby <- group_by(chamber_4_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_4_LLV_exp$hour <- format(as.POSIXct(chamber_4_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_4_LLV_exp_groupby <- group_by(chamber_4_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_5_HLV_exp$hour <- format(as.POSIXct(chamber_5_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_5_HLV_exp_groupby <- group_by(chamber_5_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_5_LLV_exp$hour <- format(as.POSIXct(chamber_5_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_5_LLV_exp_groupby <- group_by(chamber_5_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_6_HLV_exp$hour <- format(as.POSIXct(chamber_6_HLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_6_HLV_exp_groupby <- group_by(chamber_6_HLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))

chamber_6_LLV_exp$hour <- format(as.POSIXct(chamber_6_LLV_exp$Date.Time, format = "%Y-%m-%d %H:%M:%S"), "%H")
chamber_6_LLV_exp_groupby <- group_by(chamber_6_LLV_exp, hour) %>%
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            Temp_SE = sd(Temperature, na.rm = T)/sqrt(length(Temperature)),
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE),
            RH_SE = sd(RH, na.rm = T)/sqrt(length(RH)))


## Graph each treatment/chamber by hour ########################################

chamber_1_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_1_HLV_exp_groupby) +
  geom_line()
chamber_1_HLV_plot

chamber_1_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_1_LLV_exp_groupby) +
  geom_line()
chamber_1_LLV_plot

chamber_2_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_2_HLV_exp_groupby) +
  geom_line()
chamber_2_HLV_plot

chamber_2_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_2_LLV_exp_groupby) +
  geom_line()
chamber_2_LLV_plot

chamber_3_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_3_HLV_exp_groupby) +
  geom_line()
chamber_3_HLV_plot

chamber_3_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_3_LLV_exp_groupby) +
  geom_line()
chamber_3_LLV_plot

chamber_4_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_4_HLV_exp_groupby) +
  geom_line()
chamber_4_HLV_plot

chamber_4_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_4_LLV_exp_groupby) +
  geom_line()
chamber_4_LLV_plot

chamber_5_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_5_HLV_exp_groupby) +
  geom_line()
chamber_5_HLV_plot

chamber_5_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_5_LLV_exp_groupby) +
  geom_line()
chamber_5_LLV_plot

chamber_6_HLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_6_HLV_exp_groupby) +
  geom_line()
chamber_6_HLV_plot

chamber_6_LLV_plot <- ggplot(aes(y=Temp_Mean, x = as.numeric(hour)), data = chamber_6_LLV_exp_groupby) +
  geom_line()
chamber_6_LLV_plot



## Script below finds means and sd for each treatment ##########################

# High Temp Variability
# Chambers 1, 5, 6

# Low Temp Variability
# Chamber 2, 3, 4
#### LTVLLV #####################################################################
## Filter data for LTVLLV treatments; then combine for each step; calc mean and sd
# Day
## Step 1: 6am to 8am (22 C)
chamber_2_LLV_exp_step1 <- chamber_2_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_3_LLV_exp_step1 <- chamber_3_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_4_LLV_exp_step1 <- chamber_4_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")

# Combine
combine_LTVLLV_step1 <- bind_rows(chamber_2_LLV_exp_step1, chamber_3_LLV_exp_step1, chamber_4_LLV_exp_step1)


# Mean & SD
LTVLLV_step1_stats <- combine_LTVLLV_step1 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 2: 8am to 8pm (26 C)
chamber_2_LLV_exp_step2 <- chamber_2_LLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
chamber_3_LLV_exp_step2 <- chamber_3_LLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
chamber_4_LLV_exp_step2 <- chamber_4_LLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
# Combine
combine_LTVLLV_step2 <- bind_rows(chamber_2_LLV_exp_step2, 
                               chamber_3_LLV_exp_step2, chamber_4_LLV_exp_step2)


# Mean & SD
LTVLLV_step2_stats <- combine_LTVLLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


## Step 3: 8pm to 10pm (22 C)
chamber_2_LLV_exp_step3 <- chamber_2_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_3_LLV_exp_step3 <- chamber_3_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_4_LLV_exp_step3 <- chamber_4_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_LTVLLV_step3 <- bind_rows(chamber_2_LLV_exp_step3, 
                               chamber_3_LLV_exp_step3, chamber_4_LLV_exp_step3)


# Mean & SD
LTVLLV_step3_stats <- combine_LTVLLV_step3 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

# Night
## Night: 10pm to 6am (18 C)
chamber_2_LLV_exp_night <- chamber_2_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_3_LLV_exp_night <- chamber_3_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_4_LLV_exp_night <- chamber_4_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
# Combine
combine_LTVLLV_night <- bind_rows(chamber_2_LLV_exp_night, 
                               chamber_3_LLV_exp_night, chamber_4_LLV_exp_night)


# Mean & SD
LTVLLV_night_stats <- combine_LTVLLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

#### LTVHLV ###################################################################
## Filter data for LTVHLV treatments; then combine for each step; calc mean and sd
# Day
## Step 1: 6am to 8am (22 C)
chamber_2_HLV_exp_step1 <- chamber_2_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_3_HLV_exp_step1 <- chamber_3_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_4_HLV_exp_step1 <- chamber_4_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")

# Combine
combine_LTVHLV_step1 <- bind_rows(chamber_2_HLV_exp_step1, chamber_3_HLV_exp_step1, chamber_4_HLV_exp_step1)


# Mean & SD
LTVHLV_step1_stats <- combine_LTVHLV_step1 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 2: 8am to 8pm (26 C)
chamber_2_HLV_exp_step2 <- chamber_2_HLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
chamber_3_HLV_exp_step2 <- chamber_3_HLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
chamber_4_HLV_exp_step2 <- chamber_4_HLV_exp %>% filter(Time >= "08:00" & Time <= "20:00")
# Combine
combine_LTVHLV_step2 <- bind_rows(chamber_2_HLV_exp_step2, 
                                  chamber_3_HLV_exp_step2, chamber_4_HLV_exp_step2)



# Mean & SD
LTVHLV_step2_stats <- combine_LTVHLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


## Step 3: 8pm to 10pm (22 C)
chamber_2_HLV_exp_step3 <- chamber_2_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_3_HLV_exp_step3 <- chamber_3_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_4_HLV_exp_step3 <- chamber_4_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_LTVHLV_step3 <- bind_rows(chamber_2_HLV_exp_step3, 
                                  chamber_3_HLV_exp_step3, chamber_4_HLV_exp_step3)


# Mean & SD
LTVHLV_step3_stats <- combine_LTVHLV_step3 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

# Night
## Night: 10pm to 6am (18 C)
chamber_2_HLV_exp_night <- chamber_2_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_3_HLV_exp_night <- chamber_3_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_4_HLV_exp_night <- chamber_4_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
# Combine
combine_LTVHLV_night <- bind_rows(chamber_2_HLV_exp_night, 
                                  chamber_3_HLV_exp_night, chamber_4_HLV_exp_night)


# Mean & SD
LTVHLV_night_stats <- combine_LTVHLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


#### HTVHLV ####################################################################
## Filter data for HTVHLV treatments; combine each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_1_HLV_exp_step1 <- chamber_1_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_5_HLV_exp_step1 <- chamber_5_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_6_HLV_exp_step1 <- chamber_6_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
# Combine
combine_HTVHLV_step1 <- bind_rows(chamber_1_HLV_exp_step1, 
                               chamber_5_HLV_exp_step1, chamber_6_HLV_exp_step1)



# Mean & SD
HTVHLV_step1_stats <- combine_HTVHLV_step1 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 2: 8am to 10am (23 C)
chamber_1_HLV_exp_step2 <- chamber_1_HLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
chamber_5_HLV_exp_step2 <- chamber_5_HLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
chamber_6_HLV_exp_step2 <- chamber_6_HLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
# Combine
combine_HTVHLV_step2 <- bind_rows(chamber_1_HLV_exp_step2, 
                               chamber_5_HLV_exp_step2, chamber_6_HLV_exp_step2)


# Mean & SD
HTVHLV_step2_stats <- combine_HTVHLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 3: 10am to 12pm (26 C)
chamber_1_HLV_exp_step3 <- chamber_1_HLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
chamber_5_HLV_exp_step3 <- chamber_5_HLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
chamber_6_HLV_exp_step3 <- chamber_6_HLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
# Combine
combine_HTVHLV_step3 <- bind_rows(chamber_1_HLV_exp_step3,
                               chamber_5_HLV_exp_step3, chamber_6_HLV_exp_step3)


# Mean & SD
HTVHLV_step3_stats <- combine_HTVHLV_step3 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 4: 12pm to 4pm (31 C)
chamber_1_HLV_exp_step4 <- chamber_1_HLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
chamber_5_HLV_exp_step4 <- chamber_5_HLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
chamber_6_HLV_exp_step4 <- chamber_6_HLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
# Combine
combine_HTVHLV_step4 <- bind_rows(chamber_1_HLV_exp_step4,
                               chamber_5_HLV_exp_step4, chamber_6_HLV_exp_step4)


# Mean & SD
HTVHLV_step4_stats <- combine_HTVHLV_step4 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 5: 4pm to 6pm (26 C)
chamber_1_HLV_exp_step5 <- chamber_1_HLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
chamber_5_HLV_exp_step5 <- chamber_5_HLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
chamber_6_HLV_exp_step5 <- chamber_6_HLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
# Combine
combine_HTVHLV_step5 <- bind_rows(chamber_1_HLV_exp_step5,
                               chamber_5_HLV_exp_step5, chamber_6_HLV_exp_step5)


# Mean & SD
HTVHLV_step5_stats <- combine_HTVHLV_step5 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 6: 6pm to 8pm (23 C)
chamber_1_HLV_exp_step6 <- chamber_1_HLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
chamber_5_HLV_exp_step6 <- chamber_5_HLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
chamber_6_HLV_exp_step6 <- chamber_6_HLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
# Combine
combine_HTVHLV_step6 <- bind_rows(chamber_1_HLV_exp_step6,
                               chamber_5_HLV_exp_step6, chamber_6_HLV_exp_step6)



# Mean & SD
HTVHLV_step6_stats <- combine_HTVHLV_step6 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 7: 8pm to 10pm (20 C)
chamber_1_HLV_exp_step7 <- chamber_1_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_5_HLV_exp_step7 <- chamber_5_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_6_HLV_exp_step7 <- chamber_6_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_HTVHLV_step7 <- bind_rows(chamber_1_HLV_exp_step7,
                               chamber_5_HLV_exp_step7, chamber_6_HLV_exp_step7)


# Mean & SD
HTVHLV_step7_stats <- combine_HTVHLV_step7 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


# Night
## 10pm to 6am (18 C)
chamber_1_HLV_exp_night <- chamber_1_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_5_HLV_exp_night <- chamber_5_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_6_HLV_exp_night <- chamber_6_HLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
# Combine
combine_HTVHLV_night <- bind_rows(chamber_1_HLV_exp_night, 
                               chamber_5_HLV_exp_night, chamber_6_HLV_exp_night)


# Mean & SD
HTVHLV_night_stats <- combine_HTVHLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

#### HTVLLV ####################################################################
## Filter data for HTVLLV treatments; combine each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_1_LLV_exp_step1 <- chamber_1_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_5_LLV_exp_step1 <- chamber_5_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_6_LLV_exp_step1 <- chamber_6_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
# Combine
combine_HTVLLV_step1 <- bind_rows(chamber_1_LLV_exp_step1, 
                                  chamber_5_LLV_exp_step1, chamber_6_LLV_exp_step1)


# Mean & SD
HTVLLV_step1_stats <- combine_HTVLLV_step1 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 2: 8am to 10am (23 C)
chamber_1_LLV_exp_step2 <- chamber_1_LLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
chamber_5_LLV_exp_step2 <- chamber_5_LLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
chamber_6_LLV_exp_step2 <- chamber_6_LLV_exp %>% filter(Time >= "08:00" & Time <= "10:00")
# Combine
combine_HTVLLV_step2 <- bind_rows(chamber_1_LLV_exp_step2, 
                                  chamber_5_LLV_exp_step2, chamber_6_LLV_exp_step2)


# Mean & SD
HTVLLV_step2_stats <- combine_HTVLLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 3: 10am to 12pm (26 C)
chamber_1_LLV_exp_step3 <- chamber_1_LLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
chamber_5_LLV_exp_step3 <- chamber_5_LLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
chamber_6_LLV_exp_step3 <- chamber_6_LLV_exp %>% filter(Time >= "10:00" & Time <= "12:00")
# Combine
combine_HTVLLV_step3 <- bind_rows(chamber_1_LLV_exp_step3,
                                  chamber_5_LLV_exp_step3, chamber_6_LLV_exp_step3)


# Mean & SD
HTVLLV_step3_stats <- combine_HTVLLV_step3 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 4: 12pm to 4pm (31 C)
chamber_1_LLV_exp_step4 <- chamber_1_LLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
chamber_5_LLV_exp_step4 <- chamber_5_LLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
chamber_6_LLV_exp_step4 <- chamber_6_LLV_exp %>% filter(Time >= "12:00" & Time <= "16:00")
# Combine
combine_HTVLLV_step4 <- bind_rows(chamber_1_LLV_exp_step4,
                                  chamber_5_LLV_exp_step4, chamber_6_LLV_exp_step4)


# Mean & SD
HTVLLV_step4_stats <- combine_HTVLLV_step4 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 5: 4pm to 6pm (26 C)
chamber_1_LLV_exp_step5 <- chamber_1_LLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
chamber_5_LLV_exp_step5 <- chamber_5_LLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
chamber_6_LLV_exp_step5 <- chamber_6_LLV_exp %>% filter(Time >= "16:00" & Time <= "18:00")
# Combine
combine_HTVLLV_step5 <- bind_rows(chamber_1_LLV_exp_step5,
                                  chamber_5_LLV_exp_step5, chamber_6_LLV_exp_step5)


# Mean & SD
HTVLLV_step5_stats <- combine_HTVLLV_step5 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 6: 6pm to 8pm (23 C)
chamber_1_LLV_exp_step6 <- chamber_1_LLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
chamber_5_LLV_exp_step6 <- chamber_5_LLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
chamber_6_LLV_exp_step6 <- chamber_6_LLV_exp %>% filter(Time >= "18:00" & Time <= "20:00")
# Combine
combine_HTVLLV_step6 <- bind_rows(chamber_1_LLV_exp_step6,
                                  chamber_5_LLV_exp_step6, chamber_6_LLV_exp_step6)


# Mean & SD
HTVLLV_step6_stats <- combine_HTVLLV_step6 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

## Step 7: 8pm to 10pm (20 C)
chamber_1_LLV_exp_step7 <- chamber_1_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_5_LLV_exp_step7 <- chamber_5_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_6_LLV_exp_step7 <- chamber_6_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_HTVLLV_step7 <- bind_rows(chamber_1_LLV_exp_step7,
                                  chamber_5_LLV_exp_step7, chamber_6_LLV_exp_step7)


# Mean & SD
HTVLLV_step7_stats <- combine_HTVLLV_step7 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


# Night
## 10pm to 6am (18 C)
chamber_1_LLV_exp_night <- chamber_1_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_5_LLV_exp_night <- chamber_5_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
chamber_6_LLV_exp_night <- chamber_6_LLV_exp %>% filter(Time >= "22:00" | Time <= "06:00")
# Combine
combine_HTVLLV_night <- bind_rows(chamber_1_LLV_exp_night, 
                                  chamber_5_LLV_exp_night, chamber_6_LLV_exp_night)


# Mean & SD
HTVLLV_night_stats <- combine_HTVLLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


## Print results for mean and SD by protocol step ##############################
print(LTVLLV_step1_stats)
print(LTVLLV_step2_stats)
print(LTVLLV_step3_stats)
print(LTVLLV_night_stats)

print(LTVHLV_step1_stats)
print(LTVHLV_step2_stats)
print(LTVHLV_step3_stats)
print(LTVHLV_night_stats)

print(HTVHLV_step1_stats)
print(HTVHLV_step2_stats)
print(HTVHLV_step3_stats)
print(HTVHLV_step4_stats)
print(HTVHLV_step5_stats)
print(HTVHLV_step6_stats)
print(HTVHLV_step7_stats)
print(HTVHLV_night_stats)

print(HTVLLV_step1_stats)
print(HTVLLV_step2_stats)
print(HTVLLV_step3_stats)
print(HTVLLV_step4_stats)
print(HTVLLV_step5_stats)
print(HTVLLV_step6_stats)
print(HTVLLV_step7_stats)
print(HTVLLV_night_stats)

### Make a table for each step #################################################
##### LTVLLV
LTVLLV_steps <- data.frame(step = c(1, 2, 3, 4), time = c("6:00", "8:00", "20:00", "22:00"), 
                           temp_set = c(22, 26, 22, 18), rh_set = c(70, 70, 70, 70))
LTVLLV_data <- data.frame(step = c(1, 2, 3, 4), actual_temp_mean = c(LTVLLV_step1_stats$Temp_Mean, 
                                                      LTVLLV_step2_stats$Temp_Mean,
                                                      LTVLLV_step3_stats$Temp_Mean,
                                                      LTVLLV_night_stats$Temp_Mean),
                          actual_temp_sd = c(LTVLLV_step1_stats$Temp_SD, 
                                               LTVLLV_step2_stats$Temp_SD,
                                               LTVLLV_step3_stats$Temp_SD,
                                               LTVLLV_night_stats$Temp_SD),
           actual_rh_mean = c(LTVLLV_step1_stats$RH_Mean, LTVLLV_step2_stats$RH_Mean,
                              LTVLLV_step3_stats$RH_Mean, LTVLLV_night_stats$RH_Mean),
           actual_rh_sd = c(LTVLLV_step1_stats$RH_SD, LTVLLV_step2_stats$RH_SD,
                              LTVLLV_step3_stats$RH_SD, LTVLLV_night_stats$RH_SD))
LTVLLV_table <- left_join(LTVLLV_steps, LTVLLV_data, join_by(step))


#### LTVHLV
LTVHLV_steps <- data.frame(step = c(1, 2, 3, 4), time = c("6:00", "8:00", "20:00", "22:00"), 
                           temp_set = c(22, 26, 22, 18), rh_set = c(70, 70, 70, 70))
LTVHLV_data <- data.frame(step = c(1, 2, 3, 4), actual_temp_mean = c(LTVHLV_step1_stats$Temp_Mean, 
                                                                     LTVHLV_step2_stats$Temp_Mean,
                                                                     LTVHLV_step3_stats$Temp_Mean,
                                                                     LTVHLV_night_stats$Temp_Mean),
                          actual_temp_sd = c(LTVHLV_step1_stats$Temp_SD, 
                                             LTVHLV_step2_stats$Temp_SD,
                                             LTVHLV_step3_stats$Temp_SD,
                                             LTVHLV_night_stats$Temp_SD),
                          actual_rh_mean = c(LTVHLV_step1_stats$RH_Mean, LTVHLV_step2_stats$RH_Mean,
                                             LTVHLV_step3_stats$RH_Mean, LTVHLV_night_stats$RH_Mean),
                          actual_rh_sd = c(LTVHLV_step1_stats$RH_SD, LTVHLV_step2_stats$RH_SD,
                                           LTVHLV_step3_stats$RH_SD, LTVHLV_night_stats$RH_SD))
LTVHLV_table <- left_join(LTVHLV_steps, LTVHLV_data, join_by(step))


#### HTVHLV
HTVHLV_steps <- data.frame(step = c(1, 2, 3, 4, 5, 6, 7, 8), 
                           time = c("6:00", "8:00", "10:00", "12:00", "16:00", "18:00", "20:00", "22:00"), 
                           temp_set = c(20, 23, 26, 31, 26, 23, 20, 18), rh_set = c(70, 70, 70, 70, 70, 70, 70, 70))
HTVHLV_data <- data.frame(step = c(1, 2, 3, 4, 5, 6, 7, 8), actual_temp_mean = c(HTVHLV_step1_stats$Temp_Mean, 
                                                                     HTVHLV_step2_stats$Temp_Mean,
                                                                     HTVHLV_step3_stats$Temp_Mean,
                                                                     HTVHLV_step4_stats$Temp_Mean,
                                                                     HTVHLV_step5_stats$Temp_Mean,
                                                                     HTVHLV_step6_stats$Temp_Mean,
                                                                     HTVHLV_step7_stats$Temp_Mean,
                                                                     HTVHLV_night_stats$Temp_Mean),
                          actual_temp_sd = c(HTVHLV_step1_stats$Temp_SD, HTVHLV_step2_stats$Temp_SD,
                                               HTVHLV_step3_stats$Temp_SD, HTVHLV_step4_stats$Temp_SD,
                                               HTVHLV_step5_stats$Temp_SD, HTVHLV_step6_stats$Temp_SD,
                                               HTVHLV_step7_stats$Temp_SD, HTVHLV_night_stats$Temp_SD),
                          actual_rh_mean = c(HTVHLV_step1_stats$RH_Mean, HTVHLV_step2_stats$RH_Mean,
                                             HTVHLV_step3_stats$RH_Mean, HTVHLV_step4_stats$RH_Mean,
                                             HTVHLV_step5_stats$RH_Mean, HTVHLV_step6_stats$RH_Mean,
                                             HTVHLV_step7_stats$RH_Mean, HTVHLV_night_stats$RH_Mean),
                          actual_rh_sd = c(HTVHLV_step1_stats$RH_SD, HTVHLV_step2_stats$RH_SD,
                                             HTVHLV_step3_stats$RH_SD, HTVHLV_step4_stats$RH_SD,
                                             HTVHLV_step5_stats$RH_SD, HTVHLV_step6_stats$RH_SD,
                                             HTVHLV_step7_stats$RH_SD, HTVHLV_night_stats$RH_SD))
HTVHLV_table <- left_join(HTVHLV_steps, HTVHLV_data, join_by(step))


#### HTVLLV
HTVLLV_steps <- data.frame(step = c(1, 2, 3, 4, 5, 6, 7, 8), 
                           time = c("6:00", "8:00", "10:00", "12:00", "16:00", "18:00", "20:00", "22:00"), 
                           temp_set = c(20, 23, 26, 31, 26, 23, 20, 18), rh_set = c(70, 70, 70, 70, 70, 70, 70, 70))
HTVLLV_data <- data.frame(step = c(1, 2, 3, 4, 5, 6, 7, 8), actual_temp_mean = c(HTVLLV_step1_stats$Temp_Mean, 
                                                                                 HTVLLV_step2_stats$Temp_Mean,
                                                                                 HTVLLV_step3_stats$Temp_Mean,
                                                                                 HTVLLV_step4_stats$Temp_Mean,
                                                                                 HTVLLV_step5_stats$Temp_Mean,
                                                                                 HTVLLV_step6_stats$Temp_Mean,
                                                                                 HTVLLV_step7_stats$Temp_Mean,
                                                                                 HTVLLV_night_stats$Temp_Mean),
                          actual_temp_sd = c(HTVLLV_step1_stats$Temp_SD, HTVLLV_step2_stats$Temp_SD,
                                             HTVLLV_step3_stats$Temp_SD, HTVLLV_step4_stats$Temp_SD,
                                             HTVLLV_step5_stats$Temp_SD, HTVLLV_step6_stats$Temp_SD,
                                             HTVLLV_step7_stats$Temp_SD, HTVLLV_night_stats$Temp_SD),
                          actual_rh_mean = c(HTVLLV_step1_stats$RH_Mean, HTVLLV_step2_stats$RH_Mean,
                                             HTVLLV_step3_stats$RH_Mean, HTVLLV_step4_stats$RH_Mean,
                                             HTVLLV_step5_stats$RH_Mean, HTVLLV_step6_stats$RH_Mean,
                                             HTVLLV_step7_stats$RH_Mean, HTVLLV_night_stats$RH_Mean),
                          actual_rh_sd = c(HTVLLV_step1_stats$RH_SD, HTVLLV_step2_stats$RH_SD,
                                           HTVLLV_step3_stats$RH_SD, HTVLLV_step4_stats$RH_SD,
                                           HTVLLV_step5_stats$RH_SD, HTVLLV_step6_stats$RH_SD,
                                           HTVLLV_step7_stats$RH_SD, HTVLLV_night_stats$RH_SD))
HTVLLV_table <- left_join(HTVLLV_steps, HTVLLV_data, join_by(step))

# script to average chamber rh & temp 

## load packages
library(dplyr)
library(readxl)
library(tidyr)


## load data
chamber_1_HLV <- read_excel("rh_temp/Chamber 1 2024-07-25 11_10_01 CDT (Data CDT).xlsx")
chamber_1_LLV <- read_excel("rh_temp/Chamber 1 LLV 2024-07-25 11_19_43 CDT (Data CDT).xlsx")

chamber_2_HLV <- read_excel("rh_temp/Chamber 2 2024-07-25 11_18_53 CDT (Data CDT).xlsx")
chamber_2_LLV <- read_excel("rh_temp/Chamber 2 LLV 2024-07-25 11_19_31 CDT (Data CDT).xlsx")

chamber_3_HLV_1 <- read_excel("rh_temp/Chamber 3 2024-06-21 10_34_44 CDT (Data CDT).xlsx")
chamber_3_HLV_2 <- read_excel("rh_temp/Chamber 3 2024-07-09 15_45_33 CDT (Data CDT).xlsx")
chamber_3_HLV_3 <- read_excel("rh_temp/Chamber 3 2024-07-25 11_20_56 CDT (Data CDT).xlsx")
chamber_3_LLV <-  read_excel("rh_temp/Chamber 3 LLV 2024-07-25 11_20_11 CDT (Data CDT).xlsx")

chamber_4_HLV <- read_excel("rh_temp/Chamber 4 2024-07-25 11_20_23 CDT (Data CDT).xlsx")
chamber_4_LLV <- read_excel("rh_temp/Chamber 4 LLV 2024-07-25 11_18_40 CDT (Data CDT).xlsx")

chamber_5_HLV <- read_excel("rh_temp/Chamber 5 2024-07-25 11_20_00 CDT (Data CDT).xlsx")
chamber_5_LLV <- read_excel("rh_temp/Chamber 5 LLV 2024-07-25 11_19_18 CDT (Data CDT).xlsx")

chamber_6_HLV <- read_excel("rh_temp/Chamber 6 2024-07-25 11_21_11 CDT (Data CDT)(1).xlsx")
chamber_6_LLV <- read_excel("rh_temp/Chamber 6 LLV 2024-07-25 11_19_07 CDT (Data CDT).xlsx")

################################################################################
## Separate Date-Time column
chamber_1_HLV <- separate(data = chamber_1_HLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_1_LLV <- separate(data = chamber_1_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

chamber_2_HLV <- separate(data = chamber_2_HLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_2_LLV <- separate(data = chamber_2_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

chamber_3_HLV_1 <- separate(data = chamber_3_HLV_1, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_3_HLV_2 <- separate(data = chamber_3_HLV_2, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_3_HLV_3 <- separate(data = chamber_3_HLV_3, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_3_LLV <- separate(data = chamber_3_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

chamber_4_HLV <- separate(data = chamber_4_HLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_4_LLV <- separate(data = chamber_4_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

chamber_5_HLV <- separate(data = chamber_5_HLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_5_LLV <- separate(data = chamber_5_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

chamber_6_HLV <- separate(data = chamber_6_HLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')
chamber_6_LLV <- separate(data = chamber_6_LLV, col = "Date-Time (CDT)", into = c('Date', 'Time'), sep = ' ')

################################################################################
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


################################################################################
# separate by chamber treatment
  # High Temp Variability
  # Chambers 1, 5, 6

  # Low Temp Variability
  # Chamber 2, 3, 4

################################################################################
# Script below aggregates by hour for each chamber
################################################################################
# what format does R think the time is in; ask for format of time
data$hour <- as.Time(column, "%h")
# take an average over the largest window; aggregate
?aggregate
aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
# want to aggregate
object_groupby <- group_by(chamber_1_HLV_exp, hour)
object_hour <- summarise(object_groupby, fun = mean)
# get mean, sd, se (have to get count)



################################################################################
#Script below finds means and sd for each treatment
################################################################################
## Filter data for LTVLLV treatments; then combine for each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_2_LLV_exp_step1 <- chamber_2_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_3_LLV_exp_step1 <- chamber_3_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_4_LLV_exp_step1 <- chamber_4_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")

# Combine
combine_LTVLLV_step1 <- bind_rows(chamber_2_LLV_exp_step1, chamber_3_LLV_exp_step1, chamber_4_LLV_exp_step1)

colnames(combine_LTVLLV_step1)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_LTVLLV_step2)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
LTVLLV_step2_stats <- combine_LTVLLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


## Step 3: 8pm to 10pm (20 C)
chamber_2_LLV_exp_step3 <- chamber_2_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_3_LLV_exp_step3 <- chamber_3_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_4_LLV_exp_step3 <- chamber_4_LLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_LTVLLV_step3 <- bind_rows(chamber_2_LLV_exp_step3, 
                               chamber_3_LLV_exp_step3, chamber_4_LLV_exp_step3)

colnames(combine_LTVLLV_step3)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_LTVLLV_night)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
LTVLLV_night_stats <- combine_LTVLLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

################################################################################
## Filter data for LTVHLV treatments; then combine for each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_2_HLV_exp_step1 <- chamber_2_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_3_HLV_exp_step1 <- chamber_3_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_4_HLV_exp_step1 <- chamber_4_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")

# Combine
combine_LTVHLV_step1 <- bind_rows(chamber_2_HLV_exp_step1, chamber_3_HLV_exp_step1, chamber_4_HLV_exp_step1)

colnames(combine_LTVHLV_step1)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_LTVHLV_step2)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
LTVHLV_step2_stats <- combine_LTVHLV_step2 %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


## Step 3: 8pm to 10pm (20 C)
chamber_2_HLV_exp_step3 <- chamber_2_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_3_HLV_exp_step3 <- chamber_3_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
chamber_4_HLV_exp_step3 <- chamber_4_HLV_exp %>% filter(Time >= "20:00" & Time <= "22:00")
# Combine
combine_LTVHLV_step3 <- bind_rows(chamber_2_HLV_exp_step3, 
                                  chamber_3_HLV_exp_step3, chamber_4_HLV_exp_step3)

colnames(combine_LTVHLV_step3)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_LTVHLV_night)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
LTVHLV_night_stats <- combine_LTVHLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))


################################################################################
## Filter data for HTVHLV treatments; combine each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_1_HLV_exp_step1 <- chamber_1_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_5_HLV_exp_step1 <- chamber_5_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_6_HLV_exp_step1 <- chamber_6_HLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
# Combine
combine_HTVHLV_step1 <- bind_rows(chamber_1_HLV_exp_step1, 
                               chamber_5_HLV_exp_step1, chamber_6_HLV_exp_step1)

colnames(combine_HTVHLV_step1)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step2)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step3)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step4)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step5)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step6)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_step7)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVHLV_night)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
HTVHLV_night_stats <- combine_HTVHLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

################################################################################
## Filter data for HTVLLV treatments; combine each step; calc mean and sd
# Day
## Step 1: 6am to 8am (20 C)
chamber_1_LLV_exp_step1 <- chamber_1_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_5_LLV_exp_step1 <- chamber_5_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
chamber_6_LLV_exp_step1 <- chamber_6_LLV_exp %>% filter(Time >= "06:00" & Time <= "08:00")
# Combine
combine_HTVLLV_step1 <- bind_rows(chamber_1_LLV_exp_step1, 
                                  chamber_5_LLV_exp_step1, chamber_6_LLV_exp_step1)

colnames(combine_HTVLLV_step1)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step2)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step3)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step4)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step5)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step6)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_step7)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

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

colnames(combine_HTVLLV_night)[c(4, 5, 6)] <- c("Temperature", "RH", "Dew_Point")

# Mean & SD
HTVLLV_night_stats <- combine_HTVLLV_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

################################################################################
# Print results for mean and SD by protocol step
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

# script to average chamber rh & temp 

# Question for Chamebr 3
# How do I get this to select every 5th minute??

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
# How do I get this to select every 5th minute??

## Filter data for LLV treatments
# Day
## 6am to 8am
## 8am to 8pm
## 8pm to 10pm

# Night
## 10pm to 6am

## Filter data for HLV treatments
# Day
## 6am to 8am
## 8am to 10am
## 10am to 12pm
## 12pm to 4pm
## 4pm to 6pm
## 6pm to 8pm
## 8pm to 10pm

# Night
## 10pm to 6am
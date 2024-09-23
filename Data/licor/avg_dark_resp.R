# script to calculate average dark respiration
library(dplyr)

# read in data
dark_resp <- read.csv("Git/variability_ms_thesis/Data/licor/merged_dark_resp.csv")

# Need to get the average of A for each unique id
resp_sum <- dark_resp %>% group_by(id) %>% 
  summarise(resp = mean(A, na.rm = TRUE)) 

# This dataframe shows averaged A for each plant
head(resp_sum)

# add this data to "all_data"
all_data <- read.csv("Git/variability_ms_thesis/Data/all_data.csv")

# join all data frames together by unique_id (all data) and id (licor data)
all_data <- left_join(all_data, resp_sum, join_by(unique_id == id))


write.csv(all_data, "Git/variability_ms_thesis/Data/all_data.csv")


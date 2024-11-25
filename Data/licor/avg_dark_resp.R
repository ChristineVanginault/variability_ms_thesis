# script to calculate average dark respiration
library(dplyr)

# read in data
dark_resp <- read.csv("Git/variability_ms_thesis/Data/licor/merged_dark_resp.csv")

# Need to get the average of A for each unique id
resp_sum <- dark_resp %>% group_by(id, set_leafT) %>% 
  summarise(resp = mean(A, na.rm = TRUE)) 

# This dataframe shows averaged A for each plant
head(resp_sum)
resp_sum <- resp_sum %>% pivot_wider(names_from = set_leafT, values_from = resp)
colnames(resp_sum)[2] <- "resp_20"
colnames(resp_sum)[3] <- "resp_25"
colnames(resp_sum)[4] <- "resp_31"

# add this data to "all_data"
all_data <- read.csv("Git/variability_ms_thesis/Data/all_data.csv")

# join all data frames together by unique_id (all data) and id (licor data)
all_data <- left_join(all_data, resp_sum, join_by(unique_id == id))
all_data

write.csv(all_data, "Git/variability_ms_thesis/Data/all_data.csv", row.names = F)


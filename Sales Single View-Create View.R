library(dplyr)

load(file = "Rcvd12-16.RData")

s_view_tt_cc <- da %>%
  filter(!(is.na(Agent_Code) | Agent_Code == "")) %>%
  group_by(Agent_Code) %>%
  summarise(tt_cc = sum(Product == "CC"))
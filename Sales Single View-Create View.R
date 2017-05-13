library(dplyr)

load(file = "Rcvd12-1704.RData")

# Function convert year and month to 12*year + month
yyyymm_to_num <- function(ym) {
  y <- as.integer(substr(ym, 1, 4))
  m <- as.integer(substr(ym, 5, 6))
  num <- (12*y)+m
  return(num)
}

# Function find max consecutive month
find_consec_mth <- function(df) {
  df$ym <- yyyymm_to_num(df$yyyymm)  # Convert yyyymm to numeric ym
  df <- df[order(df$ym), ]  # Order by ym
  o.li <- vector()  # vector of consecutive mo.
  m <- 1  # Initialized first month <- 1
  for (i in seq_along(1:nrow(df))) {  # Loop through 
    if (i < nrow(df)) {  # Check it not the last obs. 
      if ((df$ym[i]+1) == df$ym[i+1]) {  # Check if current obs == next obs (m = m+1)
        m <- m + 1  # Increase consecutive m
      } else {  # if not consecutive m
        o.li <- c(o.li, m)  # Store m in list of consecutive mo.
        m <- 1  # Reset m to 1
      }
    } else {  # For looping through the last obs
      o.li <- c(o.li, m)  # Store consecutive m
    }
  }
  o <- max(o.li)  # Output max consecutive m
}

# Function to create 5 features from tt rcvd
s_view_tt <- function(df){
  q <- quantile(df$tt_rcvd, probs = c(0, 0.2, 0.5, 0.8, 1), type = 3)
  q.df <- data.frame(as.list(q))
  colnames(q.df) <- c("min_tt", "p20_tt", "med_tt", "p80_tt", "max_tt")
  return(q.df)
}
## Create single view of tt rcvd ----
# Aggregration of tt rcvd by agent_code and yyyymm
agent_mth_tt <- da %>%
  filter(!(is.na(Agent_Code) | Agent_Code == "")) %>%
  mutate(yyyymm = format(system_date, "%Y%m")) %>%
  group_by(Agent_Code, yyyymm) %>%
  summarise(tt_rcvd = n()) %>%
  mutate(yyyymm_num = yyyymm_to_num(yyyymm))

# Create single view of tt rcvd features ----
l.tt <- split(agent_mth_tt, agent_mth_tt$Agent_Code)  # split by Agent_Code
o <- lapply(l.tt, FUN = s_view_tt)  # apply s_view function
agent_range_tt <- do.call(rbind, lapply(o, data.frame, stringsAsFactors = FALSE))  # Convert list of data frame to single dataframe
agent_range_tt$Agent_Code <- rownames(agent_range_tt)   # Re-create Agent_Codd from rownames
rownames(agent_range_tt) <- NULL  # Reset rownames
rm(list = c("l.tt", "o"))  # Clear unused varibles

# Create frequency features ----
last_12mth_num <- yyyymm_to_num(format(Sys.Date(), "%Y%m")) - 12

active_mth <- agent_mth_tt %>%
  group_by(Agent_Code) %>%
  summarise(tt_activ_mth = sum(tt_rcvd > 0), 
            last12mth_activ_mth = sum((yyyymm_num >= last_12mth_num) & tt_rcvd > 0)
  )

# Create max consecutive mth -----
l.tt <- split(agent_mth_tt, agent_mth_tt$Agent_Code)  # split by Agent_Code
o <- lapply(l.tt, FUN = find_consec_mth)  # apply find_consec_function
agent_consec_mth <- do.call(rbind, lapply(o, data.frame, stringsAsFactors = FALSE))  # Convert list of data frame to single dataframe
agent_consec_mth$Agent_Code <- rownames(agent_consec_mth)   # Re-create Agent_Codd from rownames
rownames(agent_consec_mth) <- NULL  # Reset rownames
colnames(agent_consec_mth) <- c("max_consec_mth", "Agent_Code")
rm(list = c("l.tt", "o"))  # Clear unused varibles


# Create velocity rcvd ----
last_1mth_num <- yyyymm_to_num(format(Sys.Date(), "%Y%m")) - 1
last_3mth_num <- yyyymm_to_num(format(Sys.Date(), "%Y%m")) - 3
last_6mth_num <- yyyymm_to_num(format(Sys.Date(), "%Y%m")) - 6

last_mth_tt <- agent_mth_tt %>%
  filter(yyyymm_num == last_1mth_num) %>%
  summarise(last1mth_tt = sum(tt_rcvd))

last_3mth_tt <- agent_mth_tt %>%
  filter(yyyymm_num >= last_3mth_num) %>%
  summarise(last3mth_tt = sum(tt_rcvd))

last_6mth_tt <- agent_mth_tt %>%
  filter(yyyymm_num >= last_6mth_num) %>%
  summarise(last6mth_tt = sum(tt_rcvd))

last_12mth_tt <- agent_mth_tt %>%
  filter(yyyymm_num >= last_12mth_num) %>%
  summarise(last12mth_tt = sum(tt_rcvd))

agent_velocity_tt <- da %>%
  select(Agent_Code) %>%
  distinct(Agent_Code) %>%
  left_join(last_mth_tt, by = "Agent_Code") %>%
  left_join(last_3mth_tt, by = "Agent_Code") %>%
  left_join(last_6mth_tt, by = "Agent_Code") %>%
  left_join(last_12mth_tt, by = "Agent_Code")

rm(list = c("last_mth_tt", "last_3mth_tt", "last_6mth_tt", "last_12mth_tt"))
rm(list = c("last_1mth_num", "last_3mth_num", "last_6mth_num", "last_12mth_num"))

# Next command ----

agent_s_view <- active_mth %>%
  left_join(agent_range_tt, by = "Agent_Code") %>%
  left_join(agent_consec_mth, by = "Agent_Code") %>%
  left_join(agent_velocity_tt, by = "Agent_Code")

save(list = c("agent_mth_tt", "agent_s_view"), file = "SingleView12-1704.RData")

# Add agent data ----
library(readxl)
agent_data <- read_xls("/Users/Danny/Share Win7/Sales Single View Project/Data all type team/DataAllType.xls")
detach(package:readxl)
agent_data$age <- as.integer(format(Sys.Date(), '%Y')) - as.integer(format(agent_data$Birthday, '%Y'))
agent_data$Birthday <- NULL
agent_data$Thai_Name <- NULL
agent_data$Thai_Surname <- NULL
agent_data$Email_address <- NULL
agent_data$ID_Card <- NULL

agent_s_view <- agent_s_view %>%
  left_join(agent_data, by = "Agent_Code")

rm(agent_data)
save(list = c("agent_mth_tt", "agent_s_view"), file = "SingleView12-1704.RData")

d <- da %>%
  select(Agent_Code, Source_Code) %>%
  distinct(Agent_Code, Source_Code)

agent_s_view <- agent_s_view %>%
  left_join(d, by = "Agent_Code")

save(list = c("agent_mth_tt", "agent_s_view", "da"), file = "SingleView12-1704.RData")
library(dplyr)

load(file = "Rcvd12-16.RData")

# Function convert year and month to 12*year + month
yyyymm_to_num <- function(ym) {
  y <- as.integer(substr(ym, 1, 4))
  m <- as.integer(format(ym, 5, 6))
  num <- 12*y+m
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
  summarise(tt_rcvd = n())

# create single view of tt rcvd features
l.tt <- split(agent_mth_tt, agent_mth_tt$Agent_Code)  # split by Agent_Code
o <- lapply(l.tt, FUN = s_view_tt)  # apply s_view function
agent_s_view_tt <- do.call(rbind, lapply(o, data.frame, stringsAsFactors = FALSE))  # Convert list of data frame to single dataframe
agent_s_view_tt$Agent_Code <- rownames(agent_s_view_tt)   # Re-create Agent_Codd from rownames
rownames(agent_s_view_tt) <- NULL  # Reset rownames

hist(agent_s_view_tt[,5])

# Create velocity and frequency features ---- 
agent_s_view_tt_2 <- agent_mth_tt %>%
  group_by(Agent_Code) %>%
  summarise(activ_mth = sum(tt_rcvd > 0), 
            lastyr_activ_mth = sum(substr(yyyymm, 1, 4) == "2016" & tt_rcvd > 0)
  )
# Create max consecutive mth -----
l.tt <- split(agent_mth_tt, agent_mth_tt$Agent_Code)  # split by Agent_Code
o <- lapply(l.tt, FUN = find_consec_mth)  # apply find_consec_function
agent_s_view_tt_3 <- do.call(rbind, lapply(o, data.frame, stringsAsFactors = FALSE))  # Convert list of data frame to single dataframe
agent_s_view_tt_3$Agent_Code <- rownames(agent_s_view_tt_3)   # Re-create Agent_Codd from rownames
rownames(agent_s_view_tt_3) <- NULL  # Reset rownames
colnames(agent_s_view_tt_3) <- c("max_consec_mth", "Agent_Code")


# agent_mth_cc <- da %>%
#   filter(!(is.na(Agent_Code) | Agent_Code == "")) %>%
#   mutate(yyyymm = format(system_date, "%Y%m")) %>%
#   group_by(Agent_Code, yyyymm) %>%
#   summarise(tt_cc = sum(Product == "CC"))
# 
# agent_mth_rl <- da %>%
#   filter(!(is.na(Agent_Code) | Agent_Code == "")) %>%
#   mutate(yyyymm = format(system_date, "%Y%m")) %>%
#   group_by(Agent_Code, yyyymm) %>%
#   summarise(tt_rl = sum(Product == "REV"))

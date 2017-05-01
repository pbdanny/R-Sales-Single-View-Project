# ------------------------
# Sales Single view project
# Data Cleaning & EDA

# Data Loading

sales2013 <- read.delim(file = "./Data all type team/DataAllTypeTeam2013", stringsAsFactors = FALSE)
sales2013$TL_Code <- as.character(sales2013$TL_Code)
sales2013$Agent_Code <- as.character(sales2013$Agent_Code)

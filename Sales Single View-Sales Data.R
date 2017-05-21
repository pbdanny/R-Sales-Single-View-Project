# ----------------------------
# Sales Single view project
# Data Cleaning
# --------------------------
# 2013 Data Loading
sales2013 <- read.delim(file = "./Data all type team/DataAllTypeTeam2013", stringsAsFactors = FALSE)

# 2013 Data Editing
sales2013$TL_Code <- as.character(sales2013$TL_Code)
sales2013$Agent_Code <- as.character(sales2013$Agent_Code)
sales2013$OpenDate <- as.Date(sales2013$OpenDate)

# Special adjust YYYYMM format to Data data
sales2013$snapDate <- as.Date(paste0(sales2013$snapDate,"01"), format = "%Y%m%d")

# 2014 Data Loading
sales2014 <- read.delim(file = "./Data all type team/DataAllTypeTeam2014", stringsAsFactors = FALSE)

# 2014 Data Editing
sales2014$TL_Code <- as.character(sales2014$TL_Code)
sales2014$Agent_Code <- as.character(sales2014$Agent_Code)
sales2014$OpenDate <- as.Date(sales2014$OpenDate)

# Special adjust YYYYMM format to Data data
sales2014$snapDate <- as.Date(paste0(sales2014$snapDate,"01"), format = "%Y%m%d")

# 2015 Data Loading
sales2015 <- read.delim(file = "./Data all type team/DataAllTypeTeam2015", stringsAsFactors = FALSE)

# 2015 Data Editing
sales2015$TL_Code <- as.character(sales2015$TL_Code)
sales2015$Agent_Code <- as.character(sales2015$Agent_Code)
sales2015$OpenDate <- as.Date(sales2015$OpenDate)

# Special adjust YYYYMM format to Data data
sales2015$snapDate <- as.Date(paste0(sales2015$snapDate,"01"), format = "%Y%m%d")

# 2016 Data Loading
sales2016 <- read.delim(file = "./Data all type team/DataAllTypeTeam2016", stringsAsFactors = FALSE)

# 2016 Data Editing
sales2016$TL_Code <- as.character(sales2016$TL_Code)
sales2016$Agent_Code <- as.character(sales2016$Agent_Code)
sales2016$OpenDate <- as.Date(sales2016$OpenDate)

# Special adjust YYYYMM format to Data data
sales2016$snapDate <- as.Date(paste0(sales2016$snapDate,"01"), format = "%Y%m%d")

sales2014$Title <- NULL
sales2015$Title <- NULL
sales2016$Title <- NULL

df <- rbind(sales2013, rbind(sales2014, rbind(sales2015, sales2016)))

save(salesAll, file = "salesAllData2013_2016.RData")

library(readxl)
agent_data <- read_xls("/Users/Danny/Share Win7/Sales Single View Project/Data all type team/DataAllType.xls")
detach(package:readxl)

agent_data$age <- as.integer(format(Sys.Date(), '%Y')) - as.integer(format(agent_data$Birthday, '%Y'))

# Load data

getwd()
d1 <- read.delim(file = "rcvd_12_13.txt", stringsAsFactors = FALSE)
d2 <- read.delim(file = "rcvd_14_16.txt", stringsAsFactors = FALSE)

# Convert string to tme
d1$system_date <- as.Date(d1$system_date, format = "%d/%m/%Y")
d2$system_date <- as.Date(d2$system_date, format = "%d/%m/%Y")

# Combine data
d <- rbind(d1, d2)
rm(d1)
rm(d2)

# Data pre-audit
summary(d)

# Remove year 2017
da <- d[format(d$system_date, "%Y") != "2017", ]

# Edit error Product
# da[da$Product == "Fix",'Product'] <- "FIX"

save(da, file = "Rcvd12-16.RData")

# EDA
load(file = "Rcvd12-16.RData")

ag <- aggregate(da['Agent_Code'], by = c(da['system_date'], da['Product']), FUN = length)
ag2 <- tapply(da$Agent_Code, INDEX = c(da$Product, da$system_date), FUN = length)
plot(x = ag$system_date, y = ag$Agent_Code, col = as.factor(ag$Product))

# Load 201704 rcvd data into da
d3 <- read.delim(file = "rcvd_1704.txt", stringsAsFactors = FALSE)
d3$system_date <- as.Date(d3$system_date, format = "%d/%m/%Y")
load(file = "Rcvd12-16.RData")
da <- rbind(da, d3)
rm(d3)
save(da, file = "Rcvd12-1704.RData")

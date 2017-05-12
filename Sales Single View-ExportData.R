# -----------------------------------------------------
# Project : Sales single view
# R Script for retrive snapshot of sales data from 2013 to
# 2016 and export .txt format for upload to GCP

getwd()
setwd("E:/")
list.files()

# Change this file name according to Year Data
# OSSDatabase2013.accdb
# OSSDatabaseNew2014.accdb
# OSSDatabaseNew2015.accdb
# 2016OSSDatabase.accdb

file <- "2016OSSDatabase.accdb"

# Connect database create vector list of sales monthly data
library(RODBC)
con <- odbcConnectAccess2007(file)
t <- sqlTables(con)
tblAll <- t$TABLE_NAME
tblData <- tblAll[grepl("^Data", tblAll)]

# Looping list of sales monthly data retriving table.
# Add column 'Date' as data data snapshotted.
# Bind all data and save to text file

# Function for sub string right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Output dataframe as list 

out <- list()

# Loop all monthly data

for (i in seq_along(tblData)){
	snapDate <- substrRight(tblData[i], 6)  # get YYYYMM from table name
	data <- sqlFetch(con, tblData[i], stringsAsFactors = FALSE, as.is = TRUE)
	data$snapDate <- snapDate
	# Rename column, can not re-create since data structure will diff
	# and could not be combind  
	if ('ProvinceCode' %in% colnames(data)) {
		colnames(data)[colnames(data) == 'ProvinceCode'] <- "Province"
	}
	# If varible Mobile_Number not exisit then create one
	if (!('Mobile_Number' %in% colnames(data))) {
		data$Mobile_Number <- NA
	}
	out <- rbind(out, data)
}
# Convert out put from list to dataframe
outDF <- as.data.frame(out)

# Rename output data
newName <- colnames(outDF)
colnames(outDF) <- make.names(newName)

# Write back to .txt format

setwd("E:/Sales Single View Project/Data all type team")
outFileName <- paste0("DataAllTypeTeam", substr(snapDate, 1, 4))
write.table(outDF, outFileName, 
			row.names = FALSE, col.names = TRUE, 
			quote = TRUE, sep = "\t", fileEncoding="UTF-8")
odbcCloseAll()

# -----------------------------------------
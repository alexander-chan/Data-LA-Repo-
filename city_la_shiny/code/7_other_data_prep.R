#Determines the date so we can refresh every ____day
date <- as.POSIXct(Sys.time())

#If we refresh on Sunday, leads to issue where Sunday will have no data.
#Thus, we technically will refresh on Monday when we have at least one day of data
ifelse(wday(date) == 1, previous_sunday <- floor_date(date - 86400, "week"), 
       previous_sunday <- floor_date(date, "week"))

#Requires R Socrata to load in data on the fly
#testSocrata2 <- read.socrata(paste("https://data.lacity.org/A-Well-Run-City/MyLA311-Service-Request-Data-2017/d4vt-q4t5?$where=updateddate >= ", 
#                                   paste0("'",previous_sunday, "'")))

testSocrata2 <- read_csv("./city_LA_shiny/data/Weekly Call Center.csv")

#Requires package gsheets to load in data on the fly
#flow_data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1aTjWGg8P3C5oiz4HkB7q4rmOCXmVCoZ1VHyRw3IjDso/edit#gid=0')
flow_data <- read_csv("./city_LA_shiny/data/Flow Data - Sheet1.csv")
names(flow_data) <- flow_data[1,] #Skips the first row and uses the second row to name the columns
flow_data <- flow_data[c(2:46),]
flow_data[,c(1,3:12, 14:16)] <- sapply(flow_data[,c(1,3:12,14:16)], as.numeric) #Convert majority of data to numeric as they came in as characters

i <- min(na.omit(flow_data[,1]))  #Since the YEAR variable was only on every fourth row, changed it so we can find it on every row
for(j in 1:nrow(flow_data)) {
  flow_data[j,1] <- i
  flow_data[j,14] <- i
  if(!(is.na(flow_data[j+1,1]))) {
    i <- i + 1
  }
}

flow_data$`Percent Reuse of Total Flow` <- as.numeric(sub("%", "", flow_data$`Percent Reuse of Total Flow`)) #Removes the percentage sign and makes it numeric
flow_data$`Percent Reuse` <- as.numeric(sub('%', '', flow_data$`Percent Reuse`)) #Removes the percentage sign and makes it numeric
flow_data$YEAR <- sapply(flow_data$YEAR, as.factor) #Makes the year variable a factor

for(i in seq(4, nrow(flow_data), 4)) { #Makes it so that annual data shows up on every row, not just every 4th
  for(j in 1:3) {
    flow_data[i - j, 15] <- flow_data[i,15]
    flow_data[i - j, 16] <- flow_data[i,16]
    flow_data[i - j, 17] <- flow_data[i,17]
  }
}

quarter_names <- c('JAN - MAR', 'APR - JUN', 'JUL - SEP', 'OCT - DEC')
flow_data$quarter_rev_factor <- factor(flow_data$QUARTER, levels = rev(quarter_names))
flow_data$quarter_factor <- factor(flow_data$QUARTER, levels = quarter_names)

#Melt requires package reshape2
flow_melt <- melt(flow_data[,c('YEAR', 'Annual Average Flows', 'Annual Average Reuse')], id.vars = 1)

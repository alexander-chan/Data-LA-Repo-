#Determines the date so we can refresh every ____day
date <- as.POSIXct(Sys.time())

#If we refresh on Sunday, leads to issue where Sunday will have no data.
#Thus, we technically will refresh on Monday when we have at least one day of data
ifelse(wday(date) == 1, previous_sunday <- floor_date(date - 86400, "week"), 
       previous_sunday <- floor_date(date, "week"))

testSocrata2 <- read.socrata(paste("https://data.lacity.org/A-Well-Run-City/MyLA311-Service-Request-Data-2017/d4vt-q4t5?$where=updateddate >= ", 
                                   paste0("'",previous_sunday, "'")))


#flow_data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1aTjWGg8P3C5oiz4HkB7q4rmOCXmVCoZ1VHyRw3IjDso/edit#gid=0')
flow_data <- read_csv("./city_LA_shiny/data/Flow Data - Sheet 1.csv")
names(flow_data) <- flow_data[1,]
flow_data <- flow_data[c(2:46),]
flow_data[,c(1,3:12, 14:16)] <- sapply(flow_data[,c(1,3:12,14:16)], as.numeric)

i <- min(na.omit(flow_data[,1]))
for(j in 1:nrow(flow_data)) {
  flow_data[j,1] <- i
  flow_data[j,14] <- i
  if(!(is.na(flow_data[j+1,1]))) {
    i <- i + 1
  }
}

flow_data$`Percent Reuse of Total Flow` <- as.numeric(sub("%", "", flow_data$`Percent Reuse of Total Flow`))
flow_data$`Percent Reuse` <- as.numeric(sub('%', '', flow_data$`Percent Reuse`))
flow_data$YEAR <- sapply(flow_data$YEAR, as.factor)

for(i in seq(4, nrow(flow_data), 4)) {
  for(j in 1:3) {
    flow_data[i - j, 15] <- flow_data[i,15]
    flow_data[i - j, 16] <- flow_data[i,16]
    flow_data[i - j, 17] <- flow_data[i,17]
  }
}

quarter_names <- c('JAN - MAR', 'APR - JUN', 'JUL - SEP', 'OCT - DEC')
flow_data$quarter_rev_factor <- factor(flow_data$QUARTER, levels = rev(quarter_names))
flow_data$quarter_factor <- factor(flow_data$QUARTER, levels = quarter_names)

flow_melt <- melt(flow_data[,c('YEAR', 'Annual Average Flows', 'Annual Average Reuse')], id.vars = 1)

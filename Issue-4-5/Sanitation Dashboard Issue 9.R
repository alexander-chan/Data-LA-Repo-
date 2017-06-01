library(readr)
flow_data <- read_csv('Flow Data - Sheet1.csv', skip = 1)
flow_data <- flow_data[c(1:45),]

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

library(ggplot2)
ggplot(data = flow_data[flow_data$YEAR %in% c(2006:2016),], aes(x = as.factor(YEAR))) +
  geom_bar(aes(weight = `Total Combined Reuse`, fill = quarter_rev_factor)) +
  theme_bw() + 
  coord_flip() +
  ggtitle('Bar Chart of Total Recycle by Year for 2006-2016')+
  guides(fill = guide_legend(reverse=TRUE))

ggplot(data = flow_data, aes(x = quarter_factor)) +
  geom_bar(aes(weight = `Total Combined Reuse`, fill = quarter_factor)) +
  theme_bw() +
  ggtitle('Bar Chart of Total Recycle by Quarter')

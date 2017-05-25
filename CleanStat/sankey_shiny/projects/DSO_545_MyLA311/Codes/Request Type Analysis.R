
library(RSQLite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)

driver = dbDriver("SQLite")
con = dbConnect(driver, "Database.sqlite")

dbListTables(con)

request_data = dbReadTable(con, "Request_data")

## Distribution of RequestType

sql = "select RequestType, count(*) as cnt
from Request_data
group by RequestType;"
requestType = dbGetQuery(con, sql)

## Getting request type count at date level
sql = "select CreatedDate, RequestType, count(*) as cnt
from Request_data
group by CreatedDate, RequestType;"
date_request = dbGetQuery(con, sql)
str(request_data)

## Getting request type count at month level
month_request = date_request%>%
  group_by(year, month, RequestType)%>%
  summarise(count = n())

## Following code didn't work
##sql = "select strftime('%Y', datetime(CreatedDate)) as year_created,
#strftime('%m', datetime(CreatedDate)) as month_created, RequestType, count(*) as cnt
#from Request_data
#group by strftime('%Y', datetime(CreatedDate)),strftime('%m', datetime(CreatedDate)), RequestType;"

## Creating year and month column for the created date
date_request$month_created = month(mdy_hms(date_request$CreatedDate), label = T)
date_request$year_created = year(mdy_hms(date_request$CreatedDate))

## getting request type count at year and month level
month_request = date_request%>%
  group_by(year_created, month_created, RequestType)%>%
  summarise(count = n())

## Date level line plot for number of each request type
month_request$year_month_created = paste(month_request$year_created,month_request$month_created)
table(month_request$year_month_created)

## Changing the level of the year-month column
month_request$year_month_created = factor(month_request$year_month_created, level = c("2015 Aug", "2015 Sep", "2015 Oct",
                                                                                      "2015 Nov","2015 Dec", "2016 Jan",
                                                                                      "2016 Feb","2016 Mar","2016 Apr",
                                                                                      "2016 May","2016 Jun","2016 Jul",  
                                                                                      "2016 Aug", "2016 Sep","2016 Oct", 
                                                                                       "2016 Nov"))
levels(month_request$year_month_created)

## Ordering count of requestType in decreasing order
month_request$RequestType <- factor(month_request$RequestType,
                                    levels = rev(month_request$RequestType[order(month_request$count)])
                                    ,ordered = TRUE)

##Plot of change in count of each request type with time
month_request%>%
  ggplot(aes(x = factor(year_month_created), y = count,
                          color = RequestType, group = RequestType))+
  geom_line(size = 1)+
  xlab("Year-Month of Request Created")+
  ylab("Request Count")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
##Plot of change in count of each request type with time(excluding 2016 Nov)
month_request%>%
filter(year_month_created != "2016 Nov")%>%
ggplot(aes(x = factor(year_month_created), y = count,
           color = RequestType, group = RequestType))+
geom_line(size = 1)+
xlab("Year-Month of Request Created")+
ylab("Request Count")+
theme(axis.text.x = element_text(angle = 30, hjust = 1))

###Checking max date
date_request%>%
  summarise(max_date = max(mdy_hms(CreatedDate)))


## Pie chart for request type distribution
ggplot(requestType, aes(x = factor(1), y = cnt, fill = RequestType))+
  geom_bar(width = 1, stat = "identity",color = "black")+
  coord_polar(theta = "y")+
  xlab("")+
  ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  ggtitle("Request Type Distribution")

library(scales)
### Scatter plot for request type call count
ggplot(requestType, aes(x = reorder(RequestType, cnt), y = cnt))+
  geom_point(color = "black")+
  xlab("Request Type")+
  ylab("")+
  scale_y_continuous(labels = comma, breaks = seq(0,600000,100000))+
  coord_flip()+
  ggtitle("Request Type Call Count")

#### Percentage of call transferred for each type of request to other department for completion

sql = "select RequestType, CreatedDate, UpdatedDate, ServiceDate
from Request_data;"
req_process_time = dbGetQuery(con, sql)

## Creating process and finish time columns
req_process_time$req_finish_time = mdy_hms(req_process_time$ServiceDate) - mdy_hms(req_process_time$CreatedDate)
req_process_time$req_process_time = mdy_hms(req_process_time$UpdatedDate) - mdy_hms(req_process_time$CreatedDate)

## Number of rows with negative or NA process or finish time
req_process_time%>%
  filter(req_process_time < 0 | req_finish_time < 0 | is.na(req_process_time) | is.na(req_finish_time))%>%
  summarise(count = n())

req_process_time%>%
  filter(is.na(req_process_time) | is.na(req_finish_time))%>%
  summarise(count = n())
## ONly NA = 61195

158674/1082569
## 158674
## 14.6% of data has negative process or finish time

219846/1082569
## 219846
## 20% of data has NA or negative process or finish time 

## QC to check number of rows
req_process_time%>%
  filter(req_process_time >= 0 & req_finish_time >= 0 & !is.na(req_process_time) & !is.na(req_finish_time))%>%
  summarise(count = n())
##   count: 862723

## Removing rows with NA or negative process or finish time
## Calculting mean process and finish time(in days) at request type and date created level
req_date_time = req_process_time%>%
  filter(req_process_time >= 0 & req_finish_time >= 0 & !is.na(req_process_time) & !is.na(req_finish_time))%>%
  group_by(RequestType, CreatedDate)%>%
  summarise(avg_process_time = mean(req_process_time/86400), avg_finish_time = mean(req_finish_time/86400))

## Removing rows with NA or negative process or finish time
## Calculting mean process and finish time(in days) at request type level
req_time = req_process_time%>%
  filter(req_process_time >= 0 & req_finish_time >= 0 & !is.na(req_process_time) & !is.na(req_finish_time))%>%
  group_by(RequestType)%>%
  summarise(avg_process_time = mean(req_process_time/86400), avg_finish_time = mean(req_finish_time/86400))

## COnverting CreatedDate to Date format
req_date_time$CreatedDate = mdy_hms(req_date_time$CreatedDate)

## Creating year and month column for the created date
req_date_time$month_created = month(req_date_time$CreatedDate, label = T)
req_date_time$year_created = year(req_date_time$CreatedDate)

## getting average of process and finish time of each request type at year and month level
req_month = req_date_time%>%
  group_by(RequestType, year_created, month_created)%>%
  summarise(avg_finish_time = mean(avg_finish_time), avg_process_time = mean(avg_process_time))

## Date level line plot for number of each request type
req_month$year_month_created = paste(req_month$year_created,req_month$month_created)
table(req_month$year_month_created)

## Changing the level of the year-month column
req_month$year_month_created = factor(req_month$year_month_created, level = c("2015 Aug", "2015 Sep", "2015 Oct",
                                                                                      "2015 Nov","2015 Dec", "2016 Jan",
                                                                                      "2016 Feb","2016 Mar","2016 Apr",
                                                                                      "2016 May","2016 Jun","2016 Jul",  
                                                                                      "2016 Aug", "2016 Sep","2016 Oct", 
                                                                                      "2016 Nov"))
levels(req_month$year_month_created)

## Ordering count of requestType in decreasing order
req_month$RequestType <- factor(req_month$RequestType,
                                    levels = rev(req_month$RequestType[order(req_month$count)])
                                    ,ordered = TRUE)


## Plot showing trend for time taken to finish each type of request
req_month%>%
  filter(year_month_created != "2016 Nov")%>%
  ggplot(aes(x = factor(year_month_created), y = avg_finish_time,
             color = RequestType, group = RequestType))+
  geom_line(size = 1)+
  xlab("Year-Month of Request Created")+
  ylab("Days to complete the request")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  facet_wrap(~RequestType, scales = "free", ncol = 2)+
  ggtitle("Request Type Completion Time")


## Plot showing trend for time taken to process each type of request
req_month%>%
  filter(year_month_created != "2016 Nov")%>%
  ggplot(aes(x = factor(year_month_created), y = avg_process_time,
             color = RequestType, group = RequestType))+
  geom_line(size = 1)+
  xlab("Year-Month of Request Created")+
  ylab("Days to process the request")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  facet_wrap(~RequestType, scales = "free", ncol = 2)+
  ggtitle("Request Type Process Time")

## 
LA = qmap("Los Angeles")

## Converting created date to date format
request_data$CreatedDate = mdy_hms(request_data$CreatedDate)
## Creating weekday
request_data$day = wday(request_data$CreatedDate, label = T, abbr = F)


### ACTION TAKEN

sql = "select RequestType, ActionTaken, count(*) as count
from Request_data
group by RequestType, ActionTaken;"
req_action = dbGetQuery(con, sql)

reqNoSR = subset(req_action, ActionTaken!= "SR Created")

## Action Taken Vs Req Type
reqNoSR%>%
  filter(RequestType!="Other")%>%
  group_by(RequestType)%>%
  mutate(percent = count*100/sum(count), pos = (cumsum(percent) - 0.5 * percent))%>%
  ggplot(aes(x = RequestType, y = percent, fill = ActionTaken))+
  geom_bar(stat = "identity", width = .7) +
  xlab("Request Type")+
  ylab("Action Taken")+
  ggtitle("Action taken Vs Request Type")+
  geom_text(aes(y = pos, label = paste(round(percent,2),"%"), sep = ""), size=3)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  coord_flip()

## OWNER and ASSIGN TO
sql = "select Owner, RequestType, count(*) as count
from Request_data
group by Owner,RequestType;"
dbGetQuery(con, sql)

sql = "select AssignTo, count(*) as count
from Request_data
group by AssignTo;"
dbGetQuery(con, sql)


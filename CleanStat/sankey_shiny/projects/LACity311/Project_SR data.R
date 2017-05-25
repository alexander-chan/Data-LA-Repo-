service_req = read.csv("MyLA311_Service_Request_Data_2016.csv")
##checking the sample
head(service_req,10)
##checking data types
str(service_req)
##storing as data frame
service_req_data = as.data.frame(service_req)
##checking the number of rows having NA values
sum_NA = table(is.na(service_req_data))
sum_NA
##converting columns having date values from Factor to date
library(lubridate)
service_req_data$CreatedDate = mdy_hms(service_req_data$CreatedDate)
service_req_data$UpdatedDate = mdy_hms(service_req_data$UpdatedDate)
service_req_data$ServiceDate = mdy_hms(service_req_data$ServiceDate)
service_req_data$ClosedDate = mdy_hms(service_req_data$ClosedDate)
##No.of NA is 1.2% of total rows
##omiting NA s would not cause a large effect
##Hence omiting NAs
service_req_data=na.omit(service_req_data)


## rechecking data types again
str(service_req_data)
## analysing the problems year wise
service_req_data$year_created_date=year(service_req_data$CreatedDate)
unique(service_req_data$year_created_date)
## the data is of year 2015 and 2016
## analysing the data on basis of county map
View(service_req_data)
service_req_data$time_diff_in_exec = service_req_data$ClosedDate - service_req_data$CreatedDate
service_req_data$time_diff_in_exec_in_days = service_req_data$time_diff_in_exec/86400
service_req_data$time_diff_in_exec_in_days = round(service_req_data$time_diff_in_exec_in_days)
## classifying into quarters
service_req_data$month_created_date = month(service_req_data$CreatedDate, label = T, abbr = T)
service_req_data$month_created_date = as.character(service_req_data$month_created_date)
service_req_data$quarter = ifelse(service_req_data$month_created_date == "Jul"|service_req_data$month_created_date == "Aug"|service_req_data$month_created_date == "Sep","Q1",
                                  ifelse(service_req_data$month_created_date == "Oct"| service_req_data$month_created_date == "Nov"| service_req_data$month_created_date == "Dec","Q2",
                                   ifelse(service_req_data$month_created_date == "Jan"| service_req_data$month_created_date == "Feb"| service_req_data$month_created_date == "Mar","Q3","Q4")))

View(service_req_data)
str(service_req_data)
-------------
## plotting to analyze how service requests were solved
library(ggplot2)
data1 <- service_req_data %>%
  group_by(RequestType, year_created_date) %>%
  summarise(M = median(time_diff_in_exec))
data1$time_diff_in_exec_in_days = data1$M/86400
data1$time_diff_in_exec_in_days = round(data1$time_diff_in_exec_in_days)
ggplot(data1,aes(x = RequestType,y = as.numeric(time_diff_in_exec_in_days)))+
  geom_bar(stat = "identity", fill = "blue")+
  ggtitle("Average time taken to resolve requests")+
  xlab("Request type")+
  ylab("Time taken in days")+
  facet_wrap(~year_created_date, nrow = 2)+
  theme_bw()


------------
##plotting to analyse type of the request addressed vs time of the day
service_req_data$hour_created_date = hour(service_req_data$CreatedDate)
service_req_data$day_created_date = wday(service_req_data$CreatedDate, label = T, abbr = T)
unique(service_req_data$RequestType)
##changing from factor to character
service_req_data$RequestType = as.character(service_req_data$RequestType)
service_req_data %>%
  group_by(day_created_date, hour_created_date) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = day_created_date, y = as.factor(hour_created_date), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "steelblue")+
  xlab("Day of the week")+
  ylab("Hour of the Day")+
  ggtitle("Requests based on week days and hours of the day")+
  labs(fill = "Number of requests")

# Requests based on quarters of the year
service_req_data %>%
  group_by(quarter, RequestType) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = quarter, y = count))+
  geom_point(aes(color = RequestType))+
  xlab("Quarters in 2015-16")+
  ylab("")+
  ggtitle("Requests in different quarters of the year")+
  labs(fill = "Type of Request")+
  theme_bw()
--------------------
  ## plotting to analyse which platform is used the most among mobile users
  mobile_users = service_req_data %>%
  filter(RequestSource == 'Mobile App')%>%
  group_by(MobileOS)
mobile_users %>%
  ggplot(aes(x = MobileOS))+
  geom_bar(fill = "blue")+
  xlab("Operating System")+
  ylab("No. of users")+
  ggtitle("Analysis of mobile app users")+
  theme_bw()
-----------
  ## plotting to analyse usage of different types of Request Sources
  ggplot(service_req_data,aes(x = RequestSource))+
  geom_bar()

# imporvements required in social media  
unique(service_req_data$RequestSource)        
reqsource <- service_req_data %>%
  group_by(RequestSource, year_created_date) %>%
  summarise(count = n())

# Percentage of each type of request
total <- sum(reqsource$count)
reqsource <- reqsource %>%
  group_by(year_created_date) %>%
  mutate(percentage1 = count*100/sum(count))

#Filtering the significant request types  
reqsource %>%
  filter(RequestSource %in% c("Call","Council's Office","Email", "Mobile App", "Self Service",
                              "Voicemail")) %>%
  ggplot(aes(x = RequestSource, y = percentage1)) +
  geom_bar(stat = "identity", fill = "blue")+
  xlab("Source of Request") +
  ylab("Percentage")+
  facet_wrap(~year_created_date, ncol = 1) +
  theme_bw()+
  ggtitle("Percentage of requests by years")

# Request types
service_req_data %>%
  group_by(RequestType) %>%
  summarise(percent = n()*100/total) %>%
  ggplot(aes(x = reorder(RequestType, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "blue")+
  xlab("Request type") +
  ylab("Percentage")+
  theme_bw()+
  ggtitle("Percentage of requests types")
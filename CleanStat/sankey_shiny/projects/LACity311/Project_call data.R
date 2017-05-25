Call_data = read.csv("311_Call_Center_Tracking_Data.csv")
head(Call_data,10)
## checking data types of variables
str(Call_data)
sum_NA = table(is.na(Call_data))
sum_NA
## converting Date and Time columns to required data types
library(lubridate)
Call_data$Date = mdy(Call_data$Date)
Call_data$Time = hms(Call_data$Time)
## adding a column hours
Call_data$hour = hour(Call_data$Time)
##adding column weekday
Call_data$day = wday(Call_data$Date,label = T, abbr = T)
##omit na
## not sure if na to be omitted, very less number of observations remain after
#omitting NA
Call_data=na.omit(Call_data)
Call_data <- Call_data[!duplicated(Call_data[,c('Date', 'Time', 'Service.Name', 'Zip.Code')]),]
##Plotting to analyze Request types at different times of the day
## omit observations which are blank in Department abbr
library(dplyr)
Call_data = Call_data %>%
  filter(Department.Abbreviation != " ")
 library(ggplot2) 
Call_data %>%  
  group_by(day, hour) %>%
  summarise(count = n()) %>%
   ggplot(aes(x = day, y = factor(hour), fill = count))+
   geom_tile()+
   scale_fill_gradient(low = "white", high = "steelblue")+
   xlab("Day of the week")+
   ylab("Hour of the Day")+
   ggtitle("Distribution of Different requests on hourly basis")

library(stringr)
str_replace_all(Call_data$Department.Name,"[[:blank:]]","")

# Top 10 departments
Dept_data <- Call_data %>%
  filter(Department.Abbreviation!="") %>%
  group_by(Department.Abbreviation) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  head(10)
ggplot(Dept_data, aes(x = Department.Abbreviation, y = count)) +
  geom_bar(fill = "pink", stat = "identity") +
  xlab("Department Name") +
  ylab("Number of requests") +
  ggtitle("Top 10 departments with most requests")

# Top 10 services
Service_data <- Call_data %>%
  filter(Service.Name!="") %>%
  group_by(Service.Name) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  head(10)

ggplot(Service_data, aes(x = Service.Name, y = count)) +
  geom_bar(aes(fill = Service.Name), stat = "identity") +
  xlab("Service") +
  ylab("Number of requests") +
  ggtitle("Top 10 services with most requests")+
  labs(fill = "Service Name")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Trasnferred, referred and resolved
unique(Call_data$Call.Resolution)
Call_data$Resolved = ifelse(Call_data$Call.Resolution %in% c("Transfer (City)","Referred To County",
                                                             "Warm Transfer (City)","Referred To 411",
                                                             "Referred To State","Referred To Other Governmental",
                                                             "Transferred To 411"),"Transferred/Referred",
                            ifelse(Call_data$Call.Resolution %in% c("Escalate To Supervisor", 
                                                                    "Escalated To Office of Finance"), "Escalated",
                                   ifelse(Call_data$Call.Resolution == "N/A", "Unknown Status", 
                                          ifelse(Call_data$Call.Resolution == "Service Request Processed",
                                                 "Processed","Others"))))

# Plot of status of calls
totalresolved = nrow(Call_data)
Call_data %>%
  group_by(Resolved) %>%
  summarise(percent = n()*100/totalresolved) %>%
  ggplot(aes(x = reorder(Resolved,-percent), y = percent)) +
  geom_bar(fill = "blue", stat = "identity") +
  xlab("Status")+
  ylab("Percentage") +
  ggtitle("Status of records by percentage")+
  theme_bw()

# Creating year and month variables
Call_data$year <- year(Call_data$Date)
Call_data$month <- month(Call_data$Date,label = T, abbr = T)

# Check processed and transferred calls in different months of the year
Call_data %>%
  group_by(Resolved,month) %>%
  summarise(percent = n()*100/totalresolved) %>%
  filter(Resolved %in% c("Transferred/Referred","Processed")) %>%
  ggplot(aes(x = Resolved, y = percent)) +
  geom_bar(fill = "blue", stat = "identity") +
  facet_wrap(~month, nrow = 4)+
  xlab("Status") +
  ylab("Percentage") +
  ggtitle("Processed and Transferred requests over the years")+
  theme_bw() 

# Classify into quarters  
Call_data$quarter = ifelse(Call_data$month %in%c("Jul","Aug","Sep"),"Q1",
                                  ifelse(Call_data$month %in% c("Oct","Nov","Dec"),"Q2",
                                         ifelse(Call_data$month %in% c("Jan","Feb","Mar"),"Q3","Q4")))

# Check processed and transferred calls in different quarters of the year
Call_data %>%
  group_by(Resolved,quarter) %>%
  summarise(percent = n()*100/totalresolved) %>%
  filter(Resolved %in% c("Transferred/Referred","Processed")) %>%
  ggplot(aes(x = Resolved, y = percent)) +
  geom_bar(fill = "blue", stat = "identity") +
  facet_wrap(~quarter, nrow = 2)+
  xlab("Status") +
  ylab("Percentage") +
  ggtitle("Processed and Transferred requests over the years")+
  theme_bw() 
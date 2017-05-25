library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

data1 = read.csv("311_Call_Center_Tracking_Data.csv")
data2 = read.csv("MyLA311_Service_Request_Data_2016.csv")

zipcode = read.csv("zip code.csv")

data15 = data1 %>%
  filter(Call.Resolution == "Service Request Processed")
data16 = data2

### graph "The change of service requests from 2011-2015"

## clean data
# change data15 date and time to date type

data15$Date = dmy(data15$Date)
data15$Time = hms(data15$Time)

# service type by year
data15_servby = data15 %>%
  filter(!is.na(Date) & 
           Service.Name != "")  %>%
  droplevels() %>%
  mutate(year = year(Date)) %>%
  group_by(year, Service.Name) %>%
  summarise(count = n())

### reorder the levels of Service Name
data15_servby$Service.Name = 
  factor(data15_servby$Service.Name,
         levels = 
           c("Bulky Item Pick-up",
             "Online Request for Permit Inspection",
             "Graffiti Removal - Community Beautification",
             "Report a Property Violation",
             "Report streetlight outages",
             "9-1-1 Emergency Calls for Service - LAPD",
             "Neighborhood Info"))

ggplot(data15_servby, aes(x = as.factor(year),
                          y = count,
                          group = Service.Name,
                          color = Service.Name)) +
  geom_line(size = 0.5) +
  xlab("") +
  ylab("Number of requests") +
  ggtitle("The change of service requests from 2011-2015")+
  theme(legend.title=element_text(size=10))

### graph "The change in the number of different requests by month in 2016" 

data16_type = data16 %>%
  filter(!is.na(ServiceDate)) %>%
  droplevels() %>%
  mutate(month = as.factor(month(ServiceDate, label = T)))%>%
  group_by(month, RequestType) %>%
  summarize(count = n())


data16_type$RequestType = 
  factor(data16_type$RequestType,
         levels = 
           c("Bulky Items",
             "Graffiti Removal",
             "Metal/Household Appliances",
             "Illegal Dumping Pickup",
             "Electronic Waste",
             "Homeless Encampment",
             "Single Streetlight Issue",
             "Multiple Streetlight Issue",
             "Dead Animal Removal"))

ggplot(data16_type, aes(x = month,
                        y = count,
                        group = RequestType,
                        color = RequestType)) +
  geom_line(size = 0.5) +
  xlab("") +
  ylab("Number of Requests") +
  ggtitle("The change in the number of different requests in 2016")

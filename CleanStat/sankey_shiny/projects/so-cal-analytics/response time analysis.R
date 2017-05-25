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


# change data 2 CreateDat and UpdateDate to date type

data16$CreatedDate = mdy_hms(data16$CreatedDate)
data16$ServiceDate = mdy_hms(data16$ServiceDate)
data16$UpdatedDate = mdy_hms(data16$UpdatedDate)
data16$ClosedDate = mdy_hms(data16$ClosedDate)

## Request Processing time analysis

data16_restime = data16 %>%
  filter(!is.na(UpdatedDate) &
           !is.na(CreatedDate)) %>%
  mutate(diftime = as.Date(UpdatedDate, format="%Y-%m-%d") -
           as.Date(CreatedDate, format="%Y-%m-%d")) 

ggplot(data16_restime, aes(x = as.factor(RequestType),
                           y = diftime)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,50)) +
  xlab("Request Type")+
  ylab("Response Time") +
  ggtitle("Response Time for Different Request Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


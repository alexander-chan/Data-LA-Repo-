## DATA MANIPULATION
#-------------------

library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(stringr)
library(zipcode)
library(gridExtra)

#loading files
data = read.csv("311_Call_Center_Tracking_Data.csv", header = T, sep = ",")
servicerequest = read.csv("MyLA311_Service_Request_Data_2016.csv", header = T, sep = ",")

#data manipulation and extraction using lubridate functions on date
data$Date = mdy(data$Date)
data$Time = hms(data$Time)
servicerequest$CreatedDate = mdy_hms(servicerequest$CreatedDate)
servicerequest$UpdatedDate = mdy_hms(servicerequest$UpdatedDate)
servicerequest$Month = month(servicerequest$CreatedDate)
servicerequest$Year = year(servicerequest$CreatedDate)  
servicerequest$Hour = hour(servicerequest$CreatedDate)
servicerequest$Day = wday(servicerequest$CreatedDate, label = TRUE)
servicerequest$UpdatedDate = ymd_hms(servicerequest$UpdatedDate)
servicerequest$ServiceDate = mdy_hms(servicerequest$ServiceDate)
servicerequest$ClosedDate = mdy_hms(servicerequest$ClosedDate)

#joining zipcodes to access longitude and latitude information by zipcodes
data("zipcode")
zipcode$zip = as.numeric(zipcode$zip)
data1 = left_join(data, zipcode, by = c("Zip.Code" = "zip"))

#data manipulation on new dataset
data1$Year = year(data1$Date)
data1$Month = month(data1$Date)
data1$Weekday = wday(data1$Date, abbr = F, label = T)
data1$Hour = hour(data1$Time)


#subsetting to create new datasets for change volume in requests
change = data1 %>%
filter(!is.na(Year)) %>%
group_by(Year, Call.Resolution) %>%
summarize(count = n()) %>%
mutate(percent = round(count/sum(count),3))

change1 = change %>%
mutate(colorcode = 
         ifelse(Call.Resolution == "Transfer (City)" | Call.Resolution == "Referred To County" |
                 Call.Resolution == "Warm Transfer (City)" | Call.Resolution == "Got Voicemail (City)" | 
                Call.Resolution == "Referred To State" | Call.Resolution == "Line Busy (City)", "Decreasing", 
             ifelse(Call.Resolution == "Service Request Processed" | Call.Resolution == "Gave Caller Information", "Increasing", "Constrant")))

#subsetting dataset for COUNCIL DISTRICT - GEOGRAPHIC TRENDS

sr = servicerequest %>%
 filter(Latitude != "" | Longitude != "")

cd_1 = sr %>%
 filter(CD == 1)

 cd_2 = sr %>%
  filter(CD == 2)

  cd_3 = sr %>%
filter(CD == 3)

cd_4 = sr %>%
 filter(CD == 4)

 cd_5 = sr %>%
  filter(CD == 5)

cd_6 = sr %>%
 filter(CD == 6)

cd_7 = sr %>%
 filter(CD == 7)

cd_8 = sr %>%
 filter(CD == 8)

cd_9 = sr %>%
 filter(CD == 9)

cd_10 = sr %>%
filter(CD == 10)

cd_11 = sr %>%
 filter(CD == 11)

cd_12 = sr %>%
filter(CD == 12)

cd_13 = sr %>%
filter(CD == 13)

cd_14 = sr %>%
filter(CD == 14)

cd_15 = sr %>%
filter(CD == 15)

#loading into rda file format
save(data1, change, change1, servicerequest, sr, cd_1, cd_2, cd_3, cd_4, cd_5, cd_6,
     cd_7, cd_8, cd_9, cd_10, cd_11, cd_12, cd_13, cd_14, cd_15, file = "FinalData1.rda")

#---------------------------------------------------

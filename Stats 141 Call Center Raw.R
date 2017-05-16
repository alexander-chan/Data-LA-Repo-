library(lubridate)
library(zoo)
MyLA311_Service_Request_Data_2017 <- read_csv("C:/Users/Joseph/Downloads/MyLA311_Service_Request_Data_2017.csv")
ServiceRequest2017 <- MyLA311_Service_Request_Data_2017[,c(1,2,3,4,5,6,7,8,9,10,21,22,23,24,28,29,31,32,33)]
BOSService <- ServiceRequest2017[ServiceRequest2017$Owner=="BOS",]
table(BOSService$Status)
BOSServiceC <- BOSService[BOSService$Status=="Closed",]
BOSServiceC$Created <- as.POSIXct(BOSServiceC$CreatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles") 
BOSServiceC$Updated <- as.POSIXct(BOSServiceC$UpdatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles")
BOSServiceC$TimeTaken <- (as.numeric(BOSServiceC$Updated) - as.numeric(BOSServiceC$Created))/86400
summary(BOSServiceC$TimeTaken)
BOSServiceC <- BOSServiceC[!is.na(BOSServiceC$TimeTaken),]
BOSServiceC[BOSServiceC$TimeTaken < 0,]
BOSServiceC <- BOSServiceC[-BOSServiceC$TimeTaken <0,]
BOSServiceC[BOSServiceC$TimeTaken < 0,]

#Will change to dplyr
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,mean)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,var)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,max)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,min)

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

library(dplyr)
# BOSServiceC %>% 
#   group_by(Created, Updated, RequestType, RequestSource, CD) %>%  
#   summarise(count = n())


x <- BOSServiceC %>% 
  group_by(RequestType, RequestSource, CD) %>%  
  summarise(count = n(), mean(TimeTaken), median(TimeTaken), var(TimeTaken), min(TimeTaken), max(TimeTaken),
            mean(hour(Updated)))

x2 <- BOSServiceC %>% 
  group_by(CD) %>% 
  summarise(mean(TimeTaken), median(TimeTaken), mean(hour(Updated)))

x3 <- merge(x2, BOSServiceC, by = 'CD')

x3$is_day <- ifelse(x3$TimeTaken <= 1, 1, 0)
x3$is_week <- ifelse(x3$TimeTaken <= 7, 1, 0)
x3$is_month <- ifelse(x3$TimeTaken <= 30, 1, 0)
library(ggplot2)
ggplot(data = na.omit(x)) + 
  geom_boxplot(aes(x = as.factor(CD), y = `mean(TimeTaken)`,colour = as.factor(CD))) +
  theme_bw()

ggplot(data = na.omit(x)) +
  geom_boxplot(aes(x = as.factor(CD), y = `median(TimeTaken)`, colour = as.factor(CD))) +
  theme_bw()

ggplot(data = na.omit(x)) +
  geom_boxplot(aes(x = as.factor(CD), y = `mean(hour(Updated))`, colour = as.factor(CD))) +
  theme_bw()

library(ggmap)
LA <- get_map('Los Angeles')
ggmap(LA) + 
  geom_point(data = na.omit(x3), aes(x = Longitude, y = Latitude, colour = as.factor(CD)))

ggmap(LA) +
  geom_point(data = na.omit(x3), aes(x = Longitude, y = Latitude, colour = as.factor(TimeTaken)))

ggmap(LA) + 
  geom_point(data = na.omit(BOSServiceC), aes(x = Longitude, y = Latitude, colour = as.factor(RequestSource)))

ggmap(LA) +
  geom_point(data = na.omit(BOSServiceC), aes(x = Longitude, y = Latitude, colour = as.factor(RequestType)))

ggmap(LA) + 
  geom_point(data = na.omit(x3), aes(x = Longitude, y = Latitude, colour = as.factor(is_day)))

ggmap(LA) +
  geom_point(data = na.omit(x3), aes(x = Longitude, y = Latitude, colour = as.factor(is_week)))

ggmap(LA) +
  geom_point(data = na.omit(x3), aes(x = Longitude, y = Latitude, colour = as.factor(is_month)))

ggmap(LA) +
  geom_bin2d(data = na.omit(x3), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType)))

ggmap(LA) + 
  geom_bin2d(data = na.omit(x3[x3$CD == 1,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 2,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 3,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 4,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 5,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 6,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 7,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 8,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 9,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 10,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 11,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 12,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 13,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 14,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType))) +
  geom_bin2d(data = na.omit(x3[x3$CD == 15,]), aes(x = Longitude, y = Latitude, fill = as.factor(RequestType)))

ggmap(LA) +
  stat_density2d(data = na.omit(x3[,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
  scale_fill_gradient(low = 'yellow', high = 'red')

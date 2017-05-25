
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)
library(maps)
library(viridis)


load(file = "calldata.rda")
load(file = "calldata_2.rda")
load(file = "la_zip.rda")
la <- qmap("2000 Wellington Rd Los Angeles, CA 90016", zoom = 11, color = "bw")


#Sort Call Resolution
## Give Caller Info
call1 %>% 
  filter(Call.Resolution == "Gave Caller Information") %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+ 
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Giving Caller Information'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))



call1 %>% 
  filter(Call.Resolution == "Gave Caller Information") %>% 
  group_by(Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(Zip.Code, count), y = count))+
  geom_bar(stat = "identity")+
  labs(title = "Top 10 Zipcode Given Info",
       x = "Zip Code",
       y = "Count")

d1 <- call1 %>% 
  filter(Call.Resolution == "Gave Caller Information") %>% 
  group_by(Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

j1 <- left_join(lazip, d1, by = c("id" = "Zip.Code"))

la+
  geom_polygon(data = j1, 
               aes(x = long, y = lat, fill = count, group = group, alpha = count),
               color = "Darkgrey")+
  scale_fill_viridis()+
  labs(title = "Spatial Distribution of 'Giving Caller Information'",
       x = "Longtitude",
       y = "Latitude",
       fill = "Frequency",
       alpha = "Transparency")



## Invalid Info

call1 %>% 
  filter(Call.Resolution %in% c("Caller Hung Up", "Got Voicemail (City)", 
                                "Info Not Available (Non-City)", 
                                "Line Busy (City)", 
                                "N/A", 
                                "Static/Ghost Call")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Invalid Information'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))






call1 %>% 
  filter(Call.Resolution %in% c("Got Voicemail (City)",  
                                "Line Busy (City)")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Voice Mail & Line Busy'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))




call1 %>% 
  filter(Call.Resolution %in% c("Info Not Available (Non-City)")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Information Not Available'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))


d2 <- call1 %>% 
  filter(Call.Resolution %in% c("Caller Hung Up", "Got Voicemail (City)", 
                                "Info Not Available (Non-City)", 
                                "Line Busy (City)", 
                                "N/A", 
                                "Static/Ghost Call")) %>% 
  group_by(Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

j2 <- left_join(lazip, d2, by = c("id" = "Zip.Code"))

la+
  geom_polygon(data = j2, 
               aes(x = long, y = lat, fill = count, group = group, alpha = count),
               color = "Darkgrey")+
  scale_fill_viridis()+
  labs(title = "Spatial Distribution of 'Invalid Information'",
       x = "Longtitude",
       y = "Latitude",
       fill = "Frequency",
       alpha = "Transparency")



## Referred Info

call1 %>% 
  filter(Call.Resolution %in% c("Referred To 411", 
                                "Referred To County",
                                "Referred To Other Governmental", 
                                "Referred To State")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Referred Calls'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))



d3 <- call1 %>% 
  filter(Call.Resolution %in% c("Referred To 411", 
                                "Referred To County",
                                "Referred To Other Governmental", 
                                "Referred To State")) %>% 
  group_by(Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

j3 <- left_join(lazip, d3, by = c("id" = "Zip.Code"))

la+
  geom_polygon(data = j3, 
               aes(x = long, y = lat, fill = count, group = group, alpha = count),
               color = "Darkgrey")+
  scale_fill_viridis()+
  labs(title = "Spatial Distribution of 'Referred Calls'",
       x = "Longtitude",
       y = "Latitude",
       fill = "Frequency",
       alpha = "Transparency")



## Transferred Info
call1 %>% 
  filter(Call.Resolution %in% c("Transfer (City)", 
                                "Transferred To 411", 
                                "Warm Transfer (City)")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Transferred Calls'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))



d4 <- call1 %>% 
  filter(Call.Resolution %in% c("Transfer (City)", 
                                "Transferred To 411", 
                                "Warm Transfer (City)")) %>% 
  group_by(Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

j4 <- left_join(lazip, d4, by = c("id" = "Zip.Code"))

la+
  geom_polygon(data = j4, 
               aes(x = long, y = lat, fill = count, group = group, alpha = count),
               color = "Darkgrey")+
  scale_fill_viridis()+
  labs(title = "Spatial Distribution of 'Transferred Calls'",
       x = "Longtitude",
       y = "Latitude",
       fill = "Frequency",
       alpha = "Transparency")



## Processed Info
call1 %>% 
  filter(Call.Resolution %in% c("Service Request Processed")) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Request Processed'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))




## 90012
call1 %>% 
  filter(Zip.Code == 90012) %>% 
  group_by(wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = hr, fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 24, 1))+
  labs(title = "Time Distribution of 'Zip Code 90012'",
       x = "Weekdays",
       y = "Hour",
       fill = "Count")+
  theme(panel.background = element_rect(color = "black"))




call1 %>% 
  filter(Zip.Code == 90012,
         Service.Name != "") %>% 
  group_by(Service.Name) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(Service.Name, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  theme_classic()+
  labs(title = "Top 5 Request Type of 'Zip Code 90012'",
       x = "Service Name",
       y = "Count")+
  theme(panel.background = element_rect(color = "black"))+
  coord_flip()+
  scale_fill_viridis()



call1 %>% 
  filter(Service.Name != "") %>% 
  group_by(Service.Name) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(Service.Name, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  theme_classic()+
  labs(title = "Top 5 Request Type in 2011-2015",
       x = "Service Name",
       y = "Count")+
  theme(panel.background = element_rect(color = "black"))+
  coord_flip()+
  scale_fill_viridis()



newdata %>% 
  filter(ZipCode == 90011,
         RequestType != "") %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(RequestType, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  theme_classic()+
  labs(title = "Top 5 Request Type of 'Zip Code 90011'",
       x = "Service Name",
       y = "Count")+
  theme(panel.background = element_rect(color = "black"))+
  coord_flip()


newdata %>% 
  filter(RequestType != "") %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(RequestType, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  theme_classic()+
  labs(title = "Top 5 Request Type in 2016",
       x = "Service Name",
       y = "Count")+
  theme(panel.background = element_rect(color = "black"))+
  coord_flip()


call1 %>% 
  filter(Zip.Code != 99999) %>% 
  group_by(yr, Zip.Code) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

newdata %>% 
  group_by(ZipCode) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Yearly Time Distribution 2011-2015

call1 %>% 
  filter(!is.na(yr)) %>% 
  group_by(yr, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Time Distribution 2011-2015", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~yr)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 6))


## Monthly Time Distribution 2011-2015
call1 %>% 
  filter(!is.na(yr)) %>% 
  group_by(mth, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Montly Call Time Distribution 2011-2015", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~mth)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 4))


## Yearly Spatial Distribution 2011-2015

data1 <- call1 %>% 
  filter(!is.na(Zip.Code),
         !Zip.Code %in% c(99999),
         Service.Name != "") %>% 
  group_by(yr, mth, Zip.Code, Service.Name) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

joindata1 <- left_join(lazip, data1, by = c("id" = "Zip.Code"))

la+
  geom_polygon(data = joindata1, 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Frequency by Zip Area 2011-2015", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~yr)



## Monthly Spatial Distribution 2011-2015

la+
  geom_polygon(data = joindata1, 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Frequency by Zip Area 2011-2015", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~mth)

## Yearly Time Distribution 2016
newdata$CreatedDate <- mdy_hms(newdata$CreatedDate)
newdata$UpdatedDate <- mdy_hms(newdata$UpdatedDate)
newdata <- newdata %>% 
  mutate(wd = wday(CreatedDate, label = T),
         mth = month(CreatedDate, label = T),
         hr = hour(CreatedDate),
         yr = year(CreatedDate))

str(newdata)

newdata %>% 
  filter(!is.na(yr),
         yr == 2016) %>% 
  group_by(yr, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Time Distribution 2016", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~yr)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5))


## Monthly Time Distribution 2016

newdata %>% 
  filter(!is.na(yr),
         yr == 2016) %>% 
  group_by(mth, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Time Distribution 2016", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~mth)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5))


## Yearly Spatial Distribution 2016

data2 <- newdata %>% 
  group_by(yr, mth, ZipCode, RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

str(newdata)
str(lazip)

newdata$ZipCode <- as.numeric(as.character(newdata$ZipCode))
lazip$id <- as.numeric(lazip$id)

joindata2 <- left_join(lazip, data2, by = c("id" = "ZipCode"))

la+
  geom_polygon(data = filter(joindata2, yr == 2016), 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Call Frequency by Zip Area 2016", fill =  "Frequency", alpha = "Transparency")


## Monthly Spatial Distribution 2016
la+
  geom_polygon(data = filter(joindata2, yr == 2016), 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Frequency by Zip Area 2016", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~mth)


## Efficiency

newdata <- newdata %>% 
  mutate(proc_time = UpdatedDate - CreatedDate)
newdata$proc_time <- round(newdata$proc_time/(60*60*24), digits = 0)

newdata %>% 
  group_by(RequestType) %>% 
  summarise(Average = mean(proc_time)) %>% 
  arrange(-Average)


## Average efficiency by request type

a <- newdata %>% 
  group_by(RequestType) %>% 
  summarise(Average = mean(proc_time)) %>% 
  ggplot(aes(x = reorder(RequestType,Average), y = Average, fill = Average))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  labs(title = "Procedure Time", x = NULL, y = "Avg. Days")+
  scale_fill_viridis()+
  theme_classic()


## Total number of request of each type

levels(newdata$RequestType)
newdata$RequestType = factor(newdata$RequestType, 
                             levels = c("Report Water Waste",
                                        "Dead Animal Removal",
                                        "Graffiti Removal",
                                        "Metal/Household Appliances",
                                        "Electronic Waste",
                                        "Bulky Items",
                                        "Illegal Dumping Pickup",
                                        "Single Streetlight Issue",
                                        "Other",
                                        "Feedback",
                                        "Multiple Streetlight Issue",
                                        "Homeless Encampment"))
b <- newdata %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = RequestType, y = count/1000, fill = RequestType))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  labs(title = "Calling Number Count", x = NULL, y = "Count (in Thousands)")+
  theme_classic()+
  theme(axis.text.y = element_blank())+
  scale_fill_viridis(discrete = T)

grid.arrange(a, b, nrow = 1)


newdata %>% 
  group_by(RequestType) %>% 
  summarise(count = n(),
            sumtime = sum(proc_time),
            eff = count/sumtime) %>% 
  filter(!RequestType %in% c("Report Water Waste", "Other")) %>%
  arrange(-eff) %>% 
  ggplot(aes(x = reorder(RequestType, -eff), y = eff, fill = eff))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  scale_fill_viridis(direction = 1)+
  labs(title = "Efficiency Rating", x = "Request Type", y = "Efficiency Ratio")+
  theme_classic()

## low eff - homeless encampment

newdata %>% 
  filter(RequestType == "Homeless Encampment") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/14497, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Homeless Encampment", x = "Request Sourece", y = "Percentage")+
  theme_classic()

newdata %>% 
  filter(RequestType == "Homeless Encampment") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Homeless Encampment",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

## low eff - multiple street light issue

newdata %>% 
  filter(RequestType == "Multiple Streetlight Issue") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/4989, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Multiple Streetlight Issue", x = "Request Sourece", y = "Percentage")+
  theme_classic()


newdata %>% 
  filter(RequestType == "Multiple Streetlight Issue") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Multiple Streetlight Issue",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

## large request number - bulky item

newdata %>% 
  filter(RequestType == "Bulky Items") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/542372, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Bulky Items", x = "Request Sourece", y = "Percentage")+
  theme_classic()

newdata %>% 
  filter(RequestType == "Bulky Items") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Bulky Items",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))


## large request number - graffiti removal

newdata %>% 
  filter(RequestType == "Graffiti Removal") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/279723, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Graffiti Removal", x = "Request Sourece", y = "Percentage")+
  theme_classic()

newdata %>% 
  filter(RequestType == "Graffiti Removal") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Graffiti Removal",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

## high eff - dead animal removal

newdata %>% 
  filter(RequestType == "Dead Animal Removal") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/31697, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Dead Animal Removal", x = "Request Sourece", y = "Percentage")+
  theme_classic()

newdata %>% 
  filter(RequestType == "Dead Animal Removal") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Dead Animal Removal",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

newdata %>% 
  group_by(RequestType, wd, hr, RequestSource) %>% 
  summarise(avg = mean(proc_time)) %>% 
  ggplot(aes(x = factor(hr), y = avg, fill = RequestSource))+
  geom_bar(stat = "identity", position = "dodge", show.legend = F)+
  facet_wrap(~RequestSource)+
  theme_minimal()+
  labs(title = "Avg. Procedure Time via Each Request Source in 24 Hours", x = "Hour in a Day", y = "Procedure Days")+
  theme(axis.text.y = element_text(size = 4))+
  scale_fill_viridis(discrete = T)+
  coord_flip()

newdata %>% 
  filter(RequestSource %in% c("Mobile App")) %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ggplot(aes(x = reorder(RequestType, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  coord_flip()+
  theme_classic()+
  labs(title = "Ranking of Request Type via Mobile App", x = "Request Type", y = "Count")

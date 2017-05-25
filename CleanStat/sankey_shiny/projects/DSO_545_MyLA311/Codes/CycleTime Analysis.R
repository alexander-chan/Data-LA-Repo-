library(RSQLite)
library(XML)
library(rvest)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(maps)


driver <- dbDriver("SQLite")
con <- dbConnect(driver, "/Users/pksharma/Desktop/DSO 545 Project/Database.sqlite")

## 1.List all of the tables in the database
dbListTables(con)

SQL="SELECT * FROM Request_data"
datasql=dbGetQuery(con, SQL)

unique(datasql$RequestType)


datasql$CreatedDate = mdy_hms(datasql$CreatedDate)
datasql$UpdatedDate = mdy_hms(datasql$UpdatedDate)
datasql$ServiceDate = mdy_hms(datasql$ServiceDate)
datasql$ClosedDate = mdy_hms(datasql$ClosedDate)

datasql = mutate(datasql, CycleTime = (ServiceDate - CreatedDate))
datasql$CycleTime = (datasql$CycleTime/86400)
datasql$CycleTime = as.numeric(datasql$CycleTime)
datasql = mutate(datasql, Month = month(CreatedDate, label = T, abbr = F))
datasql = mutate(datasql, Year = year(CreatedDate))
datasql = mutate(datasql, Day = wday(CreatedDate, label = T, abbr = F))
datasql =
  mutate(datasql,ResponseTime = ifelse(CycleTime < 0,"Pre-Resolved",
           ifelse(CycleTime <= 1,"Quick",
                               ifelse(CycleTime <= 3,"Average","Slow"))))

#Spark
library(sparklyr)
cs <- spark_connect(master = "local")
requests <- copy_to(dest = cs,df = datasql, name = "reqdata")
rm(cdata)


datasql %>%
  filter(!is.na(datasql$CycleTime) & datasql$CycleTime > 0) %>%
  summarise(Mean = sd(CycleTime))

b = datasql %>%
  filter(Year == 2016 & !is.na(datasql$CycleTime) & datasql$CycleTime > 0) %>%
  group_by(Month) %>%
  summarise(AvgCycleTime = mean(CycleTime), Calls = n())


#Avg CycleTime
a = datasql %>%
  filter(!is.na(datasql$CycleTime) & datasql$CycleTime > 0
         & RequestSource %in% c("Mobile App","Call", "Driver Self Report", "Self Service")) %>%
  group_by(RequestType, RequestSource) %>%
  summarise(AvgCycleTime = mean(CycleTime), Calls = n()) %>%
  ggplot(aes(x = RequestType, y = AvgCycleTime, fill = RequestSource)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = seq(0,400,100)) +
  ggtitle("Request Distribution") + xlab("") + ylab("Number of calls in Thousands") +
  theme(plot.title = element_text(color = "black", face = "bold", size = 16),
        axis.text = element_text(angle = 30, hjust = 1)) 
    
write.csv(b,"Data2.csv")
#CycleTime for only bulky items
datasql %>%
  filter(RequestType == 'Bulky Items') %>%
  ggplot(aes(x = Month, y = CycleTime)) +
  geom_jitter(alpha=0.8,color = "blue",size = 1.5,
              position = position_jitter(width = 0.5)) +
  stat_smooth(method = "loess", level = 0.95)


datasql %>%
  filter(RequestType == 'Bulky Items') %>%
  group_by(month(UpdatedDate, label = T, abbr = F)) %>%
  summarise(AvgCycleTime = (mean(CycleTime)))
  ggplot(aes(x = Month, y = mean(CycleTime))) +
  geom_jitter()
  geom_jitter(alpha=0.8,color = "blue",size = 1.5,
                position = position_jitter(width = 0.5)) +
    coord_trans(y = "log") +
    stat_smooth(method = "loess")

datasql %>%
    filter(RequestType == 'Bulky Items') %>%
    group_by(Month) %>%
    summarise(AvgCycleTime = (mean(CycleTime))) %>%
    ggplot(aes(x = factor(Month), y = AvgCycleTime)) +
    geom_point()


datasql %>%
  ggplot(aes(x = Month, y = CycleTime)) +
  geom_jitter(alpha=0.8,color = "blue",size = 1.5,
              position = position_jitter(width = 0.5)) +
  stat_smooth(method = "loess", level = 0.95)
  facet_wrap(~RequestType)


# Avg CycleTime by Day  
datasql %>%
  group_by(Day) %>%
  summarise(AvgCT = mean(CycleTime)) %>%
  ggplot(aes(Day,AvgCT)) + geom_bar(stat = "identity")

# Avg CycleTime by Month  
datasql %>%
  group_by(Month) %>%
  summarise(AvgCT = mean(CycleTime)) %>%
  ggplot(aes(Month,AvgCT)) + geom_bar(stat = "identity")

# Calls by Day
datasql %>%
  group_by(Day) %>%
  summarise(Calls = n()) %>%
  ggplot(aes(Day,Calls)) + geom_bar(stat = "identity")

# Calls by Month
datasql %>%
  filter(Year == 2016) %>%
  group_by(Month, RequestSource) %>%
  summarise(Calls = n()) %>%
  ggplot(aes(Month,Calls)) +
  geom_bar(stat = "identity")

#Calls by Day
max((datasql$CreatedDate)) - min((datasql$CreatedDate))
nrow(datasql)/470.6117

# Contour plots

install.packages(ggmap)
library(ggmap)
LosAngeles = qmap("Beverly Hills", zoom = 11, color = "bw", maptype = "roadmap")

c =  datasql %>%
  filter(RequestType == "Bulky Items" & Month == "August") 

LosAngeles + 
stat_density2d(data = c, aes(Longitude,Latitude, fill = ..level..),
               bins=5, geom = "polygon", alpha = 0.4) +
  scale_fill_gradient(low = "green", high = "red") +
  guides(fill = FALSE) + 
  ggtitle("Calls since August 2015")


qmap("Beverly Hills", zoom = 10, color = "bw", maptype = "roadmap") +
  stat_bin_2d(data = datasql, 
              aes(Longitude,Latitude, fill = ResponseTime),
              size = 0.01, bins=100, alpha=0.5) +
  scale_fill_manual(values = c("grey","white","red","green")) + 
  theme(legend.position = c(0.88,0.88)) + 
  guides(fill = guide_legend(reverse = TRUE))

qmap("Beverly Hills", zoom = 10, color = "bw", maptype = "roadmap") +
  stat_bin_2d(data = datasql, 
              aes(Longitude,Latitude),
              size = 0.01, bins=100, alpha=0.5) +
  facet_wrap(~ResponseTime)

unique(as.numeric(datasql$ZipCode))


LosAngeles +
  stat_density2d(data = datasql, aes(Longitude,Latitude, fill = ..CycleTime..),
                 bins=5, geom = "polygon", alpha = 0.4) +
  scale_fill_gradient(low = "green", high = "red") +
  guides(fill = FALSE) +
  ggtitle("Calls since August 2015")

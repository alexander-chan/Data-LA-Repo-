library(lubridate)
library(dplyr)
library(ggmap)

data <- read.csv("MyLA311_Service_Request_Data_2016.csv")

data$CreatedDate <- mdy_hms(data$CreatedDate)

data <- data %>% 
  filter(year(CreatedDate) == 2016)
  
data <-  data %>% 
  select(Longitude,Latitude,RequestSource,RequestType,
         CreatedDate,ZipCode,PolicePrecinct,APC,MobileOS,Address)

income <- read.csv("zippop.csv")

income <- income[-1,c(2,5)]

colnames(income) <- c("ZipCode","income")

income$ZipCode <- factor(income$ZipCode)

data <- left_join(data,income)

save(data, file = "data311.Rda")

mobile <-  data %>% 
  filter(RequestSource == "Mobile App") %>% 
  group_by(ZipCode) %>% 
  summarise(count = n())

totals <- data %>% 
  group_by(ZipCode) %>% 
  summarise(count2 = n())

join <- left_join(totals,mobile)

join <- inner_join(income, join)

join <- join %>% 
  mutate(percent_mobile = count/count2)

colnames(join)[3:4] <- c("count_total","count_mobile")

join$ZipCode <- str_c(join$ZipCode, " Los Angeles", sep = "")

latlong <- geocode(join$ZipCode)

join <- cbind(join,latlong)

dsr <- data %>% 
  filter(RequestSource == "Driver Self Report") %>% 
  group_by(ZipCode) %>% 
  summarise(dsr_count = n())

join$ZipCode <- str_replace(join$ZipCode, " Los Angeles","")

join <- left_join(join,dsr)

join <- join %>% 
  mutate(percent_mobile_nodsr = count_mobile/(count_total-dsr_count))

join <- join %>% 
  filter(count_total > 100)

colnames(join)[8] <- "count_dsr"

join <- join %>% 
  mutate(count_citizen = count_total-count_dsr)

ziphour <- data %>% 
  filter(RequestSource == "Mobile App") %>% 
  group_by(ZipCode) %>% 
  summarise(mobile_hour = mean(hour(CreatedDate)))

join <- inner_join(join,ziphour)

save(join,file = "stats.Rda")

hvc <- data %>% 
  filter(RequestSource == "Call") %>% 
  group_by(Address) %>% 
  summarise(count = n()) 

hvc <- hvc %>% 
  filter(count > 20)

hvc$Address <- str_c(hvc$Address,"Los Angeles", sep = " ")

latlong <- geocode(hvc$Address)

hvc <- cbind(hvc,latlong)

save(hvc,file = "hvc.Rda")

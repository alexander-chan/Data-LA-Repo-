library(lubridate)
library(dplyr)
library(stringr)
library(readxl)
library(ggmap)

calls <- read.csv("311_Call_Center_Tracking_Data.csv")

colnames(calls)[1] <- "date"

calls$date <- mdy(calls$date)

calls <- calls %>% 
  filter(year(date) == 2014 & month(date) %in% 6:12 | year(date) == 2015)

calls <- calls %>% 
  mutate(transfer = str_detect(Call.Resolution,"Transfer"))

save(calls,file = "calls.Rda")

transfer <- calls %>% 
  filter(transfer == T)

sum(calls$transfer)/nrow(calls)

calls %>% 
  group_by(Call.Resolution) %>% 
  summarise(perc = n()*100/928600) %>% 
  arrange(desc(perc))

metro <- read_excel("metro.xlsx")

metro$Address <-  as.character(metro$Address)

latlong <- geocode(metro$Address)

metro <- cbind(metro,latlong)

save(metro,file = "metro.Rda")












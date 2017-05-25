load("tracking_cleaned.rda")
load("service.rda")

library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(gridExtra)

### Maps by Zip Code 
load("zipcode_map.rda")

# For Tracking data - map
zip_merge = data %>%
  group_by(Zip.Code) %>%
  summarise(count = n())

join_table = merge(zip_merge, zipcode_map,
                   by.x = "Zip.Code",
                   by.y = "id") 
join_table = join_table %>%
  filter(city == "Los Angeles") %>%
  arrange(order)

write.csv(join_table, "join_table.csv")

m1 = ggplot(join_table, aes(x = long, y = lat, fill = count, group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "#dd1c77", guide = F) + theme_light() +
  ggtitle("Tracking Data  Request Distribution by Zip Code") + xlab("")

### For Service data - map
zip_service = service %>%
  group_by(ZipCode) %>%
  summarise(count = n())
join_table_service = merge(zipcode_map,zip_service,
                           by.x = "id",
                           by.y = "ZipCode")
join_table_service = join_table_service %>%
  filter(city == "Los Angeles") %>%
  arrange(order)

write.csv(join_table_service, "join_table_service.csv")

m2 = ggplot(join_table_service, aes(x = long, y = lat, fill = count, group = group)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "#43a2ca", guide = F) + theme_light() +
  ggtitle("Service Request Distribution by Zip Code") + xlab("") + ylab("")

grid.arrange(m1, m2, nrow = 1, bottom = "long")



###  Request Type - bar chart

rsrt = service %>%
  group_by(RequestType, RequestSource) %>%
  summarise(count = n()) %>%
  arrange(-count) 

ggplot(rsrt,aes(x=reorder(RequestType,count),y=count))+
  geom_bar(stat="identity",fill = "pink") +
  coord_flip()+
  theme_classic()+
  ylab("Request Volume") +
  xlab("Request Type")

### Request Type - Comparison

ggplot(rsrt,aes(x=reorder(RequestType,count),y=count))+
  geom_bar(stat="identity",aes(color=RequestSource,fill=RequestSource),alpha=0.7)+
  coord_flip()+
  theme_classic()+
  ylab("Request Volume")+
  xlab("Request Type")+
  scale_y_continuous(breaks=seq(0,550000,30000),
                     labels=seq(0,550000,30000),
                     limits=c(0,580000))

### Request Sources - bar chart
request = service %>%
  group_by(RequestSource) %>%
  summarise(count = n()) %>%
  arrange(-count) 

write.csv(request, "request.csv")

ggplot(request, aes(x = reorder(RequestSource, count), y = count)) + 
  geom_bar(stat = "identity", fill = "pink") + xlab("Request Source") + ylab("") +
  ggtitle("Top Request Sources") + theme_light() +
  coord_flip() + xlab("") + ylab("Request Volume") + 
  geom_text(aes(label = count, hjust = 1), fontface = "bold")




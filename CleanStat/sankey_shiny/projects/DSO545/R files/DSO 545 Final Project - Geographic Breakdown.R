LAData <- read.csv("Data_merged.csv")
library(zipcode)
library(ggmap)
library(dplyr)
library(ggplot2)

LosAngelesmap <- "Los Angeles California"
LosAngeles_map1 <- get_map(location = LosAngelesmap, zoom = 10)
LosAngeles <- qmap(LosAngelesmap, zoom = 11, maptype = "terrain", color = "bw")
?maptype
###Heatmap of Request Types by Geography
LADataBulkyItems <- LAdata %>%
  filter(RequestType == "Bulky Items")
LADataBulkyItems$Created_Day <- factor(LADataBulkyItems$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))
LosAngeles +
  stat_density2d(data = LADataBulkyItems,
                 aes(x= Longitude, 
                     y = Latitude, 
                     fill = ..level..), 
                 geom = "polygon", 
                 alpha = 0.5, 
                 bins = 5) +
  scale_fill_gradient(low = "#2AB69D",
                       high = "#E65848") +
  facet_wrap(~Created_Day) +
  theme(legend.position = "none") +
  ggtitle("Bulky Item Pickup Requests by Day of Week")

###Bar graph of requests by zip code by day of week
CDDayofWeek <- LAData %>%
  group_by(Created_Day, CD) %>%
  summarise(Count = n())

ggplot(CDDayofWeek, aes(x=Created_Day, y= Count)) +
  geom_bar(stat = "identity") +
  facet_grid(CD~.)
  


LADataBulkyItemsbyZip <- LAData %>%
  filter(RequestType == "Bulky Items") %>%
  group_by(ZipCode, Num_of_Households) %>%
  summarise(Count = n())

LADataBulkyItemsbyZip <- LADataBulkyItemsbyZip %>%
  mutate(RequestsPerHousehold = Count/Num_of_Households)
  
LADataZipCodeHouseholds <- LAData %>%
  filter(RequestType == "Bulky Items") %>%
  group_by(ZipCode, Num_of_Households, Median.Household.Income) %>%
  summarise(Count = n())


library(ggplot2)
library(dplyr)
library(stringr)
library(ggmap)

old = read.csv("311_Call_Center_Tracking_Data.csv")
new = read.csv("MyLA311_Service_Request_Data_2016.csv")

## maps
LA = "Los Angeles"
LosAngeles <- qmap(LA, zoom = 10, color = "bw", 
                   maptype = "roadmap", legend="topright")

new_location = new %>%
  filter(!is.na(Longitude)) %>%
  group_by(Longitude, Latitude) %>%
  summarize(Count = n()) %>%
  arrange(-Count) %>%
  mutate(Year = "2016")

LosAngeles + 
  geom_point(data = new_location,
             aes(x = Longitude, y = Latitude), 
             color = "darkblue", size = 2, alpha = 0.01)

homeless = new %>%
  filter(str_detect(RequestType,"Homeless")) %>%
  group_by(Owner) %>%
  summarize(Count = n()) %>%
  arrange(-Count)


homeless_old = old %>%
  filter(str_detect(Service.Name,"Homeless")) %>%
  group_by(Department.Abbreviation) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

## Bulky Item Analysis
new_bulky = new %>%
  filter(RequestType == "Bulky Items",
         NCName !="") %>%
  group_by(NCName) %>%
  summarize(Count = n()) %>%
  arrange(-Count)


new_bulky = new %>%
  filter(RequestType == "Bulky Items",
         NCName !="") %>%
  group_by(NCName) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

ggplot(data = head(new_bulky, 10), aes(x = reorder(NCName, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Top 10 Areas of Bulky Item Requests") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 60, hjust = 1))

  
new_bulky_zipcode = new %>%
  filter(RequestType == "Bulky Items") %>%
  group_by(ZipCode) %>%
  summarize(Count = n()) %>%
  arrange(-Count)


ggplot(data = head(new_bulky_zipcode, 10), aes(x = reorder(ZipCode, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Top 10 Areas of Bulky Item Requests") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 60, hjust = 1))

### Density Analysis
## Downtown
DTLA <- qmap("Downtown Los Angeles", zoom = 14, 
             maptype = "roadmap", color = "bw")


dtla = new %>%
  filter(str_detect(NCName,"DOWNTOWN")) %>%
  filter(RequestType == "Bulky Items" | RequestType == "Graffiti Removal") %>%
  group_by(NCName) 
  
DTLA +
  stat_density2d(data = dtla,
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", bins = 5, alpha = 0.3) +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = FALSE)

## Ktown
KTOWN <- qmap("Koreatwon Los Angeles", zoom = 14, 
             maptype = "roadmap", color = "bw")

ktown = new %>%
  filter(str_detect(NCName,"KOREATOWN")) %>%
  filter(RequestType == "Bulky Items" | RequestType == "Graffiti Removal") %>%
  group_by(NCName) 

KTOWN +
  stat_density2d(data = ktown,
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", bins = 5, alpha = 0.3) +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = FALSE)

## North Hollywood
NORHL <- qmap("North Hollywood Los Angeles", zoom = 14, 
              maptype = "roadmap", color = "bw")

norh = new %>%
  filter(str_detect(NCName,"NORTH HOLLYWOOD")) %>%
  filter(RequestType == "Bulky Items" | RequestType == "Graffiti Removal") %>%
  group_by(NCName) 

NORHL +
  stat_density2d(data = norh,
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", bins = 5, alpha = 0.3) +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = FALSE)

## South Central
SOCEN <- qmap(location = c(min(scentral$Longitude), min(scentral$Latitude)), zoom = 13, 
              maptype = "roadmap", color = "bw")
SOCEN

scentral = new %>%
  filter(str_detect(NCName,"SOUTH CENTRAL"),
         RequestType == "Bulky Items" | RequestType == "Graffiti Removal",
         Longitude !="") 

SOCEN +
  stat_density2d(data = scentral,
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", bins = 5, alpha = 0.3) +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = FALSE)


# Areas
new_apc = new %>%
  filter(APC != "") %>%
  group_by(APC) %>%
  summarize(Count = n()) %>%
  arrange(-Count) %>%
  mutate(Year = "2016")

ggplot(new_apc, aes(x = reorder(APC, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Request Volume by Area in 2016") +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1)) 


new_area = new %>%
  filter(NCName != "") %>%
  group_by(NCName) %>%
  summarize(Count = n()) %>%
  arrange(-Count) %>%
  mutate(Year = "2016")

ggplot(data = head(new_area, 10), aes(x = reorder(NCName, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Top 10 Blocks by Request Volume in 2016") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


## ZipCode
old_zipcode = old %>%
  filter(!is.na(Zip.Code) & Zip.Code != 99999) %>%
  group_by(Zip.Code) %>%
  summarize(Count = n()) %>%
  arrange(-Count) %>%
  mutate(Year ="Before2016")

colnames(old_zipcode)[1] <- "ZipCode"

ggplot(data = head(old_zipcode,10), aes(x = reorder(ZipCode, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Top 10 ZipCode by Request Volume before 2016") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold")) 

new_zipcode = new %>%
  filter(!is.na(ZipCode)) %>%
  group_by(ZipCode) %>%
  summarize(Count = n()) %>%
  arrange(-Count) %>%
  mutate(Year ="2016")

ggplot(data = head(new_zipcode,10), aes(x = reorder(ZipCode, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  ggtitle("Top 10 ZipCode by Request Volume in 2016") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold")) 

zipcode = merge(old_zipcode, new_zipcode, by.x = "ZipCode", by.y = "ZipCode", all.x = FALSE)
colnames(zipcode)[2] <- "Before2016"
colnames(zipcode)[4] <- "2016"

zipcode = zipcode %>%
  mutate(Change = Before2016 - `2016`) %>%
  arrange(-Change)

ggplot(data = head(zipcode, 10), aes(x = reorder(ZipCode, -Change), y = Change)) +
  geom_bar(stat = "identity", position = position_dodge(), 
            fill = "lightblue", width = 0.5) +
  ggtitle("Top 10 Request Volume Decrease by ZipCode Area") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold")) 

old_zipcode = old_zipcode %>%
  filter(ZipCode %in% new_zipcode$ZipCode) %>%
  arrange(-Count)
new_zipcode =  filter(new_zipcode, ZipCode %in% old_zipcode$ZipCode)
zipcode2 = rbind(old_zipcode, new_zipcode)

old_zipcode_top10 = head(old_zipcode, 10)
new_zipcode_top10 = filter(new_zipcode, ZipCode %in% old_zipcode_top10$ZipCode)
zipcode_top10 = rbind(old_zipcode_top10, new_zipcode_top10)

zipcode_top10$Year = as.factor(zipcode_top10$Year)
zipcode_top10$Year = factor(zipcode_top10$Year, 
                            levels = levels(zipcode_top10$Year)[c(2, 1)])

ggplot(zipcode_top10, aes(x = reorder(ZipCode, -Count), y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Change of Request Volume by ZipCode") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  theme(plot.title = element_text(size = 14, face = "bold"))

top20zipcode = head(new_zipcode, 20)
ggplot(top20zipcode, aes(x = reorder(ZipCode, -Count), y = Count)) + 
  geom_bar(stat = "identity", color = "red", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count), vjust = -2) +
  ggtitle("Top 10 ZipCode by Request Volume in 2016") +
  xlab("ZipCode")
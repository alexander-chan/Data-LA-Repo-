library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

data1 = read.csv("311_Call_Center_Tracking_Data.csv")
data2 = read.csv("MyLA311_Service_Request_Data_2016.csv")

zipcode = read.csv("zip code.csv")

data15 = data1 %>%
  filter(Call.Resolution == "Service Request Processed")
data16 = data2

## "Graffiti Removal" by Neighborhood Council
data_nc_gr = data16 %>%
  filter(NCName != "" & 
           RequestType == "Graffiti Removal") %>%
  group_by(NCName) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) 


ggplot(data_nc_gr, aes(x = reorder(NCName,count),
                       y = count,
                       fill = "darkred")) +
  geom_histogram(stat = "identity") +
  coord_flip() +
  xlab("Neighborhood Council") +
  ylab("Number of Requests") +
  guides(fill = F)


## "Graffiti removal" in Boyle Height
data16_gr_bh = data16_nbh %>%
  filter(ZipCode == 90033 &
           !is.na(ServiceDate) )%>% 
  mutate(month = as.factor(month(ServiceDate, label = T))) %>%
  group_by(month) %>%
  summarize(count = n())

ggplot(data16_gr_bh, aes(x = month, 
                         y = count,
                         group = 1,
                         color = "darkred")) +
  geom_line(size = 1) +
  xlab("") +
  ylab("Number of Requests") +
  ggtitle("Graffiti Removal Requests in Boyle Heights")

## bulky items by Neighborhood Council
data_nc_bi = data16 %>%
  filter(NCName != "" & 
           RequestType == "Bulky Items") %>%
  group_by(NCName) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) 

ggplot(data_nc_bi, aes(x = reorder(NCName,count),
                       y = count,
                       fill = "darkred")) +
  geom_histogram(stat = "identity") +
  coord_flip() +
  xlab("Neighborhood Council") +
  ylab("Number of Requests") +
  guides(fill = F)

## "Bulky Items" in SouthEast Area
data16_bi_se = data16_nbh %>%
  filter(ZipCode == 90044 &
           !is.na(ServiceDate) )%>% 
  mutate(month = as.factor(month(ServiceDate, label = T))) %>%
  group_by(month) %>%
  summarize(count = n())

ggplot(data16_bi_se, aes(x = month, 
                         y = count,
                         group = 1,
                         color = "darkred")) +
  geom_line(size = 1) +
  xlab("") +
  ylab("Number of Requests") +
  ggtitle("Bulky Items Requests in SouthEast LA")

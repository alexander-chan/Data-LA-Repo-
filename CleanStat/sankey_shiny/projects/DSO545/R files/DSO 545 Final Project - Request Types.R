LAdata <- read.csv("Data.csv")
library(ggplot2)
library(dplyr)

###Request Types Graphed
LAdataRequestType <- LAdata %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestType, aes(x=reorder(RequestType, CountforAxis), y=CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type")

###Request Types vs Department
LAdataRequestTypevsDept <- LAdata %>%
  group_by(RequestType, Owner_Full) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypevsDept, aes(x=reorder(Owner_Full, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type")

### Request Types by Department
#Bureau of Sanitation
LAdataRequestTypeSan <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeSan, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Bureau of Sanitation")

#Office of Community Beautification
LAdataRequestTypeBeau <- LAdata %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeBeau, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Office of Community Beautification")

#Bureau of Street Lighting
LAdataRequestTypeLight <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Lightning")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeLight, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Bureau of Street Lighting")

#Information Technology Agency
LAdataRequestTypeIT <- LAdata %>%
  filter(Owner_Full %in% c("Information Technology Agency")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeIT, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Information Technology Agency")

#Bureau of Street Service
LAdataRequestTypeStreet <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Service")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeStreet, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Bureau of Street Service")

#Los Angeles Department of Water and Power
LAdataRequestTypeWatPow <- LAdata %>%
  filter(Owner_Full %in% c("Los Angeles Department of Water and Power")) %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeWatPow, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Number of Requests by Request Type for Los Angeles Department of Water and Power")

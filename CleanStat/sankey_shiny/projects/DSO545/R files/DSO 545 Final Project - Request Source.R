library(dplyr)
library(ggplot2)
LAdata <- read.csv("Data_merged.csv")
### Request Source Total
LAdataRequestSource <- LAdata %>%
  group_by(RequestSource, Owner_Full) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSource, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("")

### Request Source by Department
LAdataRequestSource <- LAdata %>%
  group_by(RequestSource, Owner_Full) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSource, aes(x=reorder(Owner_Full, CountforAxis), y= CountforAxis, fill = RequestSource)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("")

###Request Source by Individual Department
#Bureau of Sanitation
LAdataRequestSourceSan <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceSan, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Bureau of Sanitation")

#Office of Community Beautification
LAdataRequestSourceBeau <- LAdata %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceBeau, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Office of Community Beautification")

#Bureau of Street Lighting
LAdataRequestSourceLight <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Lightning")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceLight, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Bureau of Street Lighting")

#Information Technology Agency
LAdataRequestSourceIT <- LAdata %>%
  filter(Owner_Full %in% c("Information Technology Agency")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceIT, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Information Technology Agency")

#Bureau of Street Service
LAdataRequestSourceStreet <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Service")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceStreet, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Bureau of Street Service")

#Los Angeles Department of Water and Power
LAdataRequestSourceWatPow <- LAdata %>%
  filter(Owner_Full %in% c("Los Angeles Department of Water and Power")) %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourceWatPow, aes(x=reorder(RequestSource, CountforAxis), y= CountforAxis, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Source") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Request Type by Request Source for Los Angeles Department of Water and Power")

###Request Source by Request Type
LAdataRequestSourcevReqType <- LAData %>%
  group_by(RequestSource, RequestType) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestSourcevReqType, aes(x=reorder(RequestType, CountforAxis), y= CountforAxis, fill = RequestSource)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Request Type") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("")

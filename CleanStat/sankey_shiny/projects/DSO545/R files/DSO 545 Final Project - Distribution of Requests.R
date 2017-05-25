LAdata <- read.csv("Data.csv")
library(ggplot2)
library(dplyr)

##Question #1 - Distribution of Requests by Department
LAdataOwner <- LAdata %>%
  group_by(Owner_Full) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000) %>%
  mutate(PercentofRequests = (CountforAxis/sum(CountforAxis))*100)
  
ggplot(LAdataOwner, aes(x=reorder(Owner_Full, CountforAxis), y= CountforAxis)) +
  geom_bar(stat="identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Departments") +
  ylab("# of Requests (in thousands)") +
  ggtitle("# of Requests by Department")

#Heatmap of Requests by Hour for each Department
###Bureau of Sanitation
LAdataOwnerHourlySan <- LAData %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlySan$Created_Day <- factor(LAdataOwnerHourlySan$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlySan, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Sanitation Calls Day v.s. Hour")

###Office of Community Beautification
LAdataOwnerHourlyBeau <- LAdata %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlyBeau$Created_Day <- factor(LAdataOwnerHourlyBeau$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlyBeau, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Beautification Calls Day v.s. Hour") +
  theme_classic()

###Bureau of Street Lighting
LAdataOwnerHourlyLight <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Lightning")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlyLight$Created_Day <- factor(LAdataOwnerHourlyLight$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlyLight, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Lighting Calls Day v.s. Hour") +
  theme_classic()

### Information Technology Agency
LAdataOwnerHourlyIT <- LAdata %>%
  filter(Owner_Full %in% c("Information Technology Agency")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlyIT$Created_Day <- factor(LAdataOwnerHourlyIT$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlyIT, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of IT Calls Day v.s. Hour") +
  theme_classic()

### Bureau of Street Service
LAdataOwnerHourlyStreetSer <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Service")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlyStreetSer$Created_Day <- factor(LAdataOwnerHourlyStreetSer$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlyStreetSer, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Street Service Calls Day v.s. Hour") +
  theme_classic()

### Los Angeles Department of Water and Power
LAdataOwnerHourlyWatPow <- LAdata %>%
  filter(Owner_Full %in% c("Los Angeles Department of Water and Power")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerHourlyWatPow$Created_Day <- factor(LAdataOwnerHourlyWatPow$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerHourlyWatPow, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Water and Power Calls Day v.s. Hour") +
  theme_classic()
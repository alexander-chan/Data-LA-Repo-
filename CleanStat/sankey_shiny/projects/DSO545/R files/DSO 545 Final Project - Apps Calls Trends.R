library(dplyr)
library(ggplot2)
LAdata <- read.csv("Data_merged.csv")
###Number of Calls and Apps
LAdataCallsAppsSelfService <- LAdata %>%
  filter(RequestSource %in% c("Call", "Mobile App","Self Service")) %>%
  group_by(Owner_Full,RequestSource) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

colors <- c("#2AB69D","#E65848","#FDC536","#FCF2D7")
ggplot(LAdataCallsAppsSelfService, aes(x=reorder(Owner_Full, CountforAxis), y= CountforAxis, fill = RequestSource)) +
  geom_bar(stat = "identity", position="fill") +
  theme(axis.text = element_text(angle = 60, hjust = 1)) +
  theme(panel.background = element_blank()) +
  xlab("Owner of Request") +
  ylab("Percentage of Requests") +
  ggtitle("Percentage of Requests by Request Source by Owner")
??panel.background

####Heatmap of Requests by Hour for each Department
#Bureau of Sanitation - Calls
LAdataOwnerCallSan <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  filter(RequestSource %in% c("Call")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerCallSan$Created_Day <- factor(LAdataOwnerCallSan$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerCallSan, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Sanitation Calls Day v.s. Hour") +
  theme_classic()

#Bureau of Sanitation - Mobile App
LAdataOwnerAppSan <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  filter(RequestSource %in% c("Mobile App")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerAppSan$Created_Day <- factor(LAdataOwnerAppSan$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerAppSan, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Sanitation Calls Day v.s. Hour")

#Office of Community Beautification - Calls
LAdataOwnerCallBeau <- LAdata %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  filter(RequestSource %in% c("Call")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerCallBeau$Created_Day <- factor(LAdataOwnerCallBeau$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerCallBeau, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Beautification Calls Day v.s. Hour") +
  theme_classic()

#Office of Community Beautification - Mobile App
LAdataOwnerAppBeau <- LAdata %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  filter(RequestSource %in% c("Mobile App")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerAppBeau$Created_Day <- factor(LAdataOwnerAppBeau$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerAppBeau, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Beautification Mobile App Day v.s. Hour") +
  theme_classic()

#Bureau of Street Lighting - Calls
LAdataOwnerCallLight <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Lightning")) %>%
  filter(RequestSource %in% c("Call")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerCallLight$Created_Day <- factor(LAdataOwnerCallLight$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerCallLight, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Lighting Calls Day v.s. Hour") +
  theme_classic()

#Bureau of Street Lighting - Mobile App
LAdataOwnerAppLight <- LAdata %>%
  filter(Owner_Full %in% c("Bureau of Street Lightning")) %>%
  filter(RequestSource %in% c("Mobile App")) %>%
  group_by(Owner_Full, Created_Day, Created_Hour) %>%
  summarise(Count = n())

LAdataOwnerAppLight$Created_Day <- factor(LAdataOwnerAppLight$Created_Day, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

ggplot(LAdataOwnerAppLight, aes(x = Created_Day, y= as.factor(Created_Hour), fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e5f5e0",
                      high = "#31a354") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Heatmap of Lighting App Requests Day v.s. Hour") +
  theme_classic()

# Information Technology Agency - Calls
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
load("service.rda")

library(stringr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

### Call vs. Mobile App
service$CreatedDate = mdy_hms(service$CreatedDate)
service$UpdatedDate = mdy_hms(service$UpdatedDate)

service = service %>%
  mutate(CreatedYear = year(CreatedDate))%>%
  mutate(CreatedMonth = month(CreatedDate, label = T)) %>%
  mutate(CreatedWeekday = wday(CreatedDate, label = T)) %>%
  mutate(CreatedHour = hour(CreatedDate))

# Call vs. Mobile App by Request Type - bar chart
type = service %>%
  group_by(RequestType, RequestSource) %>%
  summarise(count = n())

CALLvsApp = type %>%
  filter(RequestSource == "Call" |RequestSource == "Mobile App")

write.csv(CALLvsApp,"CALLvsApp.csv")

ggplot(CALLvsApp, aes(x = reorder(RequestType, count), y = count, fill =RequestSource)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("") +
  ggtitle("Request Type by Call vs. Mobile App") + 
  xlab("") + theme_light() +
  guides(fill = guide_legend(title = NULL)) + coord_flip() +
  scale_fill_manual(values = c("pink","#9ecae1"))

## Call vs. Mobile App Trend - line chart
service$month_year = paste(service$CreatedMonth, service$CreatedYear,
                           sep = "-")


call_trend = service %>%
  group_by(CreatedYear, CreatedMonth, month_year, RequestSource) %>%
  filter(RequestSource == "Call") %>%
  summarise(count = n())

call_trend = call_trend[-16,]

call_trend$month_year = factor(call_trend$month_year, 
                               levels = c("Aug-2015", "Sep-2015", "Oct-2015", "Nov-2015",
                                          "Dec-2015", "Jan-2016", "Feb-2016", "Mar-2016",
                                          "Apr-2016", "May-2016", "Jun-2016", "Jul-2016",
                                          "Aug-2016", "Sep-2016", "Oct-2016"))

app_trend = service %>%
  group_by(CreatedYear, CreatedMonth, month_year, RequestSource) %>%
  filter(RequestSource == "Mobile App") %>%
  summarise(count = n()) 

app_trend = app_trend[-16,]

write.csv(app_trend, "app_trend.csv")
write.csv(call_trend, "call_trend.csv")

app_trend$month_year = factor(app_trend$month_year, 
                              levels = c("Aug-2015", "Sep-2015", "Oct-2015", "Nov-2015",
                                         "Dec-2015", "Jan-2016", "Feb-2016", "Mar-2016",
                                         "Apr-2016", "May-2016", "Jun-2016", "Jul-2016",
                                         "Aug-2016", "Sep-2016", "Oct-2016"))


ggplot(app_trend, aes(month_year, count, group = 1)) + geom_line(color = "#f768a1") + 
  geom_line(data = call_trend, aes(month_year, count, group = 1), color = "#41b6c4") +
  xlab("Month-Year") + ylab("") + ggtitle("Call vs. App Trend") + theme_light() +
  ylab("Request Volume") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Call Heatmap by weekday vs. time
call_heatmap = service %>%
  group_by(CreatedWeekday, CreatedHour, RequestSource) %>%
  summarise(count = n()) %>%
  filter(RequestSource == "Call")

write.csv(call_heatmap, "call_heatmap.csv")

h1 = ggplot(call_heatmap, aes(x = CreatedWeekday, y = factor(CreatedHour), fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#dd1c77") + theme_classic() +
  xlab("") + ylab("Hour") + ggtitle("Call") + guides(fill = F)

### Mobile App Heatmap by weekday vs. time
app_heatmap = service %>%
  group_by(CreatedWeekday, CreatedHour, RequestSource) %>%
  summarise(count = n()) %>%
  filter(RequestSource == "Mobile App")

write.csv(app_heatmap, "app_heatmap.csv")

h2 = ggplot(app_heatmap, aes(x = CreatedWeekday, y = factor(CreatedHour), fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#41b6c4") + theme_classic() +
  xlab("") + ylab("") + ggtitle("Mobile App") + guides(fill = F)

grid.arrange(h1, h2, nrow = 1, bottom = "Weekday",
             top = textGrob("Call vs. Mobile App Heatmaps", 
                                              gp = gpar(fontsize = 20, 
                                                        fontface = "bold")))




### Processing Efficiency
service = service %>%
  mutate(ProcessTimeSec = UpdatedDate - CreatedDate) %>%
  mutate(ProcessDay = (ProcessTimeSec/3600)/24)
service$ProcessDay = round(service$ProcessDay, 2)

service %>%
  filter(ProcessDay >0)%>%
  summarise(mean(ProcessDay))

processtime = service %>%
  filter(ProcessDay > 0) %>%
  group_by(RequestType) %>%
  summarise(count = n(), avg = mean(ProcessDay))

processtime$RequestType = factor(processtime$RequestType, levels = 
                                   c("Report Water Waste", "Dead Animal Removal", "Metal/Household Appliances", "Electronic Waste","Bulky Items",
                                     "Graffiti Removal","Illegal Dumping Pickup","Single Streetlight Issue", "Other","Multiple Streetlight Issue",
                                     "Homeless Encampment", "Feedback"))

write.csv(processtime, "processtime.csv")

p1 = processtime %>%
  ggplot(aes(reorder(RequestType,avg), avg)) + 
  geom_bar(stat = "identity", fill = c(rep("#9ecae1",5), rep("pink", 7)),
           color = c(rep("NA",5), rep("red",2),rep("NA", 5))) + 
  geom_text(aes(label = avg),hjust = 1, fontface = "bold") +
  coord_flip() +
  geom_hline(yintercept = 4.83, color = "red") + xlab("Request Type") + 
  ylab("Average Processing Days") + 
  geom_text(aes(0, 4.83, label = 4.83, vjust = -1, hjust = -0.3), color = "red") +
  theme_light()

p2 = processtime %>%
  ggplot(aes(RequestType, count)) + 
  geom_bar(stat = "identity",
           fill = c(rep("#9ecae1",5), rep("pink", 7)),
           color = c(rep("NA",5), rep("red",2),rep("NA", 5))) +
  geom_text(aes(label = count),hjust = 0.5, fontface = "bold") +
  coord_flip() + xlab("") + ylab("Request Volume") + theme_light() +
  theme(axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)


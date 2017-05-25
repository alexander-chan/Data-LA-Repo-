###Processing Efficiency Analysis
library(dplyr)
library(ggplot2)
library(gridExtra)

service = read.csv("~/Documents/USC/Fall 2016/DSO 545/FinalProject/MyLA311_Service_Request_Data_2016.csv")

# merge serve and zipcode data
zipcode = read.csv("zipcode.csv")
serZip = merge(service, zipcode, by.x = "ZipCode", by.y = "Los.Angeles..CA")

# created date
serZip$CreatedDate = mdy_hms(serZip$CreatedDate)
serZip$UpdatedDate = mdy_hms(serZip$UpdatedDate)

serZip$processday = (serZip$UpdatedDate - serZip$CreatedDate)/(3600*24)
serZip$processday = round(serZip$processday,2)
serZip$processday = as.numeric(serZip$processday)

serZip = serZip %>%
  mutate(weekday = weekdays(serZip$CreatedDate, abbr = TRUE),
         month = month(serZip$CreatedDate, label = TRUE, abbr = TRUE),
         year = year(serZip$CreatedDate)) 

levels(serZip$weekday) = c( "Monday","Tuesday", "Wednesday", "Thursday",
                            "Friday","Saturday" , "Sunday")

###
library(lubridate)
service$CreatedDate = mdy_hms(service$CreatedDate)
service$UpdatedDate = mdy_hms(service$UpdatedDate)

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
  summarise(count = n(), avg = as.numeric(round(mean(ProcessDay),2)))

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



###Plot avg request + avg process time
dc1 = serZip %>%
  group_by(CD) %>%
  summarise(duration = round(as.numeric(max(UpdatedDate) - min(CreatedDate)),2),  
            avgproc = round(mean(processday),2), 
            volume = n()) %>%
  mutate(handle = round(volume/duration,2))

dc1$workload = round(dc1$avgproc*dc1$handle,2)

write.csv(dc1, "dc1.csv")

dc1 = read.csv("dc1.csv")

library(gridExtra)
library(ggplot2)

a = ggplot(dc1, aes(x = as.factor(CD), y = handle)) +
  geom_bar(stat = "identity", 
           fill = c(rep("pink",7), rep("#9ecae1",3),rep("pink",2), rep("#9ecae1",3))) +
  geom_text(aes(label = handle, vjust = 1.5)) +
  ggtitle("Service Request Received by Council District")+
  xlab("Council District") +
  ylab("Service Request Received per Day") +
  geom_hline(aes(yintercept = mean(handle)), color = "red") +
  geom_text(aes( 0, 151.36, label = 151.36, hjust = -1, vjust = -1),color = "red") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))


b1 = ggplot(dc1, aes(x = as.factor(CD), y = avgproc)) +
  geom_bar(stat = "identity", 
           fill = c(rep("pink"), rep("#9ecae1",2), rep("pink",2), rep("#9ecae1",4),rep("pink",2),rep("#9ecae1",1),rep("pink",2),rep("#9ecae1",1)))+
  geom_text(aes(label = avgproc, vjust = 1.5)) +
  ggtitle("Average Processing Days per Request by Council District")+
  xlab("Council District") +
  ylab("Average Process Days") +
  geom_hline(aes(yintercept = mean(avgproc)), color = "red") +
  geom_text(aes( 0, 4.15, label = 4.15, hjust = -3, vjust = -0.5),color = "red") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(a,b1)



###plot process days + workload

b2 = ggplot(dc1, aes(x = as.factor(CD), y = avgproc)) +
  geom_bar(stat = "identity", 
           fill = c(rep("pink"), rep("#9ecae1",2), rep("pink",2), rep("#9ecae1",4),rep("pink",2),rep("#9ecae1",1),rep("pink",2),rep("#9ecae1",1)),
           color = c("blue",rep("NA",2), rep("red",2), rep("NA",4),  "blue","red","NA",rep("blue",2),rep("NA",1) ) )+
  geom_text(aes(label = avgproc, vjust = 1.5)) +
  ggtitle("Average Processing Days per Request by Council District")+
  xlab("Council District") +
  ylab("Average Process Days") +
  geom_hline(aes(yintercept = mean(avgproc)), color = "red") +
  geom_text(aes( 0, 4.15, label = 4.15, hjust = -3, vjust = -0.5),color = "red") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))


c = ggplot(dc1, aes(x = as.factor(CD), y = workload)) +
  geom_bar(stat = "identity", 
           fill = c(rep("pink",1), rep("#9ecae1",6),rep("pink",3), rep("#9ecae1",2),rep("pink",2),rep("#9ecae1",1))) +
  geom_text(aes(label = workload, vjust = 1.5)) +
  ggtitle("Workload by Council District")+
  xlab("Council District") +
  ylab("Workload per Day") +
  geom_hline(aes(yintercept = mean(workload)), color = "red") +
  geom_text(aes( 0, 623.22, label = 623.22, hjust = -1.7, vjust = -0.5),color = "red") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(b2,c)


###request type by council district
dc3 = serZip %>%
  group_by(CD, RequestType) %>%
  summarise(volume = n()) %>%
  arrange(CD,-volume) %>%
  slice(1:5)

write.csv(dc3, "dc3.csv")

dc3 = read.csv("dc3.csv")
ggplot(dc3, aes(x = reorder(RequestType, volume), y = volume, fill = RequestType)) +
  geom_bar(stat = "identity") +
  facet_wrap(~CD, 4,4)+
  coord_flip() +
  xlab("Request Type")+
  ylab("Request Volume") +
  ggtitle("Top 5 Request Type by Council District ") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y=element_blank()) 

###request source by council district

dc2 = serZip %>%
  group_by(CD, RequestSource) %>%
  summarise(volume = n()) %>%
  arrange(CD,-volume) %>%
  slice(1:5)

write.csv(dc2, "dc2.csv")


dc2 = read.csv( "dc2.csv")
ggplot(dc2, aes(x = reorder(RequestSource, volume), y = volume, fill = RequestSource)) +
  geom_bar(stat = "identity") +
  facet_wrap(~CD, 4,4)+
  coord_flip() +
  xlab("Request Volume") +
  ylab("Request Source") +
  ggtitle("Top 5 Request Source by Council District ") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y=element_blank())

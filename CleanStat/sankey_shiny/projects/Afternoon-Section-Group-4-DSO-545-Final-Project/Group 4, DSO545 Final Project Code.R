
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggmap)
library(forecast)

data= read.csv("MyLA311_Service_Request_Data_2016.csv", header = T)

####### Exploraion
## Top 5 Request Channels by Volume
per= data %>%
  select(RequestSource) %>%
  group_by(RequestSource) %>%
  summarise(count= n()) %>%
  mutate(percent= round(100*count/sum(count), 2)) %>%
  arrange(desc(percent))

per_top5= head(per, 5)

ggplot(per_top5,aes(x= reorder(RequestSource, desc(percent)), y= percent))+
  geom_bar(stat= "identity",fill= "darkred", width = 0.7)+
  geom_text(aes(label= percent),size=3,vjust=-0.7, hjust= 0.5)+
  ylab("Percent (%)")+
  xlab("")+
  theme(axis.title.y = element_text(size = 15, face = "bold", vjust = 0.5, hjust = 0.5))+
  ggtitle("Top 5 Request Channels by Volume")

## Top Service Types by Request Source
top5_service = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal", "Metal/Household Appliances", "Illegal Dumping Pickup", "Electronic Waste"))%>%
  group_by(RequestType, RequestSource) %>%
  summarise(count= n()/100)%>%
  group_by(RequestType)%>%
  top_n(n=3, wt=count)

top5_service$RequestType = factor(top5_service$RequestType, levels = c("Electronic Waste", "Illegal Dumping Pickup", "Metal/Household Appliances","Graffiti Removal", "Bulky Items"))
ggplot(top5_service,aes(x= RequestType, y=count))+
  geom_bar(aes(fill= RequestSource), stat = "identity")+
  coord_flip()+
  xlab("")+
  ylab("Volume of Requests  ('00s)")+
  ggtitle("Top Service Types by Request Source")+
  theme(axis.title.x = element_text(face = "bold"))+
  scale_fill_discrete(guide = guide_legend(title = "Request Source")) 

## Daily Requests of Top 5 Request Sources
data$CreatedDate=mdy_hms(data$CreatedDate)

requests_wday=data%>%
  filter(RequestSource=='Call'|RequestSource=='Driver Self Report'|
           RequestSource=='Mobile App'|
           RequestSource=='Self Service'|RequestSource=='Email')%>%
  group_by(weekday=wday(CreatedDate,label=TRUE,abb=FALSE),RequestSource)%>%
  summarise(count=n())%>%
  mutate(per=count/sum(count))

requests_wday$weekday=factor(requests_wday$weekday,levels(requests_wday$weekday)[c(2:7,1)])
requests_wday$RequestSource= factor(requests_wday$RequestSource, 
                                    levels= c("Call", "Mobile App", "Self Service","Driver Self Report", "Email"))

ggplot(requests_wday,aes(x=weekday,y=per,fill=RequestSource))+
  geom_bar(stat='identity',position = 'dodge',color='black')+
  xlab('')+
  ylab('Percentage (%)')+
  ggtitle('Daily Requests of Top 5 Sources')+
  scale_fill_discrete(guide = guide_legend(title = "Request Source")) 

####### Calls versus Mobile Requests
## 2016 Mothly Changes in Call and Mobile App Request Volume
data$year= year(data$CreatedDate)
data$month= month(data$CreatedDate, label = T)

Q1=data%>%
  select(CreatedDate, RequestSource, year, month)%>%
  filter(RequestSource=='Mobile App'|RequestSource=='Call')%>%
  filter(year=="2016" & month!= "Dec") %>%
  group_by(month,RequestSource)%>%
  summarise(count=n())

ggplot(Q1,aes(x=factor(month),y=count,group= RequestSource, 
              color= RequestSource))+
  geom_line(size=2)+
  geom_point(size = 2, color= "Black")+
  xlab('')+
  ylab("Request  Volume")+
  ggtitle("2016 Monthly Changes in Call and Mobile App Request Volume")

## Request Volume by Service Type
referral_type = data %>%
  select(RequestType, RequestSource) %>%
  filter(RequestSource %in% c("Call", "Mobile App"))%>%
  group_by(RequestSource, RequestType) %>%
  summarise(count= n()/100) %>%
  group_by(RequestSource) %>%
  top_n(n=5, wt=count)

ggplot(referral_type, aes(x= RequestSource,y= count, fill= RequestType))+
  geom_bar(position = position_dodge(0.8),width = 0.7, stat = "identity")+
  geom_text(aes(label= count),size=3,vjust=-0.5, hjust= 0.5,position = position_dodge(0.8))+
  xlab("")+
  ylab("Request  Volume  ('00s)")+
  ggtitle("Request Volume by Service Type")+
  theme(axis.title.y = element_text(face = "bold"))+
  scale_fill_discrete(guide = guide_legend(title = "Request Type")) 

## Geographical Distributions of Request Volume for Calls and Mobile
LAMap <- qmap("Los Angeles", zoom = 10, color = "bw", legend = "topright")

alltypes.call=data%>%
  filter(RequestSource=='Call')
LAMap + stat_bin2d(data = alltypes.call, 
                   aes(x = Longitude, y = Latitude),alpha=0.8)+
  scale_fill_gradient2(low = 'white', high = 'red')+ggtitle('Call')  

alltypes.mobile=data%>%
  filter(RequestSource=='Mobile App')
LAMap + stat_bin2d(data = alltypes.mobile, 
                   aes(x = Longitude, y = Latitude),alpha=0.8)+
  scale_fill_gradient2(low = 'white', high = 'red')+ggtitle('Mobile APP')  

####### Digital Deep Dive
## Relationship of Waiting Time and Select Digital Channels
data2=read.csv('311_Data.csv',header = T)

line=data2%>%filter(!is.na(WaitTimesseconds))%>%
  mutate(WaitTimesmin=WaitTimesseconds/60)

str(data2)
library(forecast)
ggplot(line,aes(x=RequestsbyAppWeb,y=WaitTimesmin))+geom_point()+
  stat_smooth(method = 'lm',color= "red",se=FALSE)+
  xlab('App & Web Usage (%)')+
  ylab('Waiting Time (min)')+
  ggtitle('Relationship of Waiting Time to Digital Channels')

regression=lm(WaitTimesseconds~RequestsbyAppWeb,data = line)
summary(regression)

## Monthly Request Volume from Web Forms and Twitter
Q2=data%>%
  filter(RequestSource=='Web Form'|RequestSource=='Twitter')%>%
  filter(year=="2016" & month!= "Dec") %>%
  group_by(year, month, RequestSource) %>%
  summarise(count=n())

Q2$time <-  paste(Q2$month, Q2$year, sep=" '")

Q2$time= factor(Q2$time, levels=c("Jan '2016", "Feb '2016","Mar '2016","Apr '2016","May '2016","Jun '2016","Jul '2016","Aug '2016", "Sep '2016","Oct '2016","Nov '2016"))

ggplot(Q2,aes(x=time,y=count,group= RequestSource, 
              color= RequestSource))+
  geom_line(size=1)+
  geom_point(size = 2, color= "Black")+
  xlab('')+
  ylab("Request  Volume")+
  ggtitle("Monthly Request Volume from Web Forms & Twitter")

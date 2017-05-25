request<-read.csv("request_update.csv")
zip<-read.csv("zipcode.csv")

library(ggplot2)
library(dplyr)

### Seasonality
unique(request$RequestType)

#summarise by time
request$CreatedDate=mdy_hms(request$CreatedDate)


day=wday(request$CreatedDate,abbr=T,label = T)
hour=hour(request$CreatedDate) 
request=cbind(request,day,hour)
month=month(request$CreatedDate,abbr=T,label=T)
request=cbind(request,month)
# day and hour
dayhour=request%>%
  group_by(day,hour)%>%
  summarise(count=n())
ggplot(dayhour,aes(x=day,y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")
write.csv(dayhour,"dayhour.csv")

write.csv(apphour,"apphour.csv")
write.csv(hourst,"hourst.csv")
# request source and hour
apphour=request%>%
  group_by(hour,RequestSource)%>%
  summarise(count=n())
ggplot(apphour,aes(x=hour,y=RequestSource,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  theme_classic()+
  ggtitle("Request Source By Hour of the Day")


#month and hour
monthhr=request%>%
  group_by(month,hour)%>%
  summarise(count=n())
ggplot(monthhr,aes(x=factor(month),y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")


#month and request type
monthst=request%>%
  group_by(month,RequestType)%>%
  summarise(count=n())

ggplot(monthst,aes(x=factor(month),y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  ggtitle("Request Type By Month")+
  theme_classic()+
  xlab("Month")

levels(monthst$month)=c("Jan","Feb","Mar","Apr","May","Jun",
                        "Jul","Aug","Sep","Oct","Nov","Dec")

levels(monthst$month)
#request and hour
hourst=request%>%
  group_by(hour,RequestType)%>%
  summarise(count=n())
ggplot(hourst,aes(x=RequestType,y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")

#request and day
wdayst=request%>%
  group_by(day,RequestType)%>%
  summarise(count=n())

ggplot(wdayst,aes(x=day,y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  theme_classic()+
  ggtitle("Request Type By Day of the Week")

write.csv(wdayst,"weekdayst.csv")
levels(wdayst$day)=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")


###Demographics

### Income
incomert = SerZip %>%
  group_by(RequestType, total.income) %>%
  summarize(count = n())

ggplot(incomert,aes(x=total.income,y=count,group=RequestType))+
    geom_point(aes(color=RequestType),alpha=0.7)+
    xlab("Median Income ")+
    ylab("Request Volume")+
    theme_classic()

### Top Request Types Distribution by most representative race and income
zipcode = read.csv("zipcode.csv")

demo = merge(service, zipcode,
             by.x = "ZipCode",
             by.y = "Los.Angeles..CA")

request_dist = demo %>%
  group_by(RequestType, total.income, race) %>%
  summarise(count = n())

write.csv(request_dist, "request_dist.csv")

service %>%
  group_by(RequestType) %>%
  summarise(count = n()) %>%
  arrange(-count)

request_dist %>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal", "Metal/Household Appliances",
                            "Illegal Dumping Pickup", "Electronic Waste",
                            "Dead Animal Removal")) %>%
  ggplot(aes(x = total.income, y = count, color = race)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~RequestType) + xlab("Median Income") + ylab("Request Volume") +
  ggtitle("Top 6 Request Types Distribution by Most Representative Race") + theme_light() +
  theme(legend.title=element_blank()) +
  scale_color_manual(values = c("#f768a1", "#feb24c", "#41b6c4", "#8856a7"))




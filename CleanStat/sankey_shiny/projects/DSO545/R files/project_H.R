library(ggplot2)
library(ggmap)
library(dplyr)
library(stringr)
library(lubridate)
merged_data = read.csv("Data_merged.csv")
merged_data$Date=str_extract_all(merged_data$CreatedDate,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}")
merged_data$Date = ymd(merged_data$Date)

merged_data = census
Data_1Y = merged_data%>%
  filter(Date>"2015-11-01" & Date< "2016-10-31")
merged_data = Data_1Y
str(merged_data)
(merged_data$Date[1])

ggplot(merged_data,aes(x = Longitude, y = Latitude))+
  geom_point()

LACC = qmap("Los Angeles Central City", color = "bw", zoom=15)
##BUlky and Grffiti Removal item
data1 = merged_data%>%
  filter(RequestType %in% c("Bulky Items"))%>%
  filter(RequestSource%in% c("Call","Mobile App","Self Service","Driver Self Report"))

table = merged_data%>%
  group_by(RequestType)%>%
  summarise(count = n())%>%droplevels()

table$RequestType=factor(table$RequestType, levels = levels(table$RequestType)[c(1,5,2,3,4,6:12)])

ggplot(data = table,aes(x = "",y = count,fill = RequestType))+
  geom_bar(width = 1, stat = "identity",color = "darkgrey")+
  coord_polar("y", start=0)+
  xlab("")+
  ylab("")+
  ggtitle("Distribution of RequestType")+
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black")
    #legend.title = element_text("none")
  ) +
  scale_fill_manual(values = c("#FDC536","#E65848","#2AB69D","#2AB69D","#2AB69D" ,"#2AB69D","#2AB69D","#2AB69D","#2AB69D","#2AB69D","#2AB69D","#2AB69D"))

kt = merged_data %>%
  mutate(SD= ifelse(RequestType=="Driver Self Report","Driver","Self"))

qmap("Los Angeles Korean", color = "bw", zoom=15)
stat_bin_2d()


###hollywood Bulky Items
Hollywood = c("90046","90069","90068","90028","90038")
bulky_h = data1 %>%
  filter(RequestType == "Bulky Items") %>%
  filter(ZipCode %in% Hollywood)
#levels(bulky_lacc$Created_Day) = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
ggplot(bulky_h,aes(x = as.factor(Created_Day)))+
  geom_bar(fill = "#2AB69D")+
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")

  ) +
  xlab("")+
  ylab("")+
  ggtitle("Distribution of Bulky Items in Hollywood Area by Day of Week")
qmap("Los Angeles Hollywood", color = "bw", zoom=13)+
  geom_point(data = bulky_h,aes(x = Longitude,y = Latitude),color = "#2AB69D")
ggplot(bulky_lacc,aes(x=))
#graffiti removal
data3 <- merged_data %>%
  filter(RequestType %in% c("Bulky Items","Graffiti Removal","Homeless Encampment","Illegal Dumping Pickup"))
data3$Median.Household.Income

ggplot(data3, aes(x = Median.Household.Income ))+
  geom_histogram(fill = "#2AB69D")+
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")

  ) +
  facet_wrap(~RequestType)+
  scale_y_continuous(limits = c(0,5000))+
  xlab("Median Household Income")+
  ylab("")+
  ggtitle("Relationship between Median Household Income and RequestType ")

##########################
BOS = merged_data%>%
  filter(Owner == "BOS")%>%
  group_by(Created_Day, Created_Hour)%>%
  summarise(count = n())%>%droplevels()


BOS$Created_Day=factor(BOS$Created_Day,levels = levels(BOS$Created_Day)[c(4,2,6,7,5,1,3)])
levels(BOS$Created_Day)
ggplot(data = BOS,aes(Created_Day,factor(Created_Hour),fill=count)) +
  geom_tile() +
  scale_fill_gradient(low="#FDC536", high ="#E65848" ) +
  xlab("Day of Week") +
  ylab("Hour") +
  ggtitle("Heat Map of Requests for Bureau of Sanitation by Hour and Day_of_Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black"),
    legend.position = "none"
  )

Data7 = Data_1Y%>%
  filter(RequestSource%in% c("Call","Mobile App","Self Service","Driver Self Report"))%>%
  filter(Owner_Full=="Office of Community Beautification")%>%
  filter(Created_Day == "Wed")%>%
  mutate(SD= ifelse(RequestSource=="Driver Self Report","Driver","Self"))%>%
  group_by(SD,Created_Day,Longitude,Latitude )%>%
  summarise(count = n())




qmap("Los Angeles", zoom=12, color="bw") +
  stat_density2d(data=Data7,aes(Longitude,Latitude,fill=SD),
                 geom ="polygon",alpha=0.2) +
  scale_fill_manual(values = c("#FDC536","#E65848"))+
  theme(
    legend.position="none"
  )








BOS_A = merged_data %>%
  filter(Owner == "BOS")%>%
  group_by(AssignTo)%>%
  summarise(count = n())

OCB = merged_data%>%
  filter(Owner == "OCB")%>%
  group_by(Created_Day, Created_Hour)%>%
  summarise(count = n())%>%droplevels()


OCB$Created_Day=factor(OCB$Created_Day,levels = levels(OCB$Created_Day)[c(4,2,6,7,5,1,3)])

ggplot(data = OCB,aes(Created_Day,factor(Created_Hour),fill=count)) +
  geom_tile() +
  scale_fill_gradient(low="#FDC536", high ="#E65848") +
  xlab("Day of Week") +
  ylab("Hour") +
  ggtitle("Heat Map of Requests for Office of City Beautification by Day_of_Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black"),
    legend.position = "none"
  )



graffiti = merged_data %>%
  filter(RequestType %in% c("Graffiti Removal"))

LA+
  stat_density_2d(data = merged_data, aes(Longitude,Latitude,fill=..level..,alpha=..level..),geom="polygon") +
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729")

data3$Percent_of_Occupied_Housing_Units
ggplot(data3, aes(x = as.factor(Percent_of_Occupied_Housing_Units )))+
  geom_histogram(color = "#2AB69D")+
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")

  ) +
  facet_wrap(~RequestType)+
  scale_y_continuous(limits = c(0,5000))+
  xlab("Median Household Income")+
  ylab("")+
  ggtitle("Relationship between Median Household Income and RequestType ")



Koreatown = c("90005")
####service type:
####Bulky Item; Dead Animal Removal; Electronic Waste; Graffiti Removal; Homeless Encampment
##Illegal Dumping Pickup Metal/Household Appliances; Multiple Streetlight Issue; Single Streetlight Issue; Report Water Waste

summary(merged_data$RequestType)
#handled by Bureau of Sanitation
#1. Bulky Item
#The City of Los Angeles encourages you to recycle your bulky items by donating them to charitable organizations
#or thrift stores. However, if you are unable to do so, the Department of Public Works Bureau of Sanitation (BOS)
#will pick-up large or bulky household items, such as mattresses, couches, and other furniture

#2. Electronic Waste , can be donated
#3. Dead Animal Removal

##reduce maintainence fee
#1. volumn of Bulky Item
bulky <- merged_data %>%
  filter(RequestType == "Bulky Items")



LA+
  stat_density2d(data=bulky,aes(Longitude,Latitude,fill=..level..,alpha=..level..),geom="polygon") +
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729") +
  theme(legend.position = "none")

LA +
  geom_point(data = bulky, aes(x = Longitude, y = Latitude), color = "dark blue", alpha = 0.01)




###Response Time for Bureau of Sanitation
Data_merged=merged_data%>%
  mutate(SRDate=str_sub(ServiceDate,start=1,end=10))%>%
  mutate(UPDate=str_sub(UpdatedDate,start=1,end=10))%>%
  mutate(CRDate=str_sub(CreatedDate,start=1,end=10))


Data_merged$SRDate=mdy(Data_merged$SRDate)
Data_merged$UPDate=mdy(Data_merged$UPDate)


Data_merged$CRDate=str_extract_all(Data_merged$CreatedDate,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}")
Data_merged$CRDate = ymd(Data_merged$CRDate)
pp = Data_merged%>%
  mutate(Duration=UPDate-CRDate)







list =levels(merged_data$RequestType)[c(1,5,8,7)]
d = pp%>%
  filter(Owner %in% c("BOS","OCB"),RequestType %in% list) %>%
  filter(RequestSource%in% c("Call","Mobile App","Self Service")) %>%
  group_by(Owner, RequestType,RequestSource)%>%
  summarise(mean = mean(Duration))
d$mean=as.numeric(d$mean)

d$mean = round(d$mean,1)
ggplot(data =d, aes(x = RequestType, y = mean,fill=RequestType))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#4D95DE","#2AB69D","#FFAD29","#E65848"))+
  facet_grid(~RequestSource)+
  ylab("Response Time/Day")+
  xlab("")+
  theme_classic()+
  theme(
    axis.title=element_text(face = "bold",colour = "black"),
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black")
  )+
  ggtitle("Distribution of Response Time by Request Type")
#####Bulky heatmap
pp$Created_Day=factor(pp$Created_Day,levels = levels(pp$Created_Day)[c(4,2,6,7,5,1,3)])
Bdata = pp%>%
  filter(pp$RequestType == "Bulky Items")%>%
  group_by(Created_Day,Created_Hour)%>%
  summarise(count = n())%>%droplevels()


ggplot(data = Bdata,aes(Created_Day,factor(Created_Hour),fill=count)) +
  geom_tile() +
  scale_fill_gradient(low="#FDC536", high ="#E65848" ) +
  xlab("Day of Week") +
  ylab("Hour") +
  ggtitle("Heat Map of RequestType Bulky Item by Hour and Day_of_Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black"),
    legend.position = "none"
  )
Gdata = pp%>%
  filter(pp$RequestType == "Graffiti Removal")%>%
  group_by(Created_Day,Created_Hour)%>%
  summarise(count = n())%>%droplevels()


ggplot(data = Gdata,aes(Created_Day,factor(Created_Hour),fill=count)) +
  geom_tile() +
  scale_fill_gradient(low="#FDC536", high ="#E65848" ) +
  xlab("Day of Week") +
  ylab("Hour") +
  ggtitle("Heat Map of RequestType Graffiti Removal by Hour and Day_of_Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="black"),
    legend.position = "none"
  )




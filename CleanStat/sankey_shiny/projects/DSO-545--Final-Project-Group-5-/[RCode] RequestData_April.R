library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
library(lubridate)
load("TrackingData.rda")
load("RequestData.rda")
head(RequestData)
library(ggmap)
LosAngeles = "Los Angeles City"
library(dplyr)
library(ggplot2)
RequestData$Year = as.factor(year(RequestData$CreatedDate))
RequestData$Month = month(RequestData$CreatedDate, label = T, abbr = T)
RequestData$Day = as.factor(day(RequestData$CreatedDate))
RequestData$Weekday = wday(RequestData$CreatedDate, label = T, abbr = T)
RequestData$Hour = as.factor(hour(RequestData$CreatedDate))

colnames(TrackingData)

##ActionTaken
unique(RequestData$ActionTaken)
ggplot(RequestData, aes(x = ActionTaken))+
  geom_bar() ##don't make sense

RequestData%>%
  group_by(ActionTaken)%>%
  summarise(count = n())
formal_theme = theme(plot.title = element_text(size = 15, hjust = 0.5),
                     panel.background = element_blank(),
                     panel.grid.major.y = element_line(color = "lightgrey"),
                     panel.grid.minor.y = element_line(color = c("#E6E6E6")),
                     axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
                     axis.text.y = element_text(size = 9))
##Owner
unique(RequestData$Owner) ## department who's in charge of this kind of case 
owner =RequestData%>%
  filter(Owner!="", Owner!="NA")%>%
  group_by(Owner)%>%
  summarise(Count = n())
ggplot(owner, aes(x =reorder(Owner, -Count), y = Count/1000))+
  geom_bar(stat = "identity",fill = "#326FA8")+
  xlab("Owner")+
  ylab("Request Count (Unit: K)")+
  ggtitle("Requests for Different Owner")+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))
  

##Top 3: BOS OCB BSL

##Request Type
unique(RequestData$RequestType)
requesttype = RequestData%>%
  group_by(RequestType)%>%
  summarise(count = n())
ggplot(requesttype, aes(x = reorder(RequestType,-count),
                        y = count/1000))+
  geom_bar(stat = "identity", fill = "#326FA8")+
  xlab("Request Type")+
  ylab("Request Count (Unit:K)")+
  ggtitle("Requests in Different Types")+
    theme(plot.title = element_text(size = 15, hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "lightgrey"),
          panel.grid.minor.y = element_line(color = c("#E6E6E6")),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9))
  
##top 3: Bulky items, Graffiti Removal Metal, Household Appliances
### Status
unique(RequestData$Status)
status = RequestData%>%
  group_by(Status)%>%
  summarise(count = n())
ggplot(status, aes(x = reorder(Status, -count), y = count/1000))+
  geom_bar(stat = "identity", fill =  "#326FA8")+
  xlab("Request Status")+
  ylab("Request Count (Unit:K)")+
  ggtitle("Requests in Different Status")+
  formal_theme
##closed happens most

###RequestSource
unique(RequestData$RequestSource)
requestsource = RequestData%>%
  group_by(RequestSource)%>%
  summarise(count = n())%>%
  arrange(count)
ggplot(requestsource, aes(x =reorder(RequestSource,-count),
                          y = count/1000))+
  geom_bar(stat = "identity",fill = "#326FA8")+
  xlab("Request Source")+
  ylab("Request Count (Unit:K)")+
  ggtitle("Request from Different Devices")+
  formal_theme


###top 3: Call, Driver Self Report

###MobileOS
unique(RequestData$MobileOS)
RequestData%>%
  group_by(MobileOS)%>%
  summarise(count = n())
##NA is the most

###Anonymous
unique(RequestData$Anonymous)
unique(RequestData$AddressVerified)
unique(RequestData$ApproximateAddress)

##Assigned to
unique(RequestData$AssignTo) 
RequestData%>%
  group_by(AssignTo)%>%
  summarise(count = n())
##IED; SMD; SPD; UFD;LCD??
##ZipCode?????? How to deal with ZipCode?
unique(RequestData$ZipCode)
zipcode = RequestData%>%
  group_by(ZipCode)%>%
  summarise(count = n())%>%
  arrange(ZipCode)

#####
unique(RequestData$TBMPage) # ? #
unique(RequestData$TBMColumn) # ? # another google map
unique(RequestData$TBMRow) # ? #

#####APC
apc = RequestData%>%
  filter(APC != "")%>%
  group_by(APC)%>%
  summarise(count = n())
  apc = arrange(apc, -count)
unique(RequestData$APC) ##?## Area Planning Commissions
ggplot(apc, aes(x= reorder(APC,-count), y = count/1000))+
  geom_bar(stat = "identity", fill = "#326FA8")+
  xlab("APC (Area Planning Commission)")+
  ylab("Request Count (Unit:K)")+
  ggtitle("Requests in Different Area Planning Commissions")+
  formal_theme
##top 3: South LA APC; Central APC; South Valley APC

unique(RequestData$CD) # ? # concil District
RequestData$CD = factor(RequestData$CD)
CD = RequestData%>%
  filter(CD != "NA")%>%
  group_by(CD)%>%
  summarise(count = n())

ggplot(CD, aes(x = reorder(CD,-count), y = count))+
  geom_bar(stat = "identity", fill = "#326FA8")+
  xlab("CD (Council Districts)")+
  ylab("Request Count")+
  ggtitle("Requests in Different Council Districts")+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.y = element_text(size = 9))
##council district 9

unique(RequestData$CDMember) 
ggplot(RequestData, aes(x =CDMember))+
  geom_bar()
CD = RequestData%>%
  group_by(CDMember, CD)%>%
  summarise(count = n())

ggplot(CD, aes(x = factor(CD), y = CDMember,fill = count), alpha = 0.6)+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "brown")

?theme
######???####
unique(RequestData$NC) 
unique(RequestData$NCName)

unique(RequestData$PolicePrecinct)

##summary: ??: owner; assignto; suffix; TBM;APC; CD; NC.
ggmap
LosAngeles=qmap(LosAngeles, maptype = "road")
LosAngeles+
  geom_point(data = RequestData,aes(x = Longitude, y = Latitude,
                         color = Owner),
             size = 2,alpha = 0.6)



ggplot(RequestData, aes(x = Owner))+
  geom_bar()


type_anonymous = RequestData%>%
  group_by(RequestType, Anonymous)%>%
  summarise(count = n())
ggplot(type_anonymous, aes(x = reorder(RequestType,-count), 
                           y = count/1000,
                           fill = Anonymous))+
  geom_bar(stat = "identity", position = position_dodge())+
  formal_theme+
  xlab("Request Type")+
  ylab("Request Count (Unit:K)")+
  ggtitle("Anonymous and Non-anonymous Requests 
          in Different Types")
  
TrackingData$Hour = hour(TrackingData$Time)
TrackingData$weekday = wday(TrackingData$Date,abbr = F, label = T)
unique(TrackingData$Hour)
Tracking_Hour = filter(TrackingData, !is.na(Hour))


heatmap = Tracking_Hour%>%
group_by(Hour, weekday) %>%
  summarise(count = n())

ggplot(heatmap, aes(x = weekday, y = factor(Hour), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "rosybrown", high = "violetred4")

colnames(RequestData)
View(filter(Tracking_Hour, Hour== 23))

filter(TrackingData, Department.Abbrreviation == "OCB")
unique(TrackingData$Department.Name)
unique(TrackingData$Department.Abbreviation)


###################################################
RequestData %>%
  group_by(RequestType, Status) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(RequestType, desc(count)), y = count/1000, fill = Status)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#FFF75C","#1A6791", "#8B6DED", "#B6F5AB","#FFAE00"))+
  xlab("Request Type")+
  ylab("Request Count (Unit: K)")+
  ggtitle("Type of Requests in Different Status")+
  formal_theme

RequestData %>%
  group_by(Owner) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(Owner, desc(count)), count/1000)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


###### App vs Phone call referrals, service type question for each input channel ######

# App vs Phone call referrals
table(RequestData$RequestSource)
RequestData %>%
  group_by(RequestSource) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

RequestData %>%
  filter(RequestSource %in% c("Mobile App", "Call", "Driver Self Report", "Self Service")) %>%
  group_by(RequestType, RequestSource) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(RequestType, desc(count)), count/1000)) +
  geom_bar(stat = "identity",
           fill = "#326FA8") +
  facet_wrap(~RequestSource, ncol = 2) +
  xlab("Request Type")+
  ylab("Request Count (Unit: K)")+
  ggtitle("Type of Requests from Different Sources")+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))

  
  

# Android vs IOS
table(RequestData$MobileOS) # roughly the same number of requests


###### Overall trends in calls/apps - sums and counts ######


###### % of calls handled vs referred. How much services we deliver over time? ######
###Type of Requests from Different Sources

TrackingData %>%
  filter(!is.na(Year)) %>%
  group_by(Year, Resolution.Category) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count)*100) %>%
  ggplot(aes(x = Year, y = percent, color = Resolution.Category, group = Resolution.Category)) +
  geom_line()


###### Service Type breakdowns. Which requests are most common over time and areas. ######


###### Change volume of requests and input type (app/call) over time ######

###### Geographic + Service Type breakdown ######
## Use scatter plot/stat_2d to see which neighborhood reports which services 




###### Other Ideas ######

### Look at percentage changes over years - e.g. call resolution rate, uses of apps %

### Is there any relationship between number of requests and population?

### Look at service failure and how to improve

### Time it takes to solve (close) the request (RequestData: closeddate - createddate)
RequestData$TimeTaken = (RequestData$ClosedDate - RequestData$CreatedDate)/(24*60*60)
RequestData %>%
  filter(!is.na(TimeTaken)) %>%
  group_by(Year) %>%
  summarize(averagetimetaken = mean(TimeTaken)) %>%
  ggplot(aes(x = Year, y = averagetimetaken, group = 1)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 5))

### RequestType on map
LosAngeles = qmap("Los Angeles", zoom = 10, maptype = "roadmap")

LosAngeles +
  geom_point(data = RequestData, aes(x = Longitude, y = Latitude, color = RequestType))

LosAngeles +
  stat_bin2d(data = RequestData, aes(x = Longitude, y = Latitude, fill = RequestType), bins = 40, alpha = 0.5)


### focus on the cancelled data and closed data=patrick

### population density + requestdata= April

##deadline for all the graphs: Monday
## get the graphs done, themes;Rmarkdown; Monday.

la_pop = read.csv("california_pop_density.csv")
la_zipcode = read.csv("california_zip_code.csv")
pop_zip = merge(la_zipcode, 
                la_pop,
                by.x ="zip",
                by.y = "ZCTA",
                all.x = T)

request_pop = merge(RequestData,
                    pop_zip,
                    by.x = "ZipCode",
                    by.y = "zip",
                    all.x = T )

colnames(RequestData)
###### !!!!! #######

request_pop1 = request_pop%>%
  select(longitude, latitude, RequestType, PopulationDensity)
colnames(request_pop1) = c("longitude", "latitude","Request Type", "Population Density")
colnames(request_pop1)
LosAngeles+
  geom_point(data = request_pop, 
             aes(x = longitude, y = latitude,
                 color = RequestType,
                 size = PopulationDensity),
             alpha = 0.4)+
  ggtitle("Requests in Different Types VS. Population Density")+
  guides(color = guide_legend(title = "Request Type"),
         size = guide_legend(title = "Population Density"))
##########################
mapcount = request_pop %>%
  group_by(PopulationDensity,Longitude,Latitude,ZipCode) %>%
  summarise(count = n())
LosAngeles+
  geom_point(data = mapcount,
             aes(x = Longitude, y = Latitude,
                 color = PopulationDensity,
                 size = count),
             alpha = 0.6)+
  scale_color_gradient(low = "rosybrown", high = "violetred4")
unique(RequestData$RequestType)
unique(RequestData$Owner)
##### Population Density VS request count #####
LosAngeles+
  geom_count(data = request_pop, 
             aes(x = longitude, y = latitude,
                 color = PopulationDensity),
             alpha = 0.7)+
  scale_color_gradient(low = "rosybrown", high = "violetred4",
                       name = "Population Density")+
  ggtitle("Population Density VS. Request Count")+
  scale_size(name = "Request Count")
  
  
##### 

head(mapcount)
request_pop$ZipCode = factor(request_pop$ZipCode)

LosAngeles+
  geom_point(data = mapcount,
             aes(x = Longitude, y = Latitude,
                 color = PopulationDensity,
                 size = count),
             alpha = 0.6)+
  scale_color_gradient(low = "rosybrown", high = "violetred4")
unique(RequestData$ZipCode)
 zipcode = zipcode%>%
  arrange(-count)
 zipcodetop20 = zipcode[1:20, ]
 
LosAngeles+
  geom_count(data = RequestData,
             aes(x = Longitude, y = Latitude,
                 color =ZipCode),
             alpha = 0.6)+
  scale_color_gradient(low = "rosybrown", high = "violetred4")

LosAngeles+
  geom_count(data = RequestData,
             aes(x = Longitude, y = Latitude,
                 color =ZipCode),
             alpha = 0.6)+
  scale_color_gradient(low = "rosybrown", high = "violetred4")

ZIPCODE = read.csv("free-zipcode-database-Primary.csv")
LA_Zip = filter(ZIPCODE, City == "LOS ANGELES")



LosAngeles+
stat_bin2d(data = mapcount, 
             aes(x = Longitude,
                 y = Latitude,
                 bins= count),
             alpha = 0.8)+
  scale_fill_gradient(low = "mistyrose", high = "red4")+
  ggtitle("Heatmap of Request Amount")

"#4C803B", "#57AD40", "#74DE56", "#AFFF99", "#A3CC97", "#C1DFF5", "#8DC1EB", "#6AB6F5", "#157FD6", "#1562A1"

  geom_point(data = LA_Zip,
             aes(x = Long, y = Lat,
                 size = EstimatedPopulation),
             alpha = 0.7, color = "rosybrown")

LosAngeles+
  geom_point(data = LA_Zip,
             aes(x = Long, y = Lat,
                 size = EstimatedPopulation))
####### Zipcode & RequestType
ZipCodeCount = RequestData %>%
  filter(ZipCode != "99999", ZipCode != "") %>%
  group_by(ZipCode) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
Top.Zip.Code = ZipCodeCount$ZipCode[1:20] %>%
  droplevels()
  

RequestCount = RequestData %>%
  filter(RequestType != "") %>%
  group_by(RequestType) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
Top.Requests = RequestCount$RequestType[1:10] %>% 
  droplevels()

TopZipRequests = RequestData %>%
  filter(ZipCode %in% Top.Zip.Code, RequestType %in% Top.Requests)

TopZipRequests%>%
  group_by(ZipCode, RequestType) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(ZipCode, desc(count)), count/1000, fill = RequestType)) +
  geom_bar(stat = "identity", alpha = 0.85)+xlab("Zip Code") +
  ylab("Count (Unit: K)") +
  ggtitle("Correspondence between Top20 Zip Code Areas and Top10 Request Types") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))+
  scale_fill_manual(values = c("#C1DFF5", "#8DC1EB", "#6AB6F5", "#157FD6", "#1562A1",
                               "#4C803B", "#57AD40", "#74DE56", "#AFFF99", "#A3CC97"))

  
  
TopZipRequests %>%
  group_by(ZipCode, RequestType) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(ZipCode, desc(count)), count/1000, fill = RequestType)) +
  geom_bar(stat = "identity") +
  facet_wrap(~RequestType, ncol = 2)+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.major.x = element_line(color = c("lightgrey")),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))+
  scale_fill_discrete(guide = F)+
  xlab("Zip Code")+
  ylab("Count (Unit: K)")+
  scale_fill_manual(values = c("#C1DFF5", "#8DC1EB", "#6AB6F5", "#157FD6", "#1562A1",
                               "#4C803B", "#57AD40", "#74DE56", "#AFFF99", "#A3CC97"))
  

###### fhdfgh 3######

filter(RequestData,)

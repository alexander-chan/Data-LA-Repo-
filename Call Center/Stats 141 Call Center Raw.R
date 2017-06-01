library(lubridate)
library(zoo)
MyLA311_Service_Request_Data_2017 <- read_csv("C:/Users/Joseph/Downloads/MyLA311_Service_Request_Data_2017.csv")
ServiceRequest2017 <- MyLA311_Service_Request_Data_2017[,c(1,2,3,4,5,6,7,8,9,10,21,22,23,24,28,29,31,32,33)]
BOSService <- ServiceRequest2017[ServiceRequest2017$Owner=="BOS",]
table(BOSService$Status)
BOSServiceC <- BOSService[BOSService$Status=="Closed",]
BOSServiceC$Created <- as.POSIXct(BOSServiceC$CreatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles") 
BOSServiceC$Updated <- as.POSIXct(BOSServiceC$UpdatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles")
BOSServiceC$TimeTaken <- (as.numeric(BOSServiceC$Updated) - as.numeric(BOSServiceC$Created))/86400
summary(BOSServiceC$TimeTaken)
BOSServiceC <- BOSServiceC[!is.na(BOSServiceC$TimeTaken),]
BOSServiceC[BOSServiceC$TimeTaken < 0,]
BOSServiceC <- BOSServiceC[-BOSServiceC$TimeTaken <0,]
BOSServiceC[BOSServiceC$TimeTaken < 0,]
hours <- hour(BOSServiceC$Created)
BOSServiceC$weekReported <- week(BOSServiceC$Created)
BOSServiceC$weekSolved <- week(BOSServiceC$Updated)


#Will change to dplyr
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,mean)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,var)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,max)
tapply(BOSServiceC$TimeTaken,BOSServiceC$RequestType,min)
tapply(BOSServiceC$TimeTaken,BOSServiceC$CD,mean)

x2 <- BOSServiceC %>% 
  group_by(CD) %>% 
  summarise(mean(TimeTaken), median(TimeTaken), mean(hour(Updated)))

x2

x3 <- merge(x2, BOSServiceC, by = 'CD')

x3$is_one_day <- ifelse(x3$TimeTaken <= 1, 1, 0)
x3$is_one_week <- ifelse(x3$TimeTaken <= 7, 1, 0)
x3$is_one_month <- ifelse(x3$TimeTaken <= 30, 1, 0)

longestrunning <- function(strdate,CD,RType){
  usefuldate <- as.POSIXct(strdate,"%m/%d/%Y",tz = "America/Los_Angeles")
  BOSServiceCcurrent <- BOSServiceC[BOSServiceC$Updated > usefuldate & BOSServiceC$Created < usefuldate & BOSServiceC$CD==CD & BOSServiceC$RequestType == RType,]
  longest <- BOSServiceCcurrent[which.max(as.numeric(usefuldate) - as.numeric(BOSServiceCcurrent$Created)),]
  return(longest)
}
longestrunning("2/1/2017",5,"Bulky Items")

longestrunning("3/2/2017",1,"Illegal Dumping Pickup")

x4 <- BOSServiceC %>% group_by(CD,weekReported) %>%
  summarise(mean(TimeTaken), median(TimeTaken))
x4plus <- BOSServiceC %>% group_by(CD,weekReported, RequestType) %>%
  summarise(mean(TimeTaken), median(TimeTaken))
x5 <- BOSServiceC %>% group_by(CD,weekSolved) %>%
  summarise(mean(TimeTaken), median(TimeTaken))
x5plus <- BOSServiceC %>% group_by(weekSolved,CD,RequestType) %>%
  summarise(mean(TimeTaken), median(TimeTaken))
x6 <- BOSServiceC %>% group_by(CD,weekReported) %>% tally
x7 <- BOSServiceC %>% group_by(CD,weekSolved) %>% tally
names(x6)[2] <- "week"
names(x7)[2] <- "week"
newx7 <- full_join(x6,x7,by=c("CD","week"))
names(newx7) <- c("CD","week","nReported","nSolved")
newx7$CD <- as.factor(newx7$CD)

x4mod <- x4plus[,c(1,2,3,5)]
x4mod$CD <- as.factor(x4mod$CD)
names(x4mod)[4] <- "median.TimeTaken"
x4mod2 <- spread(x4mod,RequestType,median.TimeTaken)
x4mod2[(is.na(x4mod2))] <- 0
xmodlist <- list()
length(unique(x4mod2$weekReported))
for(i in 1: length(unique(x4mod2$weekReported))){
  xmodlist[[i]] <- x4mod2[x4mod2$weekReported== i ,]
}

xmodlist2<- list()
for(i in 1:length(unique(x4mod2$CD))){
  xmodlist2[[i]] <- x4mod2[x4mod2$CD == i ,]
}

xmodCD12 <- x4mod2[x4mod2$CD == 1 | x4mod2$CD == 2 ,]
ggplot(data = xmodCD12[xmodCD12$CD==1,],aes(x = weekReported,y = `Bulky Items`,color = "Red")) + 
  geom_point() + 
  geom_line() +
  geom_point(data = xmodCD12[xmodCD12$CD==2,],aes(x = weekReported,y = `Bulky Items`,color = "Blue"))+
  geom_line(data = xmodCD12[xmodCD12$CD==2,],aes(x = weekReported,y = `Bulky Items`,color = "Blue"))


week1 <- as.matrix(xmodlist[[1]])
week1 <- matrix(as.numeric(week1),ncol=10)
heatmap.2(week1)


xmodlist3 <- list()
length(unique(newx7$CD))
for(i in 1: length(unique(newx7$CD))){
  xmodlist3[[i]] <- newx7[newx7$CD== i ,]
}

(g1 <- ggplot(data=xmodlist3[[1]],aes(x=week,y=nReported,color = "red")) + 
  geom_point() + 
  geom_line() + 
  geom_point(aes(x=week,y=nSolved),color = "blue") + 
  geom_line(aes(x=week,y=nSolved),color = "blue"))

(g2 <- ggplot(data=xmodlist3[[1]],aes(x=week,y=nReported,fill = "red")) + 
  geom_bar(stat = "identity") + 
  geom_line(aes(x=week,y=nSolved),color = "blue",size=1.5) +
  geom_point(aes(x=week,y=nSolved),color = "blue", fill = "cyan",size = 3))

(g3 <- ggplot(data=xmodlist3[[1]],aes(x=week,y=nReported,fill = "red")) + 
  geom_area(stat = "identity") +
  geom_point(color="red")+
  geom_area(aes(x=week,y=nSolved,fill = "blue"))+
  geom_point(aes(x=week,y=nSolved,color = "blue")))

#source file for Call Center table and bar-line graph
BOS16readynew <- readRDS(file = "./city_LA_shiny/data/BOS16ready.RDS")

x4plus16 <- BOS16readynew %>% group_by(CD,weekReported, RequestType) %>%
  summarise(mean(TimeTaken), median(TimeTaken))
x4mod16 <- x4plus16[,c(1,2,3,5)]
names(x4mod16)[4] <- "median.TimeTaken"
x4mod2.16 <- spread(x4mod16,RequestType,median.TimeTaken)
x4mod2.16[(is.na(x4mod2.16))] <- 0
x4mod2.16$CD <- as.factor(x4mod2.16$CD)
xmodlist16 <- list()
for(i in 1: length(unique(x4mod2.16$weekReported))){
  xmodlist16[[i]] <- x4mod2.16[x4mod2.16$weekReported== i ,]
}

valuematavg <- function(xmodlist,Week){
weekly <- xmodlist[[Week]]
valuemat2 <- as.matrix(weekly[,3:10],ncol = 8)
modvaluemat2 <- apply(valuemat2,2,function(x){
  if(sum(x)!= 0){
    x/max(x)
  } else{
    x <- rep(0,16)
  }})
modvaluemat2 <- modvaluemat2[-16,]
modvaluemat2 <- apply(modvaluemat2,2,rev)
valuemat2rev <- apply(valuemat2,2,rev)
valuemat2rev <- valuemat2rev[-1,]
return(modvaluemat2)
}

valuemat <- function(xmodlist,Week){
  weekly <- xmodlist[[Week]]
  valuemat2 <- as.matrix(weekly[,3:10],ncol = 8)
  valuemat2rev <- apply(valuemat2,2,rev)
  valuemat2rev <- valuemat2rev[-1,]
  return(valuemat2rev)
}

x6.16 <- BOS16readynew %>% group_by(CD,weekReported,RequestType) %>% tally
x7.16 <- BOS16readynew %>% group_by(CD,weekSolved,RequestType) %>% tally
names(x6.16)[2] <- "week"
names(x7.16)[2] <- "week"
newx7.16 <- full_join(x6.16,x7.16,by=c("CD","week","RequestType"))
names(newx7.16) <- c("CD","week","RequestType","nReported","nSolved")
newx7.16 <- newx7.16[complete.cases(newx7.16$CD),]
newx7.16$CD <- as.factor(newx7.16$CD)
newx7.16$nReported[(is.na(newx7.16$nReported))]<- 0
newx7.16$nSolved[(is.na(newx7.16$nSolved))]<- 0

xmodlist3.16 <- list()
for(i in 1: length(unique(newx7.16$CD))){
  xmodlist3.16[[i]] <- newx7.16[newx7.16$CD== i ,]
}

 BOSService <- readRDS("./city_LA_shiny/data/BOS17preJune.RDS")
 #Service2017 <- read.socrata("https://data.lacity.org/A-Well-Run-City/MyLA311-Service-Request-Data-2017/d4vt-q4t5?$where=updateddate >= '2017-06-04' ")
 
 #Service2017 <- Service2017[,c(1,2,3,4,5,6,7,8,9,10,21,22,23,24,28,29,31,32,33)]
 #BOSService <- Service2017[Service2017$Owner=="BOS",]
 BOSServiceC <- BOSService[BOSService$Status=="Closed",]
 # BOSServiceC$Created <- as.POSIXct(BOSServiceC$CreatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles") 
 # BOSServiceC$Updated <- as.POSIXct(BOSServiceC$UpdatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles")
 # BOSServiceC$TimeTaken <- (as.numeric(BOSServiceC$Updated) - as.numeric(BOSServiceC$Created))/86400
 # BOSServiceC <- BOSServiceC[!is.na(BOSServiceC$TimeTaken),]
 # BOSServiceC <- BOSServiceC[-BOSServiceC$TimeTaken <0,]
 # hours <- hour(BOSServiceC$Created)
 # BOSServiceC$weekReported <- week(BOSServiceC$Created)
 # BOSServiceC$weekSolved <- week(BOSServiceC$Updated)
 # BOSServiceC$CreatedDate <- as.character(BOSServiceC$CreatedDate)
 # BOSServiceC$UpdatedDate <- as.character(BOSServiceC$UpdatedDate)
 
 #BOSServiceC <- rbind(BOSServiceC,BOS17preJune)
 BOSServiceC <- BOSServiceC[BOSServiceC$RequestType %in% c("Bulky Items","Dead Animal Removal","Electronic Waste","Feedback","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other"),]
 
 x4plus.17 <- BOSServiceC %>% group_by(CD,weekReported, RequestType) %>%
   summarise(mean(TimeTaken), median(TimeTaken))
 x4mod <- x4plus.17[,c(1,2,3,5)]
 names(x4mod)[4] <- "median.TimeTaken"
 x4mod2 <- spread(x4mod,RequestType,median.TimeTaken)
 x4mod2[(is.na(x4mod2))] <- 0
 x4mod$CD <- as.factor(x4mod$CD)
 xmodlist <- list()
 for(i in 1: length(unique(x4mod2$weekReported))){
   xmodlist[[i]] <- x4mod2[x4mod2$weekReported== i ,]
 }
 
 x6.17 <- BOSServiceC %>% group_by(CD,weekReported,RequestType) %>% tally
 x7.17 <- BOSServiceC %>% group_by(CD,weekSolved,RequestType) %>% tally
 names(x6.17)[2] <- "week"
 names(x7.17)[2] <- "week"
 newx7.17 <- full_join(x6.17,x7.17,by=c("CD","week","RequestType"))
 names(newx7.17) <- c("CD","week","RequestType","nReported","nSolved")
 newx7.17 <- newx7.17[complete.cases(newx7.17$CD),]
 newx7.17$CD <- as.factor(newx7.17$CD)
 newx7.17$nReported[(is.na(newx7.17$nReported))]<- 0
 newx7.17$nSolved[(is.na(newx7.17$nSolved))]<- 0
 #Small issue with data misattributed to 2017
 newx7.17 <- newx7.17[newx7.17$week <= week(Sys.time()),]

 
 xmodlist3.17 <- list()
 length(unique(newx7.17$CD))
 for(i in 1: length(unique(newx7.17$CD))){
   xmodlist3.17[[i]] <- newx7.17[newx7.17$CD== i ,]
 }



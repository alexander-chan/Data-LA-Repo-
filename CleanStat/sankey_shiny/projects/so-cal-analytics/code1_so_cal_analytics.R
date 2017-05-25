
#set your working directory to the folder in which the file 
#ServiceReq.csv is present

kk = read.csv("ServiceReq.csv")

library(dplyr)
library(ggplot2)
library(lubridate)

#this code pertains to the third diagram in 
#"II.Findings by Service Type" in the section "Insights and Analysis"

#bulky item problem


bulky=kk%>%
  filter(RequestType == "Bulky Items")%>%
  group_by(PolicePrecinct)%>%
  summarise(count=n())

ggplot(bulky,aes(x=PolicePrecinct,y=count))+
  geom_bar(fill="darkred",stat="identity")+
  coord_flip()


#grafitti removal problem


graf=kk%>%
  filter(RequestType == "Graffiti Removal")%>%
  group_by(PolicePrecinct)%>%
  summarise(count=n())

ggplot(graf,aes(x=PolicePrecinct,y=count))+
  geom_bar(fill="darkred",stat="identity")+
  coord_flip()





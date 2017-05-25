
monthst = read.csv("monthst.csv")

appday$daynum = ifelse(appday$day == "Mon", 1, 
                  ifelse(appday$day == "Tues", 2,
                    ifelse(appday$day == "Wed", 3,
                      ifelse(appday$day == "Thurs", 4,
                        ifelse(appday$day == "Fri", 5, 
                          ifelse(appday$day == "Sat", 6, 7))))))


monthst$monnum = ifelse(monthst$month == "Jan", 1,
                  ifelse(monthst$month == "Feb", 2,
                    ifelse(monthst$month == "Mar", 3,
                      ifelse(monthst$month == "Apr", 4,
                        ifelse(monthst$month == "May", 5,
                          ifelse(monthst$month == "Jun", 6,
                            ifelse(monthst$month == "Jul", 7,
                              ifelse(monthst$month == "Aug", 8,
                                ifelse(monthst$month == "Sep", 9,
                                  ifelse(monthst$month == "Oct", 10,
                                    ifelse(monthst$month == "Nov", 11, 12)))))))))))
monthday = monthday %>%
  group_by(month, day) %>%
  summarise(count = sum(count))

ggplot(appday,aes(x=day,y=RequestSource,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  ylab("")+
  xlab("")+
 theme_classic() 

dayhour$day = as.numeric(dayhour$day
                         )
write.csv(monthst, "monthst.csv", row.names = F)

read.csv("weekdayst.csv")

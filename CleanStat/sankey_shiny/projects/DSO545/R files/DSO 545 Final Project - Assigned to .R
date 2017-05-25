library(dplyr)
library(lubridate)
LAdata <- read.csv("Data_merged.csv")
###Changed to date
LAdata$CreatedDate <- ymd_hms(LAdata$CreatedDate)
LAdata$UpdatedDate <- mdy_hms(LAdata$UpdatedDate)

###Created processing time column
LAdata2 <- LAdata %>%
  mutate(ProcessingTime = ((UpdatedDate - CreatedDate)/86400))

###Checked processing time for request sources
LAdataProcessingTimebyRequestSource <- LAdata2 %>%
  group_by(RequestSource) %>%
  summarise(mean(ProcessingTime))

###Checked processing time for request types by request source
LAdataProcessingTimebyRequestType <- LAdata2 %>%
  filter(RequestSource %in% c("Call","Mobile App", "Self Service")) %>%
  group_by(Owner_Full,RequestSource, RequestType) %>%
  summarise(AvgTime = mean(ProcessingTime)) %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  filter(RequestType %in% c("Bulky Items", "Metal/Household Appliance", "Illegal Dumping Pickup", "Electronic Waste","Dead Animal Removal"))

###Graphed processing time for request types by request source
colors = c("#2AB69D","#E65848","#FDC536","#FCF2D7","#2AB69D","#E65848","#FDC536","#FCF2D7","#2AB69D","#E65848","#FDC536","#FCF2D7")
ggplot(LAdataProcessingTimebyRequestType, aes(x=RequestType, y = AvgTime)) +
  geom_bar(stat = "identity", fill = colors) + 
  facet_grid(RequestSource~.) +
  xlab("Type of Request") +
  ylab("Average Response Time (in days)") +
  ggtitle("Response Time by Request Source for Bureau of Sanitation") +
  theme_classic()

###Filtering data for response time by Assigned To



LADataAssignedTo

LADataAssignedTo
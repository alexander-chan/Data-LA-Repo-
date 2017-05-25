library(stringr)
library(lubridate)
library(dplyr)
LAdata$Date=str_extract_all(LAdata$CreatedDate,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}")
LAdata$Date = ymd(LAdata$Date)
Data_1Y <- Data_1Y %>%
  mutate(ProcessingTime = ((UpdatedDate - CreatedDate)/86400))

### Filter data by date
Data_1Y = LAdata%>%
  filter(Date>"2015-11-01" & Date< "2016-10-31")

### Filter data by Bureau of Sanitation and Request Type
Data_1YBOS <- Data_1Y %>%
  filter(Owner %in% c("BOS")) %>%
  group_by(Owner, AssignTo, RequestType) %>%
  summarise(Count = n())

### Graph of Assigned To by Bureau of Sanitation
ggplot(Data_1YBOS, aes(x= reorder(AssignTo, Count), y = Count, fill = RequestType)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey"))

### Processing Time of Assigned to for Bureau of Sanitation
Data_1YProcessTimeBOSAssign <- Data_1Y %>%
  filter(Owner_Full %in% c("Bureau of Sanitation")) %>%
  filter(AssignTo != "") %>%
  filter(RequestType %in% c("Bulky Items", "Metal/Household Appliances","Illegal Dumping Pickup","Electronic Waste")) %>%
  group_by(AssignTo, RequestType) %>%
  summarise(MeanProcessingTime = mean(ProcessingTime))

### Graph of Processing Time of Assigned To for Bureau of Sanitation
ggplot(Data_1YProcessTimeBOSAssign, aes(x= reorder(AssignTo, MeanProcessingTime), y = MeanProcessingTime)) +
  geom_bar(stat = "identity") +
  facet_grid(RequestType~.)

### Filter data by Office of Community Beautification and Request Type
Data_1YOCB <- Data_1Y %>%
  filter(Owner %in% c("OCB")) %>%
  filter(RequestType %in% c("Graffiti Removal")) %>%
  group_by(Owner, AssignTo, RequestType) %>%
  summarise(Count = n())

### Graph of Assigned To by Office of Community Beautificaiton
ggplot(Data_1YOCB, aes(x= reorder(AssignTo, Count), y = Count, fill = RequestType)) +
  geom_bar(stat = "identity")

### Filter Data for Bulky Item Requests
Data_1YBOSBulkReq <- Data_1Y %>%
  filter(Owner %in% c("BOS")) %>%
  filter(APC != "") %>%
  filter(RequestType %in% c("Bulky Items")) %>%
  group_by(Owner, APC, RequestType, RequestSource) %>%
  summarise(Count = n())

### Graph ofBulky Item Requests by APC
ggplot(Data_1YBOSBulkReq, aes(x= reorder(APC, Count), y = Count)) +
  geom_bar(stat = "identity")

### Processing Time of Assigned to for Office of Community Beautification Graffiti
Data_1YProcessTimeOCBAssign <- Data_1Y %>%
  filter(Owner_Full %in% c("Office of Community Beautification")) %>%
  filter(AssignTo != "") %>%
  filter(RequestType %in% c("Graffiti Removal")) %>%
  group_by(AssignTo, RequestType) %>%
  summarise(MeanProcessingTime = mean(ProcessingTime))

### Graph of Processing Time of Assigned To for Office of Community Beautification Graffiti
ggplot(Data_1YProcessTimeOCBAssign, aes(x= reorder(AssignTo, MeanProcessingTime), y = MeanProcessingTime)) +
  geom_bar(stat = "identity")

### Filter by Requests by CD for Bureau of Sanitation Bulky Items
Data_1YCDBOS <- Data_1Y %>%
  filter(Owner %in% c("BOS")) %>%
  filter(CD != "NA") %>%
  filter(RequestType %in% "Bulky Items") %>%
  group_by(Owner, CD, RequestType) %>%
  summarise(Count = n())

### Graph 
ggplot(Data_1YCDBOS, aes(x= reorder(CD, Count), y = Count)) +
  geom_bar(stat = "identity")

### Filter by Requests by CD for Office of Community Beautification Graffiti Removal
Data_1YCDOCB <- Data_1Y %>%
  filter(Owner %in% c("OCB")) %>%
  filter(CD != "NA") %>%
  filter(RequestType %in% c("Graffiti Removal")) %>%
  group_by(Owner, CD, RequestType) %>%
  summarise(Count = n())

### Graph of CD vs Graffiti Removal Requests
ggplot(Data_1YCDOCB, aes(x= reorder(CD, Count), y = Count)) +
  geom_bar(stat = "identity")


### Households by CD
str(Data_1Y)
Data_1Y$Num_of_Households <- as.numeric(Data_1Y$Num_of_Households)
Data_1YHouseholdCD <- Data_1Y %>%
  filter(Num_of_Households != "NA") %>%
  group_by(as.factor(CD)) %>%
  summarise(Households = count(Num_of_Households))

### Graph of Households by CD
ggplot(Data_1YHouseholdCD, aes(x= reorder(CD, Num_of_Households), y = Num_of_Households)) +
  geom_bar(stat = "identity")


### Graffiti Requests by Median Income
Data_1YGraffiti <- Data_1Y %>%
  filter(RequestType %in% c("Graffiti Removal")) %>%
  group_by(Median.Household.Income, RequestType) %>%
  summarise(Count = n())

### Graph Graffiti Requests by Median Income
ggplot(Data_1YGraffiti, aes(x= Median.Household.Income, y = Count)) +
  geom_point()

### Bulky Item Requests by Median Income
Data_1YBulkItems <- Data_1Y %>%
  filter(RequestType %in% c("Bulky Items")) %>%
  group_by(Median.Household.Income, RequestType) %>%
  summarise(Count = n())

### Graph Bulky Item Requests by Median Income
ggplot(Data_1YBulkItems, aes(x= Median.Household.Income, y = Count)) +
  geom_point()

### Bulky Item and Graffiti Removal Requests by Median Income
Data_1YBulkItemsGrafRemove <- Data_1Y %>%
  filter(RequestType %in% c("Bulky Items","Graffiti Removal")) %>%
  group_by(Median.Household.Income, RequestType) %>%
  summarise(Count = n())

### Graph Bulky Item and Graffiti Removal Requests by Median Income
ggplot(Data_1YBulkItemsGrafRemove, aes(x= Median.Household.Income, y = Count)) +
  geom_point() +
  facet_grid(RequestType~.)

### Processing Times for Bulky Item and Graffiti Removal Requests
Data_1Y <- Data_1Y %>%
  mutate(ProcessingTime = ((UpdatedDate - CreatedDate)/86400))

Data_1YBulkItemsGrafRemoveProcessTime <- Data_1Y %>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal")) %>%
  group_by(RequestType, RequestSource) %>%
  summarise(MeanProcessingTime = mean(ProcessingTime)) %>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service","Driver Self Report"))

### Graph of Processing Time
ggplot(Data_1YBulkItemsGrafRemoveProcessTime, aes(x= RequestSource, y = MeanProcessingTime)) +
  geom_bar(stat = "identity") +
  facet_grid(RequestType~.)

### Processing Time for Bulky Items and Graffiti Removal by CD
Data_1YBulkItemsGrafRemoveProcessTimeCD <- Data_1Y %>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal")) %>%
  group_by(RequestType, CD) %>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service","Driver Self Report")) %>%
  filter(CD != "NA") %>%
  summarise(MeanProcessingTime = mean(ProcessingTime))

### Graph Processing Time by CD
ggplot(Data_1YBulkItemsGrafRemoveProcessTimeCD, aes(x= as.factor(CD), y = MeanProcessingTime)) +
  geom_bar(stat = "identity") +
  facet_grid(RequestType~.)

### Mean Processing Time by Median Income for Call, Mobile App and Self Service
Data_1YProcessTimeCD <- Data_1Y %>%
  group_by(CD, Median.Household.Income, RequestType) %>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service","Driver Self Report")) %>%
  filter(RequestType %in% c("Bulky Items","Graffiti Removal")) %>%
  filter(CD != "NA") %>%
  filter(Median.Household.Income != "NA") %>%
  summarise(MeanProcessingTime = mean(ProcessingTime)) %>%
  filter(MeanProcessingTime < 15)

### Graph of Processing Time by Median Income
ggplot(Data_1YProcessTimeCD, aes(x= Median.Household.Income, y = MeanProcessingTime)) +
  geom_point() +
  facet_grid(.~RequestType)

### Processing Time for Graffiti Removal by NC
Data_1YGrafRemoveProcessTimeNC <- Data_1Y %>%
  filter(RequestType %in% c("Graffiti Removal")) %>%
  group_by(RequestType, NC) %>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service","Driver Self Report")) %>%
  filter(NC != "NA") %>%
  summarise(MeanProcessingTime = mean(ProcessingTime))

### Heatmap of Processing Time by NC
ggplot(Data_1YGrafRemoveProcessTimeNC, aes(x= NC, y = MeanProcessingTime)) +
  geom_point(stat = "identity")

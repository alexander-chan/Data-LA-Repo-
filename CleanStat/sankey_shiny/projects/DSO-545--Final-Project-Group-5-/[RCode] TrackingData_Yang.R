library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)


##### Create hour/day/month/year/weekday & Resolution.Category######
### Lubridate
TrackingData$Year = as.factor(year(TrackingData$Date))
TrackingData$Month = month(TrackingData$Date, label = T, abbr = T)
TrackingData$Day = as.factor(day(TrackingData$Date))
TrackingData$Weekday = wday(TrackingData$Date, label = T, abbr = T)
TrackingData$Hour = as.factor(hour(TrackingData$Time))
### Create Resolution.Category
TrackingData$Resolution.Category = 
  ifelse(TrackingData$Call.Resolution %in% c("Call Resolution", "Gave Caller Information", "Service Request Processed"), "Handled",
         ifelse(TrackingData$Call.Resolution %in% c("Referred To 411","Referred To County","Referred To Other Governmental","Referred To State"), "Referred",
                ifelse(TrackingData$Call.Resolution %in% c("Got Voicemail (City)", "Info Not Available (Non-City)", "Line Busy (City)"), "Service Failure",
                       ifelse(TrackingData$Call.Resolution %in% c("Transfer (City)","Transferred To 411","Warm Transfer (City)"), "Transferred",
                              ifelse(TrackingData$Call.Resolution %in% c("Escalate To Supervisor","Escalated To Office of Finance"), "Escalated", "Other")))))
TrackingData$Resolution.Category = as.factor(TrackingData$Resolution.Category)

save(list = c("TrackingData","TrackingDataTopDepartments","TrackingDataTopServices", "TrackingDataTopZipCode","TrackingDataTopZipService", "TrackingDataWZipCode", "LosAngeles", "Top.Departments", "Top.Services"), file = "FinalTrackingData.rda")

load("FinalTrackingData.rda")


##### 1. Which departments are associated with most number of calls? #####

### 1a. Count of each department (top 10 departments)

TrackingData %>%
  filter(Department.Abbreviation!= "") %>%
  group_by(Department.Abbreviation) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Department.Abbreviation, desc(count)), count/1000)) +
  geom_bar(stat = "identity", fill = c("#326FA8")) +
  xlab("Department Abbreviation") +
  ylab("Count (Unit: K)") +
  ggtitle("Number of Calls by Department (Top 10)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))
  

### 1b. Call resolution category of top 10 departments (fill = Resolution.Category)

DepartmentCount = TrackingData %>%
  filter(Department.Abbreviation != "") %>%
  group_by(Department.Abbreviation) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 
Top.Departments = DepartmentCount$Department.Abbreviation[1:10] %>% droplevels()

TrackingDataTopDepartments = TrackingData %>%
  filter(Department.Abbreviation %in% Top.Departments) %>%
  droplevels() 

TrackingDataTopDepartments$Department.Abbreviation = factor(TrackingDataTopDepartments$Department.Abbreviation, 
                                                            levels = c("LADBS", "BOS", "LAPD", "BPW", "DOT", "BSS", "BSL", "DWP", "OOF", "HCIDLA"))

TrackingDataTopDepartments %>%
  ggplot(aes(x = Department.Abbreviation, fill = Resolution.Category)) +
  geom_bar() +
  xlab("Department Abbreviation") +
  ylab("Count (Unit: K)") +
  ggtitle("Resolution Category of Calls Associated with Each Department") +
  scale_y_continuous(labels = seq(0, 1200, 300)) +
  scale_fill_manual(name = "Resolution Category",
                    values = c("#FFF75C", "#FFAE00", "#B6F5AB", "#8B6DED", "#823953", "#1A6791")) +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(0.84, 0.68))


##### 2. Which services are most frequently tracked? #####

### 2a. Count of services

TrackingData %>%
  filter(Service.Name != "") %>%
  group_by(Service.Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Service.Name, desc(count)), count/1000)) +
  geom_bar(stat = "identity", fill = c("#326FA8")) +
  scale_x_discrete(labels = c("Bulky Item Pick-up", "Permit Inspection Request", "Graffiti Removal", "Subject Specialty Group", "Property Violation Report", "Non-emergency Police Service", "Building Construction Permits", "Pool Noise Inspection", "Streetlight Outages Report", "Official Police Garage Tow")) +
  xlab("Service Name") +
  ylab("Count (Unit: K)") +
  ggtitle("Number of Calls by Service (Top 10)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))


### 2b. Call resolution category breakdown of each service

ServiceCount = TrackingData %>%
  filter(Service.Name != "") %>%
  group_by(Service.Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
Top.Services = ServiceCount$Service.Name[1:10] %>% droplevels()

TrackingDataTopServices = TrackingData %>%
  filter(Service.Name %in% Top.Services) %>%
  droplevels()

TrackingDataTopServices %>%
  group_by(Service.Name, Resolution.Category) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(Service.Name, desc(count)), count/1000, fill = Resolution.Category)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Bulky Item Pick-up", "Permit Inspection Request", "Graffiti Removal", "Subject Specialty Group", "Property Violation Report", "Non-emergency Police Service", "Building Construction Permits", "Pool Noise Inspection", "Streetlight Outages Report", "Official Police Garage Tow")) +
  scale_fill_manual(name = "Resolution Category",
                    values = c("#FFF75C", "#FFAE00", "#B6F5AB", "#8B6DED", "#823953", "#1A6791")) +
  xlab("Service Name") +
  ylab("Count (Unit: K)") +
  ggtitle("Resolution Category of Calls Associated with Each Service") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(0.84, 0.68))


### 2c. look at departments corresponded to top services

TrackingDataTopServices %>%
  filter(Department.Abbreviation %in% Top.Departments) %>%
  group_by(Service.Name, Department.Abbreviation) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Service.Name, y = count/1000, fill = Department.Abbreviation)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Bulky Item Pick-up", "Permit Inspection Request", "Graffiti Removal", "Subject Specialty Group", "Property Violation Report", "Non-emergency Police Service", "Building Construction Permits", "Pool Noise Inspection", "Streetlight Outages Report", "Official Police Garage Tow")) +
  scale_fill_manual(name = "Department Abbreviation",
                    values = c("#BD2828", "#2B3B8F", "#43B9E0", "#F09C5B", "#9ACC3D")) +
  xlab("Service Name") +
  ylab("Count (Unit: K)") +
  ggtitle("Correspondence between Top10 Services and Top10 Departments") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), 
        legend.position = c(0.84, 0.68))


##### 3. Cases of Service faiure #####
TrackingData %>%
  filter(Department.Abbreviation %in% Top.Departments, 
         Resolution.Category == "Service Failure") %>%
  group_by(Call.Resolution, Department.Abbreviation, Hour) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Department.Abbreviation, y = Hour, fill = count)) +
  geom_tile(alpha = 0.95) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count")+
  facet_wrap(~ Call.Resolution) +
  xlab("Department Abbreviation") +
  ylab("Hour of Day") +
  ggtitle("Number of Service Failure by Department and Call Resolution") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), 
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))

##### 4. Percentage of call resolution? #####

TrackingData %>%
  filter(!is.na(Year)) %>%
  group_by(Year, Resolution.Category) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count)*100) %>%
  ggplot(aes(x = Year, y = percent, color = Resolution.Category, group = Resolution.Category)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, 60)) +
  scale_color_manual(name = "Resolution Category",
                     values = c("#FFFB12", "#FFAE00", "#51FC5F", "#8B6DED", "#823953", "#1A6791")) +
  xlab("Year") +
  ylab("Percent (%)") +
  ggtitle("Percentage of Resolution Category by Year") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))


##### 5. Tracking data heatmap (Year of 2015 excluded!) #####

### 4a. Hour & Weekday

TrackingData %>%
  filter(!is.na(Hour), !is.na(Weekday), Year != "2015") %>%
  group_by(Hour, Weekday) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Weekday, y = Hour, fill = count)) +
  geom_tile(alpha = 0.95) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count of Calls") +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Number of Calls by Weekday and Hour") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

# facet year
TrackingData %>%
  filter(!is.na(Hour), !is.na(Weekday), Year != "2015") %>%
  group_by(Hour, Weekday, Year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Weekday, y = Hour, fill = count)) +
  geom_tile(alpha = 0.95) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count of Calls") +
  facet_wrap(~Year) +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Number of Calls by Weekday, Hour and Year") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))


### 4b. Weekday & Month

TrackingData%>%
  filter(!is.na(Month), !is.na(Weekday), Year != "2015") %>%
  group_by(Month, Weekday) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Weekday, y = Month, fill = count)) +
  geom_tile(alpha = 0.95) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count of Calls") +
  xlab("Day of Week") +
  ylab("Month of Year") +
  ggtitle("Number of Calls by Weekday and Month") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

TrackingData%>%
  filter(!is.na(Month), !is.na(Weekday), Year != "2015") %>%
  group_by(Month, Weekday, Year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Weekday, y = Month, fill = count)) +
  geom_tile(alpha = 0.95) +
  facet_wrap(~Year) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count of Calls") +
  xlab("Day of Week") +
  ylab("Month of Year") +
  ggtitle("Number of Calls by Weekday, Hour and Year") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))



##### 6. Zip Code + Service Type breakdown #####

### 5a. Zip Code, population density and number of records

CAZipCode = read.csv("california_zip_code.csv")
CAPopDensity = read.csv("california_pop_density.csv")

CAZipPop = merge(x = CAZipCode, y = CAPopDensity,
                 by.x = "zip", by.y = "ZCTA", 
                 all.x = T, all.y = T)
TrackingDataWZipCode = merge(x = TrackingData, y = CAZipPop, 
                             by.x = "Zip.Code", by.y = "zip",
                             all.x = T)

LosAngeles = qmap("Los Angeles", zoom = 10, maptype = "roadmap")

LosAngeles +
  geom_count(data = TrackingDataWZipCode, aes(x = longitude, y = latitude, color = PopulationDensity)) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Population Density") +
  scale_size(name = "Count of Calls") +
  ggtitle("Number of Calls vs. Population Density at Zip Code Level") +
  theme(plot.title = element_text(size = 15, hjust = 0.5))

### Do not include this one ###
PointMapData = TrackingDataWZipCode %>%
  filter(Service.Name %in% Top.Services) %>%
  group_by(Zip.Code, PopulationDensity, Service.Name, longitude, latitude)%>%
  summarise(count = n())
LosAngeles +
  geom_point(data = PointMapData, aes(x = longitude, y = latitude, color = Service.Name, size = PopulationDensity))




### 5b. Type of Service in zip code area

ZipCodeCount = TrackingData %>%
  filter(Zip.Code != "99999", Zip.Code != "") %>%
  group_by(Zip.Code) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
Top.Zip.Code = ZipCodeCount$Zip.Code[1:20]
TrackingDataTopZipCode = TrackingData %>%
  filter(Zip.Code %in% Top.Zip.Code)
TrackingDataTopZipService = TrackingData %>%
  filter(Zip.Code %in% Top.Zip.Code, Service.Name %in% Top.Services)

TrackingDataTopZipService %>%
  group_by(Zip.Code, Service.Name) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(Zip.Code, desc(count)), count/1000, fill = Service.Name)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_manual(name = "Service Name",
                    labels = c("Non-emergency Police Service", "Building Construction Permits", "Bulky Item Pick-up", "Graffiti Removal", "Official Police Garage Tow", "Permit Inspection Online Request", "Pool Noise Inspection", "Property Violation Report", "Streetlight Outages Report", "Subject Specialty Group"),
                    values = c("#4C803B", "#57AD40", "#74DE56", "#AFFF99", "#A3CC97", "#C1DFF5", "#8DC1EB", "#6AB6F5", "#157FD6", "#1562A1")) +
  xlab("Zip Code") +
  ylab("Count (Unit: K)") +
  ggtitle("Correspondence between Top20 Zip Code Areas and Top10 Services") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))

TrackingDataTopZipService %>%
  group_by(Zip.Code, Service.Name) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(Zip.Code, desc(count)), count/1000, fill = Service.Name)) +
  geom_bar(stat = "identity") +
  xlab("Zip Code") +
  ylab("Count (Unit: K)") +
  ggtitle("Correspondence between Top20 Zip Code Areas and Top10 Services") +
  scale_fill_manual(guide = F,
                    values = c("#4C803B", "#57AD40", "#74DE56", "#AFFF99", "#A3CC97", "#C1DFF5", "#8DC1EB", "#6AB6F5", "#157FD6", "#1562A1")) +
  facet_wrap(~Service.Name, ncol = 2,
             labeller = as_labeller(c(
               "877 ASK-LAPD - Non-emergency Police Service" = "Non-emergency Police Service",
               "Building Construction Permits" = "Building Construction Permits",
               "Bulky Item Pick-up" = "Bulky Item Pick-up",
               "Graffiti Removal - Community Beautification" = "Graffiti Removal",
               "Official Police Garage Tow (OPG) - LAPD" = "Official Police Garage Tow",
               "Online Request for Permit Inspection" = "Permit Inspection Online Request",
               "Pool Noise Inspection" = "Pool Noise Inspection",
               "Report a Property Violation" = "Property Violation Report",
               "Report streetlight outages" = "Streetlight Outages Report",
               "Subject Specialty Group" = "Subject Specialty Group"
             ))) +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.major.x = element_line(color = c("lightgrey")),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))



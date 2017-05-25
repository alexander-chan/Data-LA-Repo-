library(ggplot2)
library(dplyr)
library(stringr)

old = read.csv("311_Call_Center_Tracking_Data.csv")
new = read.csv("MyLA311_Service_Request_Data_2016.csv")

# Change of BSS duties
old_bss = old %>%
  filter(Call.Resolution == "Service Request Processed", 
         Department.Abbreviation == "BSS") 

old_bss = old %>%
  filter(Department.Abbreviation == "BSS") %>%
  group_by(Call.Resolution) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

new_bss = new %>%
  filter(Owner == "BSS") %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

old_bss
new_bss

old_ladbs = old %>%
  filter(Department.Abbreviation == "LADBS") %>%
  group_by(Service.Name) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

# Change of BOS duties
old_bos = old %>%
  filter(Department.Abbreviation == "BOS") %>%
  group_by(Service.Name) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_bos = new %>%
  filter(Owner == "BOS") %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

# New departments duties
new_ocb = new %>%
  filter(Owner == "OCB") %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_ocb

# Graffiti Removal duties before 2016
old_graffiti = old %>%
  filter(str_detect(Service.Name,"Graffiti")) %>%
  group_by(Service.Name) %>%
  select(Department.Abbreviation, Department.Name, Service.Name) %>%
  group_by(Department.Abbreviation, Department.Name, Service.Name) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_ladwp = new %>%
  filter(Owner == "LADWP") %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_ladwp

# Water Waste duties before 2016
old_water = old %>%
  filter(str_detect(Service.Name,"Water Waste")) %>%
  group_by(Service.Name) %>%
  select(Department.Abbreviation, Department.Name, Service.Name) %>%
  group_by(Department.Abbreviation, Department.Name, Service.Name) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_boe = new %>%
  filter(Owner == "BOE") %>%
  group_by(RequestType) %>%
  summarise(Count = n()) %>%
  arrange(-Count)

new_boe

# Change of Department 
department = old %>%
  select(Department.Abbreviation, Department.Name) %>%
  distinct(Department.Abbreviation, Department.Name) %>%
  filter(Department.Abbreviation != "")

old_dep = old %>%
  filter(Call.Resolution == "Service Request Processed") %>%
  filter(Department.Abbreviation != "") %>%
  group_by(Department.Abbreviation) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

old_depname = old %>%
  filter(Call.Resolution == "Service Request Processed") %>%
  filter(Department.Name != "") %>%
  group_by(Department.Name) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

# Departments by Request Volume before 2016 by abbr.
ggplot(old_dep, aes(x = reorder(Department.Abbreviation, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Departments by Request Volume before 2016") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Departments by Request Volume before 2016 by name
ggplot(old_depname, aes(x = reorder(Department.Name, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Top 10 Departments by Request Volume before 2016") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 14, face = "bold"),        
        axis.text.x = element_text(angle = 60, hjust = 1))

# Departments by Request Volume in 2016 by name
new_dep = new %>%
  filter(Owner != "") %>%
  group_by(Owner) %>%
  summarize(Count = n()) %>%
  arrange(-Count)

ggplot(new_dep, aes(x = reorder(Owner, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = Count)) +
  ggtitle("Departments by Request Volume in 2016") +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Change of Request Volume by Department before 2016 vs 2016
## Join the data
colnames(old_dep)[1] <- "Owner"
old1 = old_dep %>%
  filter(Owner %in% new_dep$Owner & Owner !="") %>%
  mutate(Year = "Before 2016")

unique(old1$Owner)
new1 = new_dep %>%
  filter(Owner %in% old1$Owner) %>%
  mutate(Year = "2016")

total = rbind(old1, new1) 

total$Year = as.factor(total$Year)
total$Year = factor(total$Year, levels = levels(total$Year)[c(2, 1)])

total1 = total %>%
  merge(department, by.x = "Owner", by.y = "Department.Abbreviation", all.x = T) %>%
  filter(!is.na(Department.Name))

ggplot(total1, aes(x = reorder(Department.Name, -Count), y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  ggtitle("Change of Request Volume by Department") +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1))


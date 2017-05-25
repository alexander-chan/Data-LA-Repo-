
library(RSQLite)
library(dplyr)
library(lubridate)
library(ggplot2)

driver = dbDriver("SQLite")
con = dbConnect(driver, "Database.sqlite")

dbListTables(con)

request_data = dbReadTable(con, "Request_data")
call_center = dbReadTable(con, "Call_Center")

dbGetQuery(con, sql)

str(call_center)
str(request_data)


sql = "select count(distinct(Department.Name)) as count
from Call_Center
"
dbGetQuery(con, sql)

dbGetQuery(con, sql)

str(call_center)

dept_call = call_center%>%
  group_by(Department.Name)%>%
  summarise(count = n())

service_cnt = call_center%>%
  group_by(Service.Name)%>%
  summarise(count = n())

call_center$date = mdy(call_center$ï..Date)
call_center$Time = hms(call_center$Time)

call_center$day = wday(call_center$date, abbr = F, label = T)

## HOur day level request count
day_hr_cnt = call_center%>%
  group_by(Department.Name, day, hour = hour(Time))%>%
  filter(day!= "NA")%>%
  summarise(count = n())

table(day_hr_cnt)

top6_dept = day_hr_cnt%>%
  filter(Department.Name == "Department of Building and Safety"|
           Department.Name == "PW/Bureau of Sanitation"|
           Department.Name == "Los Angeles Police Department"|
           Department.Name == "PW/Board of Public Works"|
           Department.Name == "Department of Transportation"|
           Department.Name == "PW/Bureau of Street Services")

## Heat map for Hour day level request count
ggplot(top6_dept, aes(x = day, y = factor(hour), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "darkred")+
  ggtitle("Heat Map of Call count")+
  xlab("Day of the week")+
  ylab("Hour")+
  theme(axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 30, hjust = 1))+
  facet_wrap(~Department.Name, nrow = 3)

  
## Day level request count  
day_req_cnt = call_center%>%
group_by(day)%>%
filter(day!= "NA")%>%
summarise(count = n())
## Most people prefer to raise request during weekdays

## Bar Plot for Day level request count  
ggplot(day_req_cnt, aes(x = reorder(day, -count), y = count, fill = day))+
  geom_bar(stat = "identity")+
  xlab("Day")+
  ylab("Number of Calls")+
  ggtitle("Number of calls per day")

## Top 15 Department based on request count
top15_dept = head(dept_call%>%
  arrange(-count),16)%>%
  filter(Department.Name!="")

top15_dept$Department.Name = factor(top15_dept$Department.Name)

top10_dept = head(dept_call%>%
                    arrange(-count),11)%>%
  filter(Department.Name!="")

## Plot of top 15 departments call count
ggplot(top10_dept, aes(x = reorder(Department.Name, -count), y = count, fill = Department.Name))+
  geom_bar(stat = "identity")+
  theme(axis.text.x =  element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Department Call Count")+
  xlab("Department Name")+
  ylab("Number of Calls")+
  scale_fill_discrete(breaks = top10_dept$Department.Name)

### plot of Call count for each type of call resolution
call_center%>%
  group_by(Call.Resolution)%>%
  summarise(count = n())%>%
  filter(count > 10000, Call.Resolution!= "N/A")%>%
  ggplot(aes(x = reorder(Call.Resolution, -count), y= count, fill = Call.Resolution))+
  geom_bar(stat="identity")+
  theme(axis.text.x =  element_text(angle = 30, hjust = 1))+
  xlab("Call Resolution")+
  ylab("Number of Calls")+
  ggtitle("Call Count for each Call Resolution")

## Getting count for each type of call resolution
call_resoln_cnt = call_center%>%
  group_by(Call.Resolution)%>%
  summarise(count = n())

## Total number of calls
call_resoln_cnt%>%
  summarise(sum(count))

## Service request percent:
1202376/3565447
## 33.7%

## Transfer(City)
1441977/3565447
## 40.4%
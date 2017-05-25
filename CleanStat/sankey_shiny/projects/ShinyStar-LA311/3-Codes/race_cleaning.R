zipdata = read.csv("zipdata.csv")

race = read.csv("race_raw.csv")
race = race[,c("name","B03002001","B03002002","B03002003",
               "B03002004","B03002005","B03002006","B03002007",
               "B03002008","B03002009", "B03002012")]

colnames(race) = c("Name","Total", "Non hispanic", "White" ,
                   "AfricanAmerican" ,"Native","Asian", "Islander", 
                   "Other" ,"Two+" ,"Hispanic")

#find max value by row
race$max =apply(race[,4:11], 1, max)
#find column names of the max value
race$race = colnames(race)[apply(race[,4:11],1,which.max)+3]


# adds percentage
race$perc = 100*race$max/race$Total
race$perc = round(race$perc, 2)
race = race[-(1:4),]

write.csv(race, "race.csv")


##merge
race = race[,-(2:12)]
zipcode = merge(zipdata, race, by.x = "Los.Angeles..CA", by.y = "Name")
write.csv(zipcode, "zipcode.csv")

zipcode = read.csv("zipcode.csv")
serZip = merge(service, zipcode, by.x = "ZipCode", by.y = "Los.Angeles..CA")

unique(serZip$ZipCode)

serZip = serZip %>%
  mutate(weekday = weekdays(serZip$CreatedDate, abbr = TRUE),
         month = month(serZip$CreatedDate, label = TRUE, abbr = TRUE),
         year = year(serZip$CreatedDate)) 


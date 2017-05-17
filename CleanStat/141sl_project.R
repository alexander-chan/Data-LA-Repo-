library(RSocrata)
library(readr)

#scrape website
clean <- ls.socrata("http://geohub.lacity.org/datasets?q=cleanstat")

#no longer scraping the clean datasets (?), scrapes all datasets from lacity.org/datasets


#subset data to those with "clean" in title
title <- clean$title
title <- toupper(title)
index <- grep("CLEAN", title)
clean <- clean[index,]

#add column of last 4 characters for year and quarter (e.g. 16 Q4)
clean$date <- substr(clean$title, nchar(clean$title)-4, nchar(clean$title))
#remove any observations that do not have Q# format
clean <- clean[grep("Q[1-4]", clean$date),]


#function to read in data, give character string with format yy Q# (e.g. "16 Q2")
quarter_function <- function(x) {
  quarter <- clean[grep(x, clean$date),]
  quarter <- quarter$landingPage[2] #may have more than one result but takes in the second, usually the grid data, can probably be better implemented
  quarter <- paste(quarter, ".csv", sep= "")
  quarter <- read_csv(quarter)
  return(quarter)
}


q1 <- quarter_function("16 Q1")
q2 <- quarter_function("16 Q2")
q3 <- quarter_function("16 Q3")
q4 <- quarter_function("16 Q4") #q4 will fail since we don't have access to grid data, only web map if you look at website




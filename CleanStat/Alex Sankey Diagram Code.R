install.packages("RSocrata")
install.packages("readr")
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

###

names(clean)
clean$landingPage[2]
q4 <- read.csv(clean$landingPage[2])
q4 <- read.csv(clean$landingPage[10])
clean$landingPage[2]
clean$landingPage[10]
q4 <- read.csv("http://geohub.lacity.org/datasets/674e3757160f4901a11cc56c2386929d_0.csv")

###
q2_score <- q2[,c("NameInt","Q2_CS_Score_Round")]
q3_score <- q3[,c("NameInt", "GridScrRounded")]
q2q3 <- merge(q2_score, q3_score, by = "NameInt")

names(q2q3)[2] <- "Q2Score"
names(q2q3)[3] <- "Q3Score"
q2q3 %>% head()
q2q3$change23 <- paste(q2q3$Q2Score,q2q3$Q3Score, sep = "")
q2q3
unique(q2q3$NameInt) %>% length()
###
length(q2$NameInt)
length(q3$NameInt)
length(q4$LAPD_Grid)

q2_table <- q2$Q2_CS_Score_Round %>% table()
q3_table <- q3$GridScrRounded %>% table()
q4$CS_RoundScore %>% table()


q2_table
q3_table

nodes <- data.frame(c("Q2 #1's", "Q2 #2's", "Q2 #3's","Q3 #1's", "Q3 #2's", "Q3 #3's" ))
names(nodes) <- "names"
nodes

links <- data.frame()
names(links) <- c("source", "target", "weight")

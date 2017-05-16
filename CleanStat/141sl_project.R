library(RSocrata)

#getting cleaner where, dirtier
#cs round = 1 cleanest

clean <- ls.socrata("http://geohub.lacity.org/datasets")

q2 <- clean$landingPage[1]
q2 <- paste(q2, ".csv", sep = "")
q2 <- read_csv(q2)

q4 <- clean$landingPage[2]
q4 <- paste(q4, ".csv", sep = "")
q4 <- read_csv(q4)

q3 <- clean$landingPage[3]
q3 <- paste(q3, ".csv", sep = "")
q3 <- read_csv(q3)

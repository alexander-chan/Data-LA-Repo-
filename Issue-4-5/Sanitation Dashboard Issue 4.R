library(readr)
miles_sewage_cleaned <- read_csv('C://Users//Conor//Downloads//LASAN__Miles_of_Sewer_Cleaned.csv')
miles_sewage_cleaned$`Calendar Year` <- ifelse(miles_sewage_cleaned$`Month Number` %in% c(1:6), 
                                               miles_sewage_cleaned$`Fiscal Year` - 1,
                                               miles_sewage_cleaned$`Fiscal Year`)
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
miles_sewage_cleaned$`Month Factor` <- factor(miles_sewage_cleaned$Month, levels = rev(month_names))

library(ggplot2)
ggplot(data = miles_sewage_cleaned, aes(`Fiscal Year`)) + 
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  coord_flip() + 
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Fiscal Year and Month')

ggplot(data = miles_sewage_cleaned, aes(`Calendar Year`)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  coord_flip()+
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Calendar Year and Month')

ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(Month)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = Month)) +
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Month for Calendar Years between 2014-2016') +
  scale_x_discrete(limits = month_names)

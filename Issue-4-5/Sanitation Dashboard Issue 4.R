library(readr)
miles_sewage_cleaned <- read_csv('LASAN__Miles_of_Sewer_Cleaned.csv')
miles_sewage_cleaned$`Calendar Year` <- ifelse(miles_sewage_cleaned$`Month Number` %in% c(1:6), 
                                               miles_sewage_cleaned$`Fiscal Year` - 1,
                                               miles_sewage_cleaned$`Fiscal Year`)
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
miles_sewage_cleaned$`Month Factor` <- factor(miles_sewage_cleaned$Month, levels = rev(month_names))
miles_sewage_cleaned$`Month Number Factor` <- factor(miles_sewage_cleaned$`Month Number`, levels = 12:1)



colors <- c("#1a1334",  "#26294a" , "#01545a",    
            "#017351",    "#03c383",    "#aad962",
            "#fbbf45",    "#ef6a32",    "#ed0345",   
            "#a12a5e",    "#710162",    "#110141")   

library(ggplot2)
ggplot(data = miles_sewage_cleaned, aes(`Fiscal Year`)) + 
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Number Factor`)) +
  coord_flip() + 
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Fiscal Year and Month')+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse=TRUE))

ggplot(data = miles_sewage_cleaned, aes(`Calendar Year`)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  coord_flip()+
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Calendar Year and Month') +
  scale_fill_manual(values=colors) +
  guides(fill = guide_legend(reverse=TRUE))

ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(`Month Factor`)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Month for Calendar Years between 2014-2016') +
  scale_x_discrete(limits = month_names)+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse=TRUE))

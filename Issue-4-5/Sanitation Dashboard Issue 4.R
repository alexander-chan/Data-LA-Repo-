library(readr)
#read in data
miles_sewage_cleaned <- read_csv('C:/Users/jon/Downloads/LASAN__Miles_of_Sewer_Cleaned.csv')

#creating calendar year variable
miles_sewage_cleaned$`Calendar Year` <- ifelse(miles_sewage_cleaned$`Month Number` %in% c(1:6), 
                                               miles_sewage_cleaned$`Fiscal Year` - 1,
                                               miles_sewage_cleaned$`Fiscal Year`)
#creating month names
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

#creating month factor (calendar months)
miles_sewage_cleaned$`Month Factor` <- factor(miles_sewage_cleaned$Month, levels = rev(month_names))

#creating month number factor (fiscal months)
miles_sewage_cleaned$`Month Number Factor` <- factor(miles_sewage_cleaned$`Month Number`, levels = 12:1)

#blue gradient
#colors <- c("#2C00E5","#2811DE","#2422D8","#2034D1","#1C45CB","#1856C4","#1468BE","#1079B7","#0C8AB1","#089CAA","#04ADA4","#00BF9E")


#rainbow colors
colors <- c("#1a1334",  "#26294a" , "#01545a",    
            "#017351",    "#03c383",    "#aad962",
            "#fbbf45",    "#ef6a32",    "#ed0345",   
            "#a12a5e",    "#710162",    "#110141")   

#fiscal year
library(ggplot2)
fiscal <- ggplot(data = miles_sewage_cleaned, aes(`Fiscal Year`)) + 
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Number Factor`)) +
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Fiscal Year and Month')+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse = TRUE))

#calendar year
calendar <- ggplot(data = miles_sewage_cleaned, aes(`Calendar Year`)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Calendar Year and Month') +
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse = TRUE))

#monthly
monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(`Month Factor`)) +
  geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle('Bar Chart of Miles Cleaned by Month for Calendar Years between 2014-2016') +
  scale_x_discrete(limits = month_names)+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse = TRUE))

#convert to ggploty
library(plotly)
plotly::ggplotly(fiscal)
plotly::ggplotly(calendar)
plotly::ggplotly(monthly)

library(readr)
#read in data
sewer_overflow <- read_csv('C://Users//jon//Downloads//LASAN___Sanitary_Sewer_Overflows.csv')

#creating calendar year variable
sewer_overflow <- read_csv('LASAN___Sanitary_Sewer_Overflows.csv')

sewer_overflow$`Calendar Year` <- ifelse(sewer_overflow$`Month Number` %in% c(1:6), 
                                         sewer_overflow$`Fiscal Year` - 1,
                                         sewer_overflow$`Fiscal Year`)
#creating month names
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

#creating month factor (calendar months)
sewer_overflow$`Month Factor` <- factor(sewer_overflow$Month, levels = rev(month_names))

#creating month number factor (fiscal months)
sewer_overflow$`Month Number Factor` <- factor(sewer_overflow$`Month Number`, levels = 12:1)


#rainbow colors
colors <- c("#1a1334",  "#26294a" , "#01545a",    
            "#017351",    "#03c383",    "#aad962",
            "#fbbf45",    "#ef6a32",    "#ed0345",   
            "#a12a5e",    "#710162",    "#110141")   

library(ggplot2)
fiscal <- ggplot(data = sewer_overflow, aes(`Fiscal Year`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Number Factor`)) +
  theme_bw() +
  ggtitle('Overflow Bar Chart by Fiscal Year and Month') +
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse=TRUE))

calendar <- ggplot(data = sewer_overflow, aes(`Calendar Year`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle('Overflow Bar Chat by Calendar Year and Month')+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse=TRUE))

monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% c(2015:2016),], aes(`Month Factor`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle('Overflow Bar Chart by Month for Calendar Years 2015-2016') +
  scale_x_discrete(limits = month_names) +
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse = TRUE))

#convert to ggplotly
library(plotly)
ggplotly(fiscal)
ggplotly(calendar)
ggplotly(monthly)
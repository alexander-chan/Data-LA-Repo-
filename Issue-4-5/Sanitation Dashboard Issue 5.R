library(readr)
sewer_overflow <- read_csv('C://Users//Conor//Downloads//LASAN___Sanitary_Sewer_Overflows.csv')
sewer_overflow$`Calendar Year` <- ifelse(sewer_overflow$`Month Number` %in% c(1:6), 
                                         sewer_overflow$`Fiscal Year` - 1,
                                         sewer_overflow$`Fiscal Year`)
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
sewer_overflow$`Month Factor` <- factor(sewer_overflow$Month, levels = rev(month_names))

library(ggplot2)
ggplot(data = sewer_overflow, aes(`Fiscal Year`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
  coord_flip() +
  theme_bw() +
  ggtitle('Bar Chart of Overflow by Fiscal Year and Month')

ggplot(data = sewer_overflow, aes(`Calendar Year`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
  coord_flip() +
  theme_bw() +
  ggtitle('Bar Chart of Overflow by Calendar Year and Month')

ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% c(2015:2016),], aes(`Month Number`)) +
  geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = Month)) +
  theme_bw() +
  ggtitle('Bar Chart of Overflow by Month for Calendar Years 2015-2016') +
  scale_x_discrete(limits = month_names)

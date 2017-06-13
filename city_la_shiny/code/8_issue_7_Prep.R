#catch_basin <- read.socrata("https://data.lacity.org/A-Livable-and-Sustainable-City/LASAN-Number-of-Catch-Basins-Cleaned/8a3v-f7cr")
catch_basin <- read.csv('./city/city_LA_shiny/data/LASAN___Number_of_Catch_Basins_Cleaned.csv')

catch_basin$`Calendar Year` <- ifelse(catch_basin$`FYMonth` %in% c(1:6), 
                                               catch_basin$`FY` - 1,
                                               catch_basin$`FY`)
#creating month names
month_names2 <- c('JANUARY', 'FEBRUARY', 'MARCH', 
                 'APRIL', 'MAY', 'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 
                 'OCTOBER', 'NOVEMBER', 'DECEMBER')

#creating month factor (calendar months)
catch_basin$`Month Factor` <- factor(catch_basin$Month, levels = rev(month_names2))

#creating month number factor (fiscal months)
catch_basin$`Month Number Factor` <- factor(catch_basin$FYMonth, levels = 12:1)

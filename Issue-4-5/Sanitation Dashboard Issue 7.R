catch_basin <- read.socrata("https://data.lacity.org/A-Livable-and-Sustainable-City/LASAN-Number-of-Catch-Basins-Cleaned/8a3v-f7cr")

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

colors <- c("#1a1334",  "#26294a" , "#01545a",    
            "#017351",    "#03c383",    "#aad962",
            "#fbbf45",    "#ef6a32",    "#ed0345",   
            "#a12a5e",    "#710162",    "#110141")  

monthly <- ggplot(data = catch_basin[catch_basin$`Calendar Year` == 2014,], aes(`Month Factor`)) +
  geom_bar(aes(weight = NUMBER.OF.CATCH.BASIN.CLEANED, fill = `Month Factor`)) +
  theme_bw() +
  ggtitle(paste('Bar Chart of Catch Basins Cleaned by Month for Calendar Year ', 2014)) +
  scale_x_discrete(limits = month_names)+
  scale_fill_manual(values=colors)+
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("Month")
monthly

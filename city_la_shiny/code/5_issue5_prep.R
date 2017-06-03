#################
# Issue 5 prep  #
#################

#creating calendar year variable
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
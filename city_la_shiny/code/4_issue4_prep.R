#################
# Issue 4 prep  #
#################

#creating calendar year variable
miles_sewage_cleaned$`Calendar Year` <- ifelse(miles_sewage_cleaned$`Month Number` %in% c(1:6), 
                                               miles_sewage_cleaned$`Fiscal Year` - 1,
                                               miles_sewage_cleaned$`Fiscal Year`)
#creating month names
month_names <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

#creating month factor (calendar months)
miles_sewage_cleaned$`Month Factor` <- factor(miles_sewage_cleaned$Month, levels = rev(month_names))

#blue gradient option
#colors <- c("#2C00E5","#2811DE","#2422D8","#2034D1","#1C45CB","#1856C4","#1468BE","#1079B7","#0C8AB1","#089CAA","#04ADA4","#00BF9E")


#rainbow colors
colors <- c("#1a1334",  "#26294a" , "#01545a",    
            "#017351",    "#03c383",    "#aad962",
            "#fbbf45",    "#ef6a32",    "#ed0345",   
            "#a12a5e",    "#710162",    "#110141") 
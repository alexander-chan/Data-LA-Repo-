#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
requests2017 <- read_csv('C:/Users/conor/Downloads/MyLA311_Service_Request_Data_2017.csv')


library(lubridate)
library(zoo)
requests2017 <- requests2017[,c(1,2,3,4,5,6,7,8,9,10,21,22,23,24,28,29,31,32,33)]
BOSService <- requests2017[requests2017$Owner=="BOS",]
table(BOSService$Status)
BOSServiceC <- BOSService[BOSService$Status=="Closed",]
BOSServiceC$Created <- as.POSIXct(BOSServiceC$CreatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles") 
BOSServiceC$Updated <- as.POSIXct(BOSServiceC$UpdatedDate,"%m/%d/%Y %I:%M:%S %p",tz = "America/Los_Angeles")
BOSServiceC$TimeTaken <- (as.numeric(BOSServiceC$Updated) - as.numeric(BOSServiceC$Created))/86400
summary(BOSServiceC$TimeTaken)
BOSServiceC <- BOSServiceC[!is.na(BOSServiceC$TimeTaken),]
BOSServiceC[BOSServiceC$TimeTaken < 0,]
BOSServiceC <- BOSServiceC[-BOSServiceC$TimeTaken <0,]
BOSServiceC[BOSServiceC$TimeTaken < 0,]

x2 <- BOSServiceC %>% 
  group_by(CD) %>% 
  summarise(mean(TimeTaken), median(TimeTaken), mean(hour(Updated)))

x3 <- merge(x2, BOSServiceC, by = 'CD')

x3$is_one_day <- ifelse(x3$TimeTaken <= 1, 1, 0)
x3$is_one_week <- ifelse(x3$TimeTaken <= 7, 1, 0)
x3$is_one_month <- ifelse(x3$TimeTaken <= 30, 1, 0)

LA <- get_map('Los Angeles')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$map <- renderPlot({
    
    if(input$n_breaks == 'None') {
      ggmap(LA)
    }
    else if(input$n_breaks == 'All') {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    
    else if(input$n_breaks == 1) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 1,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 2) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 2,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 3) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 3,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 4) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 4,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 5) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 5,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 6) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 6,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 7) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 7,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 8) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 8,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 9) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 9,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 10) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 10,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 11) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 11,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 12) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 12,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 13) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 13,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else if(input$n_breaks == 14) {
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 14,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
    else{
      ggmap(LA) +
        stat_density2d(data = na.omit(x3[x3$CD == 15,c(16,17,25)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
    }
  })
  
})

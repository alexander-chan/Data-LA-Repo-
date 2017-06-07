library(shiny)
library(shinydashboard)
#library(shinyjs)
library(tidyverse)
library(stringr)
library(lubridate)
library(plotly)
library(leaflet)
library(googleVis)

#source("R/load_data.R")
#source("R/subsets.R")
#source("R/load_shapefile.R")
#source("R/value_counts.R")

# JS function ------------------------------------------------------------------ 
#scroll <- "
#shinyjs.scroll = function() { 
#$('body').animate({ scrollTop: 0 }, 'slow'); } "

# Colors ----------------------------------------------------------------------- 
pal <- RColorBrewer::brewer.pal(11, "Spectral")
qual5 <- c(pal[1], pal[3], pal[4], pal[9], pal[10])
qual6 <- c(pal[1], pal[3], pal[4], pal[8], pal[9], pal[10])
qual7 <- c(pal[1], pal[3], pal[4], pal[7], pal[8], pal[9], pal[10])
cool <- c(pal[7], pal[8], pal[9], pal[10], pal[11])
warm <- c(pal[5], pal[4], pal[3], pal[2], pal[1])
cool_gradient <- data_frame(
  range = c(0.000, 0.115, 0.290, 0.750, 1.000),
  hex = cool
)
warm_gradient <- data_frame(
  range = c(0.000, 0.115, 0.290, 0.750, 1.000),
  hex = warm
)


requests2017 <- read_csv("C:/Users/Joseph/Downloads/MyLA311_Service_Request_Data_2017.csv")


library(lubridate)
library(zoo)
library(dplyr)
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

library(ggmap)
LA <- get_map('Los Angeles')

# Read data -------------------------------------------------------------------- 
#data <- load_data()
#data <- subset_data(data)
#districts <- load_shapefile(data)

# ui --------------------------------------------------------------------------- 
header <- dashboardHeader(
  title = tags$a(href = "",
                 tags$img(src = "www/seal_of_los_angeles.png", height = "45", width = "40",
                          style = "display: block; padding-top: 5px;"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sankey", tabName = "sankey", icon = icon("bar-chart")),
    menuItem("Maps", tabName = "maps", icon = icon("map-o"))
  )
)

body <- dashboardBody(
  #useShinyjs(),
  #extendShinyjs(text = scroll),
  tags$body(id = "body"),
  includeCSS("www/custom.css"),
  tabItems(
    
  tabItem(
    tabName = "maps",
    selectInput(input = 'n_breaks',
                label = 'Select Which Council District',
                choices = c('None', 1:15, 'All'),
                selected = 1),
    mainPanel(
      plotOutput("map")
    )
    
  ),
  
  tabItem(
    tabName = "sankey",
    mainPanel(
      htmlOutput("view"))
  )
  
  )
) # body

ui <- dashboardPage(header, sidebar, body)

# server ----------------------------------------------------------------------- 
server <- function(input, output) { 
  
  output$view <- renderGvis({
    
    df <- data.frame(origin=c(
      rep("Q2 #1",3), rep("Q2 #2",3), rep("Q2#3",3)),
      visit=c(
        rep(c("Q3 #1", "Q3 #2", "Q3 #3"),3)),
      weights=c(
        c(917, 34, 64, 29, 13, 32, 14, 4, 28) ) )
    
    
    gvisSankey(df, from="origin", 
               to="visit", weight="Count",
               options=list(
                 height=500,
                 width=1000,
                 sankey="{link:{color:{fill:'lightblue'}}}"
               ))    
    
    
  })
  
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
  
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)


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
    menuItem("Sankey", tabName = "sankey", icon = icon("bar-chart"))
  )
)

body <- dashboardBody(
  #useShinyjs(),
  #extendShinyjs(text = scroll),
  tags$body(id = "body"),
  includeCSS("www/custom.css"),
  
  mainPanel(
    htmlOutput("view")
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
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)
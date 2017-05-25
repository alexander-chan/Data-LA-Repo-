

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)
library(viridis)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Visualiztion of 311 Call Analysis"),
  
  # Tabset
  tabsetPanel(
    tabPanel(title = "Data Overview", 
             navbarPage(title = "Year",
                        tabPanel( title = "2016 Overview",
                                  dateRangeInput("dtr", "Date Range:", start = "2016-01-01", end = "2016-11-27"),
                                  plotOutput("space_overview"),
                                  br(),
                                  plotlyOutput("time_overview"),
                                  helpText("As we can see in the plot")),
                        tabPanel(title = "2011-2015 Overview",
                                 dateRangeInput("dtr2", "Date Range:", start = "2011-01-01", end = "2015-05-31"),
                                 plotOutput("space_overview2"),
                                 br(),
                                 plotlyOutput("time_overview2"),
                                 helpText("As we can see in the plot"))
             )
    ),
    
    tabPanel(title = "Call Resolution", 
             
             sidebarLayout(      
               # Define the sidebar with one input
               sidebarPanel(
                 selectInput("call_type", "Request Type:", 
                             choices=c("Processed Info", "Transferred Info", "Referred Info", "Invalid Info", "Given Info")),
                 hr(),
                 helpText("Invalid Info concludes Caller Hanger up, Got Voicemail, Info NotAvailable, Line Busy and N/A")
               ),
               
               # Create a spot for the barplot
               mainPanel(
                 plotOutput("map"),
                 plotlyOutput("heatmap")
               )
             )
    ),
    tabPanel(title = "Process Efficiency",
             plotlyOutput("eff"),
             helpText("As we can see in the plot"))
  )
  
  
  
)



#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Heat Map of LA Clean Streets by County District"),
  
  selectInput(input = 'n_breaks',
              label = 'Select Which Council District',
              choices = c('None', 1:15, 'All'),
              selected = 1),
  mainPanel(plotOutput('map'))
    )
  )

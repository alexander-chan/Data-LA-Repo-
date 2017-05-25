# Contributed by Joe Cheng, February 2013
# Requires googleVis version 0.4.0 and shiny 0.4.0 or higher
# server.R
library(googleVis)
library(shiny)
shinyServer(function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$view <- renderGvis({
    gvisScatterChart(datasetInput(), options=list(width=400, height=450))
  })
})
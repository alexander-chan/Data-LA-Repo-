# Markus Gesmann, February 2013
require(googleVis)
require(shiny)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
library(RCurl)
url <- "https://raw.githubusercontent.com/mages/diesunddas/master/Data/US%20Presidential%20Elections.csv"
dat <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
dat <- read.csv(text=dat)
## Add min and max values to the data
datminmax = data.frame(state=rep(c("Min", "Max"),21), 
                       demVote=rep(c(0, 100),21),
                       year=sort(rep(seq(1932,2012,4),2)))
dat <- rbind(dat[,1:3], datminmax)

shinyServer(function(input, output) {
  myYear <- reactive({
    input$Year
  })
  output$year <- renderText({
    paste("Democratic share of the presidential vote in", myYear())
  })
  output$gvis <- renderGvis({
    
    ##Old
    myData <- subset(dat, 
                     (year > (myYear()-1)) & (year < (myYear()+1)))
    gvisGeoChart(myData,
                 locationvar="state", colorvar="demVote",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400,
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                       ))     
  })
  
  ##New
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
               )
  )
  
})
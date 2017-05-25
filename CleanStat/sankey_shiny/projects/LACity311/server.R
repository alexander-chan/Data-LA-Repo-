

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
require(lubridate)
require(ggplot2)
require(dplyr)
#data <- read.csv("C:/Users/Chris/Desktop/DSO-545/Project/calls-short.csv")
data <- read.csv("C:/Users/Chris/Desktop/DSO-545/Project/311_Call_Center_Tracking_Data.csv")[-3469264,]
data$Date <- mdy(data[,1])
#data <- data[,-1]
data$Time <- hms(data$Time)
data$Hour <- hour(data$Time)
data$Minute <- minute(data$Time)
data$DoW <- wday(data$Date)
head(data)

# Define a server for the Shiny app
function(input, output) {
  dataset <- reactive(data[sample(nrow(data), input$sampleSize),])  
  
m_data <- reactive(  dataset() %>% filter(Department.Name == input$dept, Date >= as.POSIXct(input$dateRange[1]), Date <= as.POSIXct(input$dateRange[2])))
  # Fill in the spot we created for a plot
  output$hourPlot <- renderPlot({
    # Render a barplot
    qplot(factor(m_data()$Hour), geom="bar") + theme_minimal() + geom_bar(fill="#3399dd") + xlab("Hour") + ggtitle(input$dept)
  })

output$dowPlot <- renderPlot({
  # Render a barplot
  qplot(factor(m_data()$DoW), geom="bar") + theme_minimal() + geom_bar(fill="#3399dd") + xlab("Day of Week") + ggtitle(input$dept)
})

output$datePlot <- renderPlot({
  # Render a barplot
  m_data() %>% group_by(Date) %>% summarise(Count=n()) %>% ggplot(aes(x=Date, y=Count)) + theme_minimal() + geom_line() + xlab("Date") + ggtitle(input$dept) + geom_smooth(alpha=input$alpha/100, color="#3366dd")
})


output$dateRangeText  <- renderText({
  paste("input$dateRange is", 
        paste(class(input$dateRange), collapse = " to ")
  )
})



}



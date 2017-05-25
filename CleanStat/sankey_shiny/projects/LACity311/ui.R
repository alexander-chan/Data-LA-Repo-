#data <- read.csv("C:/Users/Chris/Desktop/DSO-545/Project/calls-short.csv")
data <- read.csv("C:/Users/Chris/Desktop/DSO-545/Project/311_Call_Center_Tracking_Data.csv")[-3469264,]
data$Date <- mdy(data[,1])
#data <- data[,-1]
data$Time <- hms(data$Time)
data$Hour <- hour(data$Time)
data$Minute <- minute(data$Time)
data$DoW <- wday(data$Date)
head(data)



# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).


# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Los Angeles Tickets by Department"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    

    # Define the sidebar with one input
    sidebarPanel(
      sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(data),
                  value=min(30000, nrow(data)), step=100, round=0),
      selectInput("dept", "Department:", 
                  choices=unique(data$Department.Name)),
      sliderInput('alpha', 'Confidence Band Transparency', min=0, max=100,
                  value=min(0, 100), step=10, round=0),
      dateRangeInput('dateRange',
                 label = 'Date range input: yyyy-mm-dd',
                 start = mdy("1-1-2011"), end = mdy("1-1-2017")
      ),
      hr(),
      helpText("Data from the City of Los Angeles.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      fluidRow(
        plotOutput("hourPlot")
      ),
      fluidRow(
        plotOutput("dowPlot")
      ),
      fluidRow(
        plotOutput("datePlot")
      )
    )
    
  )
)


library(RSQLite)
library(DBI)
library(ggplot2)
library(sparklyr)
library(dplyr)
library(shiny)
library(lubridate)
library(scales)
library(ggmap)

### Establish a connection with locl spark
sc=spark_connect(master="local")
LAMap=qmap("Los Angeles",zoom=10,maptype="roadmap")
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  # Application title
  titlePanel("311 Call Center Analysis"),
  fluidRow(
    column(4),
    column(6,
           selectInput(inputId="RT",
                       label="Select the Request Type",
                       choices = c("Bulky Items",
                                   "Graffiti Removal",
                                   "Dead Animal Removal",
                                   "Electronic Waste",
                                   "Feedback",
                                   "Homeless Encampment",
                                   "Illegal Dumping Pickup",
                                   "Metal/Household Appliances",
                                   "Multiple Streetlight Issue",
                                   "Other",
                                   "Report Water Waste",
                                   "Single Streetlight Issue"),
                       multiple = TRUE,
                       selectize = FALSE))
  ),
  
  tabsetPanel(
    tabPanel( title = "Request Type Analysis",
              fluidRow(column(6,
                       # Show a plot of the generated distribution
                       plotOutput(outputId="scatter")),
                       column(6,
                       plotOutput(outputId = "sbar"))
              ),
              fluidRow(column(6,
                       plotOutput(outputId = "trend")),
                       column(6,
                       plotOutput(outputId = "cycletime"))
                ),
              fluidRow( column(6,
                               plotOutput(outputId="RSPlot")))
              ),

    tabPanel(title = "Geographical Analysis",
             fluidRow( column(6, 
                              plotOutput(outputId="value")),
                       column(6,
                              plotOutput(outputId="APCPlot"))),
             fluidRow(column(6,
                             plotOutput(outputId="CDPlot")),
                      column(6,
                             plotOutput(outputId="PPPlot"))),
             fluidRow(column(6,
                             plotOutput(outputId="NCPlot")))
  ))

))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  sqlReq=renderText({
    
    paste("Select * from req_cnt WHERE requesttype in ('",paste(input$RT, collapse = "', '"),
          "')",sep="")
  })
  req_type = reactive({
    a=dbGetQuery(sc, sqlReq())
  })

  sqlAction=renderText({
    
    paste("Select * from reqNoSR WHERE requesttype in ('",paste(input$RT, collapse = "', '"),
          "')",sep="")
  })  
  
  action_taken = reactive({
    a=dbGetQuery(sc, sqlAction())
  })
  
  sqlTrend=renderText({
    
    paste("Select * from month_request WHERE requesttype in ('",paste(input$RT, collapse = "', '"),
          "')",sep="")
  })
    
  req_trend = reactive({
    a=dbGetQuery(sc, sqlTrend())
  })
  
  sqlCycle=renderText({
    
    paste("Select * from req_month WHERE requesttype in ('",paste(input$RT, collapse = "', '"),
          "')",sep="")
  })
  
  cycletime = reactive({
    a=dbGetQuery(sc, sqlCycle())
  })
  
  sqlText=renderText({
    paste("Select * from requesttypegeo WHERE requesttype in 
          ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  sqlAPC=renderText({ 
    paste("Select * from apcrequesttypecount where requesttype in 
          ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  sqlPP=renderText({ 
    paste("Select * from  pprequesttypecount
          where requesttype in ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  sqlCD=renderText({ 
    paste("Select * from  cdrequesttypecount where requesttype  in 
          ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  sqlSource=renderText({ 
    paste("Select * from  sourcerequesttypecount where requesttype  in 
          ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  sqlNC=renderText({ 
    paste("Select * from  NCRequestTypeCount where requesttype  in 
          ('",paste(input$RT, collapse = "', '"), "')",sep="")
  })
  
   output$scatter <- renderPlot({
      # generate bins based on input$bins from ui.R
     
    
     ### bar plot for request type call count
     ggplot(data = req_type(), aes(x = reorder(RequestType, -count), y = count, order = -as.numeric(count),
                                     fill = RequestType))+
       geom_bar(stat = "identity")+
       xlab("Request Type")+
       ylab("Request Count")+
       ggtitle("Request Type Call Count")+
       theme(axis.text.x = element_blank())
#       scale_y_continuous(labels = comma, breaks = seq(0,600000,100000))

   })
   
   ## Plot for Action Taken Vs Request Type
   output$sbar = renderPlot({

       ggplot(data = action_taken() ,aes(x = RequestType, y = percent, fill = ActionTaken))+
       geom_bar(stat = "identity", width = .7) +
       xlab("Request Type")+
       ggtitle("Action taken Vs Request Type")+
       geom_text(aes(y = pos, label = paste(round(percent,2),"%"), sep = ""), size=3)+
       coord_flip()
   })
   output$trend = renderPlot({
     
       ggplot(data = req_trend(), aes(x = factor(year_month_created), y = count,
                  color = RequestType, group = RequestType))+
       geom_line(size = 1)+
       xlab("Year-Month of Request Created")+
       ylab("Request Count")+
       ggtitle("Request Type Count")+
       theme(axis.text.x = element_text(angle = 30, hjust = 1))
   })
   
   output$cycletime = renderPlot({
     ## Plot showing trend for time taken to finish each type of request
     
     ggplot(data = cycletime(), aes(x = factor(year_month_created), y = avg_finish_time,
                                  color = RequestType, group = RequestType))+
       geom_line(size = 1)+
       xlab("Year-Month of Request Created")+
       ylab("Days to complete the request")+
       theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))+
       ggtitle("Request Type Completion Time")
   })
   df_data <- reactive({
     a <- dbGetQuery(sc, sqlText() )
   })
   output$value <- renderPlot({ LAMap+
       geom_point(data=df_data(),
                  aes(x=Longitude,y=Latitude),color="red",alpha=0.5)+
       ggtitle("Geographic Distribution of Requests")
   })
   df_data_APC=reactive ({
     a=dbGetQuery(sc, sqlAPC())
   })
   df_data_PP=reactive ({
     a=dbGetQuery(sc, sqlPP())
   })
   df_data_CD=reactive ({
     a=dbGetQuery(sc, sqlCD())
   })
   
   df_data_Source=reactive ({
     a=dbGetQuery(sc, sqlSource())
   })
   df_data_NC=reactive ({
     a=dbGetQuery(sc, sqlNC())
   })
   output$APCPlot=renderPlot({ ggplot(df_data_APC(),
                                      aes(x=reorder(APC,numbers),y=numbers,fill=APC))+
       geom_bar(stat="identity",show.legend = FALSE)+
       theme(axis.text.x=element_text(angle=-45,hjust=.1))+
       ylab("Number of Requests")+
       xlab(" ")+
       ggtitle("Distribution of requests over APC")
   })
   output$PPPlot=renderPlot({ ggplot(df_data_PP(),
                                     aes(x=reorder(PolicePrecinct,numbers),y=numbers,fill=PolicePrecinct))+
       geom_bar(stat="identity",show.legend = FALSE)+
       theme(axis.text.x=element_text(angle=-45,hjust=.1))+
       ylab("Number of Requests")+
       xlab(" ")+
       ggtitle("Distribution of requests over Police Precinct Area")
   })
   output$CDPlot=renderPlot({ ggplot(df_data_CD(),
                                     aes(x=reorder(CDMember,numbers),y=numbers,fill=CDMember))+
       geom_bar(stat="identity",show.legend = FALSE)+
       theme(axis.text.x=element_text(angle=-45,hjust=.1))+
       ylab("Number of Requests")+
       xlab(" ")+
       ggtitle("Distribution of requests over CD members")
   })
   output$RSPlot=renderPlot({ ggplot(df_data_Source(),
                                     aes(x=reorder(RequestSource,numbers),y=numbers,fill=RequestSource))+
       geom_bar(stat="identity",show.legend = FALSE)+
       theme(axis.text.x=element_text(angle=-45,hjust=.1))+
       ylab("Number of Requests")+
       xlab(" ")+
       ggtitle("Distribution of requests over Request Sources")
   })
   output$NCPlot=renderPlot({ ggplot(df_data_NC()[1:10,],
                                     aes(x=reorder(NCName,numbers),y=numbers,fill=NCName))+
       geom_bar(stat="identity",show.legend = FALSE)+
       theme(axis.text.x=element_text(angle=-45,hjust=.1))+
       ylab("Number of Requests")+
       xlab(" ")+
       ggtitle("Distribution of requests over NCs")
   })
})

# Run the application 
shinyApp(ui = ui, server = server)


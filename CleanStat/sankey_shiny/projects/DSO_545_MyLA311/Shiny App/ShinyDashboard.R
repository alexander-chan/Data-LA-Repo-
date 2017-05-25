# 03-layout.R
library(DBI)
library(ggplot2)
library(sparklyr)
library(dplyr)
library(ggplot2)

### Establish a connection with locl spark
sc=spark_connect(master="local")
dbListTables(sc)
library(ggmap)
LAMap=qmap("Los Angeles",zoom=10,maptype="roadmap")

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3),
    column(6,
  selectInput(inputId="RT",label="Choose a Request Type",c("--Select--","Bulky Items",
                                                  "Graffiti Removal",
                                                  "Metal/Household Appliances",
                                                  "Illegal Dumping Pickup",
                                                  "Electronic Waste",
                                                  "Dead Animal Removal",
                                                  "Other",
                                                  "Single Streetlight Issue",
                                                  "Homeless Encampment",
                                                  "Multiple Streetlight Issue",
                                                  "Report Water Waste",
                                                  "Feedback")))),
  fluidRow( column(6, 
  plotOutput(outputId="value")),
  column(6,
         plotOutput(outputId="RSPlot"))),
  fluidRow(column(6,
  plotOutput(outputId="APCPlot")),
  column(6,
         plotOutput(outputId="CDPlot"))),
  fluidRow(column(6,
                  plotOutput(outputId="PPPlot")),
           column(6,
                  plotOutput(outputId="NCPlot")))
           
  )



server <- function(input, output) {
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
          where requesttype in ('",paste(input$RT, collapse = "', '"),           "')",sep="")
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
}

shinyApp(ui = ui, server = server)
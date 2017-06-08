####################
## source scripts ##
####################

setwd("~/GitHub/Data-LA-Repo-") # set your local working directory

# If the script doesn't run:
# 1) make sure you're in the correct working directory
# 2) make sure you installed all the pacakges

source("./city_la_shiny/code/0_load_data.R")
source("./city_la_shiny/code/1_packages.R") # load all libraries
source("./city_la_shiny/code/2_colors.R") # load color palettes
source("./city_la_shiny/code/3_sankey_prep.R") # load sankey weights
source("./city_la_shiny/code/4_issue4_prep.R") # load issue 4 prep
source("./city_la_shiny/code/5_issue5_prep.R") # load issue 5 prep
source("./city_la_shiny/code/6_Call_Center_Prep.R") #Loading and fixing call center

# ui --------------------------------------------------------------------------- 
header <- dashboardHeader(
  title = tags$a(href = "",
                 tags$img(src = "./city_la_shiny/www/seal_of_los_angeles.png", height = "45", width = "40",
                          style = "display: block; padding-top: 5px;"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sankey Diagram", tabName = "sankey", icon = icon("bar-chart")),
    menuItem("Miles Per Sewer Cleaned", tabName = "sewer", icon = icon("bar-chart")),
    menuItem("Overflows", tabName = "overflow", icon = icon("bar-chart")),
    menuItem("Call Center Requests", tabName = "callcenterrequests",icon = icon("bar-chart")),
    menuItem("Call Center Timing",tabName = "callcentertiming",icon = icon("table"))
  )
)

body <- dashboardBody(
  #useShinyjs(),
  #extendShinyjs(text = scroll),
  tags$body(id = "body"),
  includeCSS("./city_la_shiny/www/custom.css"),
  
  
  tabItems(
    tabItem(
      tabName = "sankey",
      mainPanel(htmlOutput("sankey_view"))),
    
    
   tabItem(
     tabName = "sewer",
     mainPanel(plotlyOutput("sewer_view"))),
    
  
  tabItem(
    tabName = "overflow",
    mainPanel(plotlyOutput("overflow_view"))),

  
  tabItem(
    tabName = "callcenterrequests",
    selectInput(input = 'CD',
                label = 'Select Which Council District',
                choices = 1:15,
                selected = 1),
    selectInput(input = 'Year',
                label = 'Select Which Year',
                choices = c("2016","2017"),
                selected = "2017"),
    mainPanel(plotlyOutput("callcenter1_view"))),
  
  tabItem(
    tabName = "callcentertiming",
    selectInput(input = 'Year',
                label = 'Select Which Year',
                choices = c(2016,2017),
                selected = 2016),
    selectInput(input = 'Week',
                label = 'Select Which Week',
                choices = 1:52,
                selected = 1),
    mainPanel(plotlyOutput("callcenter2_view")))
  )
)
  
 # body

ui <- dashboardPage(header, sidebar, body)

# server ----------------------------------------------------------------------- 
server <- function(input, output) { 
  
  #################################
  #       Sankey Diagram          #  
  #################################
  output$sankey_view <- renderGvis({
    
    df <- data.frame(origin=c(
      rep("Q2 #1",3), rep("Q2 #2",3), rep("Q2#3",3)),
      visit=c(
        rep(c("Q3 #1", "Q3 #2", "Q3 #3"),3)),
      weights = weights_transition )
    
    gvisSankey(df, from="Q2", 
               to="Q3", weight="Count",
               options=list(
                 height=500,
                 width=1000,
                 caption="Flow of CleanStat Ratings in Los Angeles from 2016 Q2 to Q3",
                 sankey="{link:{color:{fill:'lightblue'}}}"
               ))   
  })
  
  #################################
  # Issue 4: Miles/sewage cleaned #
  #################################
  output$sewer_view <- renderPlotly({
    monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(`Month Factor`)) +
      geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
      theme_bw() +
      ggtitle('Bar Chart of Miles Cleaned by Month for Calendar Years between 2014-2016') +
      scale_x_discrete(limits = month_names)+
      scale_fill_manual(values=colors)+
      guides(fill = guide_legend(reverse = TRUE))
    
    ggplotly(monthly)
  })


  #################################
  # Issue 5: Overflow             #
  #################################
  output$overflow_view <- renderPlotly({
    monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% c(2015:2016),], aes(`Month Factor`)) +
      geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
      theme_bw() +
      ggtitle('Overflow Bar Chart by Month for Calendar Years 2015-2016') +
      scale_x_discrete(limits = month_names) +
      scale_fill_manual(values=colors)+
      guides(fill = guide_legend(reverse = TRUE))
    
    ggplotly(monthly)
  })
  
  #################################
  # Issue 8: Call Center          #
  #################################
  output$callcenter1_view <- renderPlotly({
    
    if(input$Year=="2016"){
      betterg2 <- plot_ly(xmodlist3.16[[as.numeric(input$CD)]]) %>%
        add_trace(x = ~week, y = ~nReported, type = 'bar', name = 'Reported',
                  marker = list(color = 'indianred'),
                  hoverinfo = "all") %>%
        add_trace(x = ~week, y = ~nSolved, type = 'scatter', mode = 'lines', name = 'Solved',
                  line = list(color = 'dodgerblue'),
                  hoverinfo = ~paste("Week", week, "Solved",nSolved))%>%
        layout(title = 'Reported and Solved Calls per Week',
               xaxis = list(title = "Week"),
               yaxis = list(side = 'left', title = 'Number of Cases', showgrid = FALSE, zeroline = FALSE))
      betterg2
    }else if(input$Year=="2017"){
      betterg3 <- plot_ly(xmodlist3.17[[as.numeric(input$CD)]]) %>%
        add_trace(x = ~week, y = ~nReported, type = 'bar', name = 'Reported',
                  marker = list(color = 'indianred'),
                  hoverinfo = "all") %>%
        add_trace(x = ~week, y = ~nSolved, type = 'scatter', mode = 'lines', name = 'Solved',
                  line = list(color = 'dodgerblue'),
                  hoverinfo = ~paste("Week", week, "Solved",nSolved))%>%
        layout(title = 'Reported and Solved Calls per Week',
               xaxis = list(title = "Week"),
               yaxis = list(side = 'left', title = 'Number of Cases', showgrid = FALSE, zeroline = FALSE))
      betterg3
    }
  })
  output$callcenter2_view <- renderPlotly({
    if(as.numeric(input$Year) == 2016){
    p2 <- plot_ly(
      x = c("Bulky Items","Dead Animal Removal","Electronic Waste","Feedback","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other"), y = c(as.character(15:1)),
      z = valuematavg(xmodlist16,as.numeric(input$Week)),
      type="heatmap", hoverinfo = "x+y+text",text = valuemat(xmodlist16,as.numeric(input$Week)))
    p2
    }else if(as.numeric(input$Year) == 2017){
      p3 <- plot_ly(
        x = c("Bulky Items","Dead Animal Removal","Electronic Waste","Feedback","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other"), y = c(as.character(15:1)),
        z = valuematavg(xmodlist,as.numeric(input$Week)),
        type="heatmap", hoverinfo = "x+y+text",text = valuemat(xmodlist,as.numeric(input$Week)))
      p3 
    }
  })
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)
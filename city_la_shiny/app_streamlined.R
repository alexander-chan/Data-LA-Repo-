####################
## source scripts ##
####################

setwd("~/GitHub/Data-LA-Repo-") # set your local working directory

# If the script doesn't run:
# 1) make sure you're in the correct working directory
# 2) make sure you installed all the pacakges


source("./city_la_shiny/code/1_packages.R") # load all libraries
source("./city_la_shiny/code/0_load_data.R")
source("./city_la_shiny/code/2_colors.R") # load color palettes
source("./city_la_shiny/code/3_sankey_prep.R") # load sankey weights
source("./city_la_shiny/code/4_issue4_prep.R") # load issue 4 prep
source("./city_la_shiny/code/5_issue5_prep.R") # load issue 5 prep
source("./city_la_shiny/code/6_Call_Center_Prep.R") #Loading and fixing call center

#Determines the date so we can refresh every ____day
date <- as.POSIXct(Sys.time())

#If we refresh on Sunday, leads to issue where Sunday will have no data.
#Thus, we technically will refresh on Monday when we have at least one day of data
ifelse(wday(date) == 1, previous_sunday <- floor_date(date - 86400, "week"), 
       previous_sunday <- floor_date(date, "week"))

testSocrata2 <- read.socrata(paste("https://data.lacity.org/A-Well-Run-City/MyLA311-Service-Request-Data-2017/d4vt-q4t5?$where=updateddate >= ", 
                                   paste0("'",previous_sunday, "'")))



#install.packages('gsheet')
library(gsheet)
flow_data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1aTjWGg8P3C5oiz4HkB7q4rmOCXmVCoZ1VHyRw3IjDso/edit#gid=0')
names(flow_data) <- flow_data[1,]
flow_data <- flow_data[c(2:46),]
flow_data[,c(1,3:12, 14:16)] <- sapply(flow_data[,c(1,3:12,14:16)], as.numeric)

i <- min(na.omit(flow_data[,1]))
for(j in 1:nrow(flow_data)) {
  flow_data[j,1] <- i
  flow_data[j,14] <- i
  if(!(is.na(flow_data[j+1,1]))) {
    i <- i + 1
  }
}

flow_data$`Percent Reuse of Total Flow` <- as.numeric(sub("%", "", flow_data$`Percent Reuse of Total Flow`))
flow_data$`Percent Reuse` <- as.numeric(sub('%', '', flow_data$`Percent Reuse`))

for(i in seq(4, nrow(flow_data), 4)) {
  for(j in 1:3) {
    flow_data[i - j, 15] <- flow_data[i,15]
    flow_data[i - j, 16] <- flow_data[i,16]
    flow_data[i - j, 17] <- flow_data[i,17]
  }
}

quarter_names <- c('JAN - MAR', 'APR - JUN', 'JUL - SEP', 'OCT - DEC')
flow_data$quarter_rev_factor <- factor(flow_data$QUARTER, levels = rev(quarter_names))
flow_data$quarter_factor <- factor(flow_data$QUARTER, levels = quarter_names)
#Get the map of LA
LA <- get_map('Los Angeles')
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
    menuItem("Heat Maps", tabName = 'heatmap', icon = icon("bar-chart")),
    menuItem("Flow Data", tabName = "flow", icon = icon("bar-chart")),
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
     tabName = "heatmap",
     checkboxGroupInput(input = 'n_breaks',
                        label = 'Select Which Council District',
                        choices = c('None', 1:15, 'All'),
                        selected = 1),
     mainPanel(plotlyOutput("heatmap_view"))),
   
   tabItem(
     tabName = "flow",
     selectInput(input = 'years',
                 label = 'Select Calendar Year',
                 choices = c(2006:2016, 'All'),
                 selected = 'All'),
     mainPanel(plotlyOutput("flow_view"))),  
  
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
                selected = 2017),
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
  #           Heatmaps            #
  #################################
  output$heatmap_view <- renderPlotly({
    if('None' %in% input$n_breaks) {
      heat_map <- ggmap(LA)
      ggplotly(heat_map)
    }
    else if('All' %in% input$n_breaks) {
      heat_map <- ggmap(LA) +
        stat_density2d(data = na.omit(testSocrata2[,c(24,25,31)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
      
      ggplotly(heat_map)
    }
    else{
      heat_map <- ggmap(LA) +
        stat_density2d(data = na.omit(testSocrata2[testSocrata2$CD %in% input$n_breaks,c(24,25,31)]), aes(x = Longitude, y = Latitude, fill = ..level..), geom = 'polygon') +
        scale_fill_gradient(low = 'yellow', high = 'red')
      ggplotly(heat_map)
    }
    
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
  
  #################################
  #         Issue 9: Flow         #
  #################################
  output$flow_view <- renderPlotly({
    if(input$years %in% c(2006:2016)) {
      monthly <- ggplot(data = flow_data[flow_data$YEAR %in% input$years,], aes(x = quarter_factor)) +
        geom_bar(aes(weight = `Total Combined Reuse`, fill = quarter_factor)) +
        theme_bw() +
        ggtitle(paste('Bar Chart of Total Recycle by Quarter in Year', input$years))
      
      ggplotly(monthly)
    }
    else {
      flow <- ggplot(data = flow_data, aes(x = quarter_factor)) +
        geom_bar(aes(weight = `Total Combined Reuse`, fill = quarter_factor)) +
        theme_bw() +
        ggtitle('Bar Chart of Total Recycle by Quarter')
      
      ggplotly(flow)
    }
  })
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)
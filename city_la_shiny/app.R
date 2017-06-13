setwd("C:/Users//conor//Documents//GitHub//Data-LA-Repo-") # set your local working directory
# If the script doesn't run:
# 1) make sure you're in the correct working directory
# 2) make sure you installed all the pacakges

source("./city_la_shiny/code/0_packages.R") # load all libraries
source("./city_la_shiny/code/1_load_data.R")
source("./city_la_shiny/code/2_colors.R") # load color palettes
source("./city_la_shiny/code/3_sankey_prep.R") # load sankey weights
source("./city_la_shiny/code/4_issue4_prep.R") # load issue 4 prep
source("./city_la_shiny/code/5_issue5_prep.R") # load issue 5 prep
source("./city_la_shiny/code/6_Call_Center_Prep.R") #Loading and fixing call center
source("./city_la_shiny/code/7_other_data_prep.R") #Loading flow and weekly call center data
source("./city_la_shiny/code/8_issue_7_Prep.R") #loading issue 7

#Get the map of LA
#LA <- get_map('Los Angeles')
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
    menuItem("Catch Basin", tabName = "basin", icon = icon("bar-chart")),
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
      selectInput(input = 'n_years',
                  label = 'Select Calendar Year',
                  choices = c(unique(miles_sewage_cleaned$`Calendar Year`), 'All'),
                  selected = 'All'),
      mainPanel(plotlyOutput("sewer_view"))),
    
    
    tabItem(
      tabName = "overflow",
      selectInput(input = 'n_years2',
                  label = 'Select Calendar Year',
                  choices = c(unique(sewer_overflow$`Calendar Year`), 'All'),
                  selected = 'All'),
      mainPanel(plotlyOutput("overflow_view"))),
    
    tabItem(
      tabName = "heatmap",
      checkboxGroupInput(input = 'n_breaks',
                  label = 'Select Which Council District',
                  choices = c('None', 1:15, 'All'),
                  selected = 1,
                  inline = TRUE),
      mainPanel(leafletOutput("heatmap_view"))),
    
    tabItem(
      tabName = "flow",
      selectInput(input = 'years',
                  label = 'Select Calendar Year',
                  choices = c(unique(as.character(flow_melt$YEAR)), 'All'),
                  selected = 'All'),
      mainPanel(plotlyOutput("flow_view"))),

    tabItem(
      tabName = "basin",
      selectInput(input = 'nyears',
                  label = 'Select Calendar Year',
                  choices = c(unique(catch_basin$`Calendar Year`), "All"),
                  selected = "All"),
      mainPanel(plotlyOutput("basin_view"))),
    
    tabItem(
      tabName = "callcenterrequests",
      selectInput(input = 'CD',
                  label = 'Select Which Council District',
                  choices = 1:15,
                  selected = 1),
      selectInput(input = 'Year',
                  label = 'Select Which Year',
                  choices = c("2016","2017"),
                  selected = "2016"),
      selectInput(input = 'RequestType',
                  label = 'Select Which Request Type',
                  choice = c("Bulky Items","Dead Animal Removal","Electronic Waste","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other","Feedback"),
                  selected = "Bulky Items"),
      mainPanel(plotlyOutput("callcenter1_view"))),
    
    tabItem(
      tabName = "callcentertiming",
      selectInput(input = 'YEAR',
                  label = 'Select Which Year',
                  choices = c("2016","2017"),
                  selected = "2017"),
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
    if(input$n_years %in% unique(miles_sewage_cleaned$`Calendar Year`)) {
      monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% input$n_years,], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Bar Chart of Miles Cleaned by Month for Calendar Year ', input$n_years)) +
        scale_x_discrete(limits = month_names)+
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
      
      ggplotly(monthly)
    }
    else {
      monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(`Month Factor`)) +
      geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
      theme_bw() +
      ggtitle(paste('Bar Chart of Miles Cleaned by Month for Calendar Years between', 
                     min(miles_sewage_cleaned$`Calendar Year`),
                     "to",
                     max(miles_sewage_cleaned$`Calendar Year`))) +
      scale_x_discrete(limits = month_names)+
      scale_fill_manual(values=colors)+
      guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
    
    ggplotly(monthly)
    }
  })
  
  
  #################################
  # Issue 5: Overflow             #
  #################################
  output$overflow_view <- renderPlotly({
    if(input$n_years2 %in% unique(sewer_overflow$`Calendar Year`)) {
      monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% input$n_years2,], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Overflow Bar Chart by Month for Calendar Year ', input$n_years2)) +
        scale_x_discrete(limits = month_names) +
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
      
      ggplotly(monthly)
    }
    else {
      monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% c(2015:2016),], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Overflow Bar Chart by Month for Calendar Years',
                min(sewer_overflow$`Calendar Year`),
                "to",
                max(sewer_overflow$`Calendar Year`))) +
        scale_x_discrete(limits = month_names) +
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
      
      ggplotly(monthly)
    }
  })
  #################################
  #           Heatmaps            #
  #################################
  output$heatmap_view <- renderLeaflet({    
    testSocrata2$TimeTaken <- round((testSocrata2$UpdatedDate - testSocrata2$CreatedDate)/86400)
    if('None' %in% input$n_breaks) {
      leaflet(na.omit(testSocrata2[,c("Longitude", "Latitude", "CD", "TimeTaken")])) %>% 
        addProviderTiles("Thunderforest.TransportDark") %>% 
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    }
    else if('All' %in% input$n_breaks) {
      leaflet(na.omit(testSocrata2[,c("Longitude", "Latitude", "CD", "TimeTaken")])) %>% 
        addProviderTiles("Thunderforest.TransportDark") %>% 
        addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~TimeTaken,
                   blur = 20, max = 0.05, radius = 15)
    }
    else {
      leaflet(na.omit(testSocrata2[testSocrata2$CD %in% input$n_breaks,c("Longitude", "Latitude", "CD", "TimeTaken")])) %>% 
        addProviderTiles("Thunderforest.TransportDark") %>% 
        addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~TimeTaken,
                   blur = 20, max = 0.05, radius = 15)
    }

  })
  
  #################################
  # Issue 8: Call Center          #
  #################################
  output$callcenter1_view <- renderPlotly({
    
    
    if(input$Year=="2016"){
      #thisCD <- xmodlist3.16[[as.numeric(input$CD)]]
      betterg2 <- plot_ly(newx7.16[(newx7.16$CD == as.numeric(input$CD)) & (newx7.16$RequestType == as.character(input$RequestType)),]) %>%
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
      #thisCD <- xmodlist3.17[[as.numeric(input$CD)]]
      betterg3 <- plot_ly(newx7.17[(newx7.17$CD == as.numeric(input$CD)) & (newx7.17$RequestType == as.character(input$RequestType)),]) %>%
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
    if(as.character(input$YEAR) == "2017"){
      p3 <- plot_ly(
        x = c("Bulky Items","Dead Animal Removal","Electronic Waste","Feedback","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other"), y = c(as.character(15:1)),
        z = valuematavg(xmodlist,as.numeric(input$Week)),
        type="heatmap", hoverinfo = "x+y+text",text = valuemat(xmodlist,as.numeric(input$Week)))
      p3 
    }else if(as.character(input$YEAR) == "2016"){
      p2 <- plot_ly(
        x = c("Bulky Items","Dead Animal Removal","Electronic Waste","Feedback","Homeless Encampment","Illegal Dumping Pickup","Metal/Household Appliances","Other"), y = c(as.character(15:1)),
        z = valuematavg(xmodlist16,as.numeric(input$Week)),
        type="heatmap", hoverinfo = "x+y+text",text = valuemat(xmodlist16,as.numeric(input$Week)))
      p2
    }
    
  })
  
  
  #################################
  #         Issue 9: Flow         #
  #################################
  output$flow_view <- renderPlotly({
      if(input$years %in% unique(flow_data$YEAR)) {
        yearly <- ggplot(flow_melt[flow_melt$YEAR %in% input$years,], aes(x = YEAR, y = value)) +
          geom_bar(aes(fill = variable), stat = 'identity', position = 'dodge') +
          theme_bw() +
          ggtitle(paste('Bar Chart of Average Flow and Reuse for ', input$years)) +
          ylab("Total Count")
        
        ggplotly(yearly)
      }
      else {
        yearly <- ggplot(flow_melt, aes(x = YEAR, y = value)) +
          geom_bar(aes(fill = variable), stat = 'identity', position = 'dodge') +
          theme_bw() +
          ggtitle('Bar Chart of Average Flow and Reuse by Year') +
          ylab("Total Count")
        
        ggplotly(yearly)
      }
  })
  
  #################################
  #     Issue 7: Catch Basin      #
  #################################
  output$basin_view <- renderPlotly({
    if(input$nyears %in% unique(catch_basin$`Calendar Year`)) {
      yearly <- ggplot(catch_basin[catch_basin$`Calendar Year` == input$nyears,], aes(`Month Factor`)) +
        geom_bar(aes(weight = NUMBER.OF.CATCH.BASIN.CLEANED, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Bar Chart of Catch Basins Cleaned by Month for Calendar Year ', input$nyears)) +
        scale_x_discrete(limits = month_names2)+
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
      
      ggplotly(yearly)
    }
    else{
      yearly <- ggplot(catch_basin, aes(`Month Factor`)) +
        geom_bar(aes(weight = NUMBER.OF.CATCH.BASIN.CLEANED, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Bar Chart of Catch Basins Cleaned by Month for All Years')) +
        scale_x_discrete(limits = month_names2)+
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE)) +
        xlab("Month")
      
      ggplotly(yearly)
    }
  })
  
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)
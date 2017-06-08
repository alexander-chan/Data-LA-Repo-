setwd("C:/Users//conor//Documents//GitHub//Data-LA-Repo-") # set your local working directory
# If the script doesn't run:
# 1) make sure you're in the correct working directory
# 2) make sure you installed all the pacakges

library(readr)
library(RSocrata)


source("./city_la_shiny/code/0_load_data.R")
source("./city_la_shiny/code/1_packages.R") # load all libraries
source("./city_la_shiny/code/2_colors.R") # load color palettes
source("./city_la_shiny/code/3_sankey_prep.R") # load sankey weights
source("./city_la_shiny/code/4_issue4_prep.R") # load issue 4 prep
source("./city_la_shiny/code/5_issue5_prep.R") # load issue 5 prep


library(lubridate)
library(ggmap)

#Determines the date so we can refresh every ____day
date <- as.POSIXct(Sys.time())

#If we refresh on Sunday, leads to issue where Sunday will have no data.
#Thus, we technically will refresh on Monday when we have at least one day of data
ifelse(wday(date) == 1, previous_sunday <- floor_date(date - 86400, "week"), 
       previous_sunday <- floor_date(date, "week"))

testSocrata2 <- read.socrata(paste("https://data.lacity.org/A-Well-Run-City/MyLA311-Service-Request-Data-2017/d4vt-q4t5?$where=updateddate >= ", 
                                   paste0("'",previous_sunday, "'")))
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
    menuItem("Heat Maps", tabName = 'heatmap', icon = icon("bar-chart"))
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
                  choices = c(2014:2016, 'All'),
                  selected = 'All'),
      mainPanel(plotlyOutput("sewer_view"))),
    
    
    tabItem(
      tabName = "overflow",
      selectInput(input = 'n_years2',
                  label = 'Select Calendar Year',
                  choices = c(2015, 2016, 'All'),
                  selected = 'All'),
      mainPanel(plotlyOutput("overflow_view"))),
    
    tabItem(
      tabName = "heatmap",
      checkboxGroupInput(input = 'n_breaks',
                  label = 'Select Which Council District',
                  choices = c('None', 1:15, 'All'),
                  selected = 1),
      mainPanel(plotlyOutput("heatmap_view")))
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
    if(input$n_years %in% c(2014:2016)) {
      monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% input$n_years,], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Bar Chart of Miles Cleaned by Month for Calendar Year ', input$n_years)) +
        scale_x_discrete(limits = month_names)+
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE))
      
      ggplotly(monthly)
    }
    else {
      monthly <- ggplot(data = miles_sewage_cleaned[miles_sewage_cleaned$`Calendar Year` %in% c(2014:2016),], aes(`Month Factor`)) +
      geom_bar(aes(weight = `Miles of Sewer Cleaned`, fill = `Month Factor`)) +
      theme_bw() +
      ggtitle('Bar Chart of Miles Cleaned by Month for Calendar Years between 2014-2016') +
      scale_x_discrete(limits = month_names)+
      scale_fill_manual(values=colors)+
      guides(fill = guide_legend(reverse = TRUE))
    
    ggplotly(monthly)
    }
  })
  
  
  #################################
  # Issue 5: Overflow             #
  #################################
  output$overflow_view <- renderPlotly({
    if(input$n_years2 %in% c(2015, 2016)) {
      monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% input$n_years2,], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle(paste('Overflow Bar Chart by Month for Calendar Year ', input$n_years2)) +
        scale_x_discrete(limits = month_names) +
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE))
      
      ggplotly(monthly)
    }
    else {
      monthly <- ggplot(data = sewer_overflow[sewer_overflow$`Calendar Year` %in% c(2015:2016),], aes(`Month Factor`)) +
        geom_bar(aes(weight = `Sanitary Sewer Overflows`, fill = `Month Factor`)) +
        theme_bw() +
        ggtitle('Overflow Bar Chart by Month for Calendar Years 2015-2016') +
        scale_x_discrete(limits = month_names) +
        scale_fill_manual(values=colors)+
        guides(fill = guide_legend(reverse = TRUE))
      
      ggplotly(monthly)
    }
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
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)

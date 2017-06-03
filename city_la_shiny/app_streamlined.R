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
    menuItem("Overflows", tabName = "overflow", icon = icon("bar-chart"))
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
     mainPanel(plotlyOutput("sewer_view")))
    )
  
) # body

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
    
    plotly::ggplotly(monthly)
  })


  #################################
  # Issue 5: Overflow             #
  #################################
  output$overflow <- renderPlotly({
    
  })
  
}
# end server


# run app ---------------------------------------------------------------------- 
shinyApp(ui, server)
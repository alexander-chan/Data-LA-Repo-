library(shiny)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(png)
library(dplyr)
library(gridExtra)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",tabName = "home",icon=icon("dashboard"),
             menuSubItem("Pages",tabName = "page1",icon=icon("dashboard")
             )),
    menuItem("Overview",tabName = "sum",icon=icon("dashboard")),
    menuItem("Request Analysis",tabName = "sum",icon=icon("dashboard"),
             menuItem("Seasonality",tabName = "season",icon=icon("dashboard"),
                      menuSubItem("Requests",tabName = "seasonr",icon=icon("dashboard")),
                      menuSubItem("Time",tabName = "time",icon=icon("dashboard"))
             ),
             menuItem("Demographics",tabName = "geo",icon=icon("dashboard"))
    ),
    
    menuItem("Request Channel Analysis",tabName = "ca",icon=icon("dashboard")),
    menuItem("Processing Efficiency Analysis",tabName = "pt",icon=icon("dashboard")),
    menuItem("Summary",tabName = "sm",icon=icon("dashboard"))
    
  ))

body<- dashboardBody(
  tabItems(
    tabItem(tabName = "page1",   
            tabBox(height=600,width=350,
                   selected="Home",
                   tabPanel("Home",  
                            h1("LA City 311 Call Center Service Improvement Analysis",align="center"),
                            hr(),
                            fluidRow(column(1),
                                     column(4,
                                            img(src='la.png',width=300,height=350,align="center")),
                                     column(7,
                                            h4("Xiangyi(Sherry) Che",align="right"),
                                            h4("Man(Mandy) Feng",align="right"),
                                            h4("Qianhui(Gabby) You",align="right"),
                                            h4("Yi(Chloe) Zhan",align="right"),
                                            h4("Fan(Tiffany) Yang",align="right"))
                            )),
                   tabPanel("Objective and Approaches", h1("Objective and Approaches",align="center"),
                            h4("Identify patterns of 311 service requests data 
                               related to the social, demographic and economic 
                               landscape of Los Angeles neighborhoods, and leverage 
                               the data analysis results to:  ",align="left"),hr(),
                           
                            h4("1.Provide defined metrics and control mechanisms 
                               to measure the 311 system and government service quality",align="left"),
                            
                            h4("2.Identify requests patterns to support government services 
                               to become more proactive and responsive",align="left"),
                           
                            h4("3.Improve the operation efficiency and optimize cost 
                               through effective resource allocation",align="left"))
                   )   ) ,
    tabItem(tabName = "sm",   
            tabBox(height=600,width=350,
                   selected="Business Insight",
                   tabPanel("Business Insight",  
                            h1("Business Insight",align="center"),
                            hr(),
                           h4("1. Leverage information of service volume to effectively allocate current 
                              resources that matches with volume and service demand"),
                           h4("2. High volume environmental issues related requests are more common across low 
                              income and education or high minority population neighbourhood"),
                           h4("3. Mobile app usage has demonstrated strong growth trend"),
                           h4("4. The request resolution capability varies across different service requests"),
                           h4("5. The operational efficiency is largely driven by the initiatives of 
                              the service tech people in the field rather than the general public")
                            ),
                          
                   tabPanel("Recommendation", h1("Recommendation",align="center"),
                            hr(),
                            h4("1. Clearly brand and define the 311 service categories, ensure
                               request align with service provided"),
                            h4("2. Pay close attention to low income and developing neighbourhood"),
                            h4("3. Incorporate volume seasonality effect into resource planning and allocating process"),
                            h4("4. Invest into app technology development, ensure app interface user friendly,
                               and easy to navigate and submit requests"),
                            h4("5. Pay close attention to Council District 4,5 and 11 to allocate more resources
                               to improve its current operating efficiency")
                            ) )  ) ,
    tabItem(tabName = "sum",  h2("Overview - Request Type & Requst Source"),
            fluidRow(
              tabBox(height=350,width=200,
                     selected="LA Map",
                     tabPanel("LA Map",plotOutput("lamap")),
                     tabPanel("Request Type", selectInput("overviewrt",
                                                          label="Graph Type",
                                                          choices=list("Bar"="overviewbarrt",
                                                                       "Comparison"="rsrt"),
                                                          selected="overviewbarrt"),
                              plotOutput("overviewrtp")),
                     tabPanel("Request Source",  plotOutput("overviewrsp"))
              ))),
    
    tabItem(tabName="seasonr", fluidRow(
      tabBox(height=350,width=400,
             selected="Request Type",
             tabPanel("Request Type", selectInput("seasonplotrt",
                                                  label="By",
                                                  choices=list("Month"="monthrt",
                                                               "Weekday"="wdayrt",
                                                               "Hour"="hourrt"),
                                                  selected="monthrt"),
                      plotOutput("seasonrtp")),
             tabPanel("Request Source", selectInput("seasonplotrs",
                                                    label="By",
                                                    choices=list("Month"="monthrs",
                                                                 "Weekday"="wdayrs",
                                                                 "Hour"="hourrs"),
                                                    selected="monthrs"),
                      plotOutput("seasonrsp"))
      )
    )
    ),
    tabItem(tabName="time", fluidRow(
      tabBox(height=350,width=400,
             selected="Request Type",
             tabPanel("Request Type", selectInput("timert",
                                                  label="Select",
                                                  choices=list("Month Vs. Weekday"="monwdayrt",
                                                               "Weekday Vs. Hour"="wdayhrrt",
                                                               "Month Vs. Hour"="monthhrrt"),
                                                  selected="monwdayrt"),
                      plotOutput("timertp"))
      )
    )
    ),
    tabItem(tabName="geo", fluidRow(
      tabBox(height=350,width=400,   
             selected="Income",
             tabPanel("Income", plotOutput("incomert")),
             tabPanel("Race", plotOutput("racert"))
      )
    )),
    tabItem(tabName = "pt",
            h2("Processing Efficiency Analysis"),
            fluidRow(
              tabBox(height=350,width=400,
                     selected="By Request Types",
                     tabPanel("By Request Types", plotOutput("pt0")),
                     tabPanel("By Council District", plotOutput("pt1")),
                     tabPanel("By Workload", plotOutput("pt2")),
                     tabPanel("Request Type by CD", plotOutput("pt3")),
                     tabPanel("Request Source by CD", plotOutput("pt4"))
              ), hr(), hr()
            )
    ),
    tabItem(tabName = "ca",
            h2("Request Channel Analysis"),
            fluidRow(
              tabBox(height=350,width=400,
                     selected="Overview",
                     tabPanel("Overview", plotOutput("callappover")),
                     tabPanel("Trend", plotOutput("callappline")),
                     tabPanel("Heatmap", plotOutput("callappheat")
                     ), hr(), hr()
              )
            ))))

ui<-dashboardPage(
  dashboardHeader(title = "Shiny Star"),
  sidebar,
  body)


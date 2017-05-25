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

server <- function(input, output) {
  ### overview data #####
  rt<-reactive({
    rt<-read.csv("rt.csv")
  })
  
  rs<-reactive({
    rs<-read.csv("rs.csv")
  })
  
  rsrt<-reactive({
    rsrt<-read.csv("rsrt.csv")
  })
  
  lap<-reactive({
    lap<-read.csv("lap.csv")
  })
  
  lab<-reactive({
    lab<-read.csv("lab.csv")
  })
  callappover<-reactive({
    callappover<-read.csv("callappover.csv")
  })
  callappline<-reactive({
    callappline<-read.csv("callappline.csv")
  })
  calltrend<-reactive({
    calltrend<-read.csv("calltrend.csv")
  })
  apptrend<-reactive({
    apptrend<-read.csv("apptrend.csv")
  })
  
  callheat<-reactive({
    callheat<-read.csv("callheat.csv")
  })
  appheat<-reactive({
    appheat<-read.csv("appheat.csv")
  })
  
  ############pt data
  dc1<-reactive({
    dc1<-read.csv("dc1.csv")
  })
  dc2<-reactive({
    dc2<-read.csv("dc2.csv")
  })
  dc3<-reactive({
    dc3<-read.csv("dc3.csv")
  })
  
  processtime<-reactive({
    processtime<-read.csv("processtime.csv")
  })
  
  #######season/season data###
  monthrt<-reactive({
    monthrt<-read.csv("monthrt.csv")
  })
  wdayrt<-reactive({
    wdayrt<-read.csv("wdayrt.csv")
  })
  hourrt<-reactive({
    hourrt<-read.csv("hourrt.csv")
  })
  
  monthrs<-reactive({
    monthrs<-read.csv("monthrs.csv")
  })
  wdayrs<-reactive({
    wdayrs<-read.csv("wdayrs.csv")
  })
  hourrs<-reactive({
    hourrs<-read.csv("hourrs.csv")
  })
  
  ##########season time########
  monwdayrt = reactive({
    monwdayrt = read.csv("monwdayrt.csv")
  })
  monthhrrt = reactive({
    monthhrrt = read.csv("monthhrrt.csv")
  })
  wdayhrrt = reactive({
    wdayhrrt = read.csv("wdayhrrt.csv")
  })
  
  
  ######geo cd#########
  geocdrt<-reactive({
    geocdrt<-read.csv("geocdrt.csv")
  })
  geocdrs<-reactive({
    geocdrs<-read.csv("geocdrs.csv")
  })
  
  ###########geodemo rt######
  incomert<-reactive({
    incomert<-read.csv("incomert.csv")
  })
  
  racert<-reactive({
    racert<-read.csv("racert.csv")
  })
  
  
  #########overviewplotrt##############
  overviewrtp = reactive({
    if(input$overviewrt == "overviewbarrt") {
      ggplot(rt(),aes(reorder(RequestType,count),count))+
        geom_bar(stat="identity",fill="pink")+
        theme_classic()+
        coord_flip()+
        ylab("Request Volume")+
        xlab("Request Type")+
        scale_y_continuous(breaks=seq(0,550000,30000),
                           labels=seq(0,550000,30000),
                           limits=c(0,580000))+
        geom_text(aes(label=count),hjust=-0.1)
    }else if(input$overviewrt=="rsrt"){ 
      ggplot(rsrt(),aes(x=reorder(RequestType,count),y=count))+
        geom_bar(stat="identity",aes(color=RequestSource,fill=RequestSource),alpha=0.7)+
        coord_flip()+
        theme_classic()+
        ylab("Request Volume")+
        xlab("Request Type")+
        scale_y_continuous(breaks=seq(0,550000,30000),
                           labels=seq(0,550000,30000),
                           limits=c(0,580000))
    }
    else{
      
    }
  })
  output$overviewrtp = renderPlot({
    overviewrtp()
  })
  
  #########overviewplotrs##############
  output$overviewrsp = renderPlot({
    ggplot(rs(),aes(x=reorder(RequestSource,count),y=count))+
      geom_bar(stat="identity",fill="pink")+
      theme_classic()+
      coord_flip()+
      ylab("Request Volume")+
      xlab("Request Source")+
      scale_y_continuous(breaks=seq(0,650000,30000),
                         labels=seq(0,650000,30000),
                         limits=c(0,650000))+
      geom_text(aes(label=count),hjust=-0.1)})
  
  
  ############request channel#######
  
  
  output$callappline<-renderPlot({
    ggplot(apptrend(), aes(month_year, count, group = 1)) + geom_line(color = "#f768a1") + 
      geom_line(data = calltrend(), aes(month_year, count, group = 1), color = "#41b6c4") +
      xlab("Month-Year") + ylab("") + ggtitle("Call vs. App Trend") + theme_light() +
      ylab("Request Volume") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$callappheat<-renderPlot({
    h1 = ggplot(callheat(), aes(x = daynum, y = factor(CreatedHour), fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#dd1c77") + theme_classic() +
      xlab("") + ylab("Hour") + ggtitle("Call") + guides(fill = F)+ 
      scale_x_continuous(breaks = seq(1,7,1),
                         labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
    
    h2 = ggplot(appheat(), aes(x = daynum, y = factor(CreatedHour), fill = count)) +
      geom_tile() +
      scale_x_continuous(breaks = seq(1,7,1),
                         labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")) +
      scale_fill_gradient(low = "white", high = "#41b6c4") +
      theme_classic() +
      xlab("") +
      ylab("") + 
      ggtitle("Mobile App") + 
      guides(fill = F)
    
    
    grid.arrange(h1, h2, nrow = 1, bottom = "Weekday")
    
  })
  
  output$callappover<-renderPlot({
    ggplot(callappover(), aes(x = reorder(RequestType, count), y = count, fill =RequestSource)) +
      geom_bar(stat = "identity", position = "dodge") + 
      ylab("") +
      ggtitle("Request Type by Call vs. Mobile App") + 
      xlab("") + theme_light() +
      guides(fill = guide_legend(title = NULL)) + coord_flip() +
      scale_fill_manual(values = c("pink","#9ecae1"))
  })
  
  
  
  ##############PT ############################
  output$pt1<-renderPlot({
    ##Plot avg request + avg process time
    a = ggplot(dc1(), aes(x = as.factor(CD), y = handle)) +
      geom_bar(stat = "identity", 
               fill = c(rep("pink",7), rep("#9ecae1",3),rep("pink",2), rep("#9ecae1",3))) +
      geom_text(aes(label = handle, vjust = 1.8)) +
      ggtitle("Service Request Received by Council District")+
      xlab("Council District") +
      ylab("Service Request Received per Day") +
      geom_hline(aes(yintercept = mean(handle)), color = "red") +
      geom_text(aes( 0, 151.36, label = 151.36, hjust = -1, vjust = -1),color = "red") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    b1 = ggplot(dc1(), aes(x = as.factor(CD), y = avgproc)) +
      geom_bar(stat = "identity", 
               fill = c(rep("pink"), rep("#9ecae1",2), rep("pink",2), rep("#9ecae1",4),rep("pink",2),rep("#9ecae1",1),rep("pink",2),rep("#9ecae1",1)))+
      geom_text(aes(label = avgproc, vjust = 1.8)) +
      ggtitle("Average Processing Days per Request by Council District")+
      xlab("Council District") +
      ylab("Average Process Days") +
      geom_hline(aes(yintercept = mean(avgproc)), color = "red") +
      geom_text(aes( 0, 4.15, label = 4.15, hjust = -3, vjust = -0.5),color = "red") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(a,b1)})
  
  
  output$pt2<-renderPlot({
    ###plot process days + workload
    b2 = ggplot(dc1(), aes(x = as.factor(CD), y = avgproc)) +
      geom_bar(stat = "identity", 
               fill = c(rep("pink"), rep("#9ecae1",2), rep("pink",2), rep("#9ecae1",4),rep("pink",2),rep("#9ecae1",1),rep("pink",2),rep("#9ecae1",1)),
               color = c("blue",rep("NA",2), rep("red",2), rep("NA",4),  "blue","red","NA",rep("blue",2),rep("NA",1) ) )+
      geom_text(aes(label = avgproc, vjust = 1.5)) +
      ggtitle("Average Processing Days per Request by Council District")+
      xlab("Council District") +
      ylab("Average Process Days") +
      geom_hline(aes(yintercept = mean(avgproc)), color = "red") +
      geom_text(aes( 0, 4.15, label = 4.15, hjust = -3, vjust = -0.5),color = "red") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    c = ggplot(dc1(), aes(x = as.factor(CD), y = workload)) +
      geom_bar(stat = "identity", 
               fill = c(rep("pink",1), rep("#9ecae1",6),rep("pink",3), rep("#9ecae1",2),rep("pink",2),rep("#9ecae1",1))) +
      geom_text(aes(label = workload, vjust = 1.5)) +
      ggtitle("Workload by Council District")+
      xlab("Council District") +
      ylab("Workload per Day") +
      geom_hline(aes(yintercept = mean(workload)), color = "red") +
      geom_text(aes( 0, 623.22, label = 623.22, hjust = -1.7, vjust = -0.5),color = "red") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    grid.arrange(b2,c)
    
  })
  #######PT 0#########
  output$pt0<-renderPlot({
    p1 = processtime() %>%
      ggplot(aes(reorder(RequestType,avg), avg)) + 
      geom_bar(stat = "identity", fill = c(rep("#9ecae1",5), rep("pink", 7)),
               color = c(rep("NA",5), rep("red",2),rep("NA", 5))) + 
      geom_text(aes(label = avg),hjust = 1, fontface = "bold") +
      coord_flip() +
      geom_hline(yintercept = 4.83, color = "red") + xlab("Request Type") + 
      ylab("Average Processing Days") + 
      geom_text(aes(0, 4.83, label = 4.83, vjust = -1, hjust = -0.3), color = "red") +
      theme_light()
    
    p2 = processtime() %>%
      ggplot(aes(RequestType, count)) + 
      geom_bar(stat = "identity",
               fill = c(rep("#9ecae1",5), rep("pink", 7)),
               color = c(rep("NA",5), rep("red",2),rep("NA", 5))) +
      geom_text(aes(label = count),hjust = 0.5, fontface = "bold") +
      coord_flip() + xlab("") + ylab("Request Volume") + theme_light() +
      theme(axis.text.y = element_blank())
    
    grid.arrange(p1, p2, nrow = 1)
    
  })
  
  ###########4
  output$pt3<-renderPlot({
    ggplot(dc3(), aes(x = reorder(RequestType, volume), y = volume, fill = RequestType)) +
      geom_bar(stat = "identity") +
      facet_wrap(~CD, 4,4)+
      coord_flip() +
      xlab("Request Type")+
      ylab("Request Volume") +
      ggtitle("Top 5 Request Type by Council District ") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y=element_blank()) })
  
  
  
  ###############5
  output$pt4<-renderPlot({
    ggplot(dc2(), aes(x = reorder(RequestSource, volume), y = volume, fill = RequestSource)) +
      geom_bar(stat = "identity") +
      facet_wrap(~CD, 4,4)+
      coord_flip() +
      ylab("Request Volume") +
      xlab("Request Source") +
      ggtitle("Top 5 Request Source by Council District ") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y=element_blank())})
  
  
  #############season/ season RT###############
  seasonrtp = reactive({
    if(input$seasonplotrt == "monthrt") {
      ggplot(monthrt(),aes(x=monnum,y=RequestType,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black")+
        theme_classic()+
        xlab("") +
        ylab("")+
        scale_x_continuous(breaks = seq(1,12,1),
                           labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        theme_classic()
      
    } else if(input$seasonplotrt == "wdayrt") {
      ggplot(wdayrt(),aes(x=daynum,y=RequestType,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black")+
        xlab("")+
        ylab("")+
        scale_x_continuous(breaks = seq(1,7,1),
                           labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))+
        theme_classic() 
      
    } else { ## hourrt
      ggplot(hourrt(),aes(x=RequestType,y=hour,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black") +
        theme_classic() +
        xlab("")+
        ylab("Hour")+
        coord_flip()
    }})
  
  output$seasonrtp = renderPlot({
    seasonrtp()
  })
  
  #############season/ season RS ###############
  seasonrsp = reactive({
    if(input$seasonplotrs == "monthrs") {
      ggplot(monthrs(),aes(x=monnum,y=RequestSource,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black")+
        theme_classic()+
        xlab("") +
        ylab("")+
        scale_x_continuous(breaks = seq(1,12,1),
                           labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        theme_classic()
    } else if(input$seasonplotrs == "wdayrs") {
      ggplot(wdayrs(),aes(x=daynum,y=RequestSource,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black")+
        ylab("")+
        xlab("")+
        theme_classic()+
        scale_x_continuous(breaks = seq(1,7,1),
                           labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      
      
    } else { ## hourrs
      
      ggplot(hourrs(),aes(x=hour,y=RequestSource,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black")+
        ylab("")+
        theme_classic() 
      
    }})
  
  output$seasonrsp = renderPlot({
    seasonrsp()
  })
  
  
  #############season/ time RT###############
  timertp = reactive({
    if(input$timert == "monwdayrt") {
      ggplot(monwdayrt(),aes(x=monnum,y=daynum,fill=count))+
        geom_tile()+
        xlab("")+
        ylab("")+
        scale_x_continuous(breaks = seq(1,12,1),
                           labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        scale_y_continuous(breaks = seq(1,7,1),
                           labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))+
        scale_fill_gradient(low="white",high="black")+
        theme_classic()
      
    } else if(input$timert == "wdayhrrt") {
      ggplot(wdayhrrt(),aes(x=daynum,y=hour,fill=count))+
        geom_tile()+
        scale_fill_gradient(low="white",high="black") +
        ylab("Hour")+
        xlab("")+
        scale_x_continuous(breaks = seq(1,7,1),
                           labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))+
        theme_classic()
    } else { ## monthhrrt
      ggplot(monthhrrt(),aes(x=monnum,y=hour,fill=count))+
        geom_tile()+
        xlab("")+
        ylab("Hour")+
        scale_x_continuous(breaks = seq(1,12,1),
                           labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        scale_fill_gradient(low="white",high="black")+
        theme_classic()
      
    }})
  
  output$timertp = renderPlot({
    timertp()
  })
  #############season/ time RS###############
  timersp = reactive({
    if(input$timers == "monwdayrs") {
      
    } else if(input$timers == "wdayhrrs") {
      
    } else { ## monhrrs
    }
  })
  
  output$timersp = renderPlot({
    timersp()
  })
  #############geo/ cd RT###############
  output$geocdrtp<-renderPlot({
    ggplot(geocdrt(),aes(reorder(RequestType,count),count))+
      geom_bar(stat="identity",aes(color=RequestType,fill=RequestType))+
      facet_wrap(~CD)+
      coord_flip()+
      theme_classic()+
      xlab("")+
      ylab("Request Volume")+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  #############geo/ cd RS###############
  output$geocdrsp<-renderPlot({
    ggplot(geocdrs(),aes(x=reorder(RequestSource,count),y=count))+
      geom_bar(stat="identity",aes(color=RequestSource,fill=RequestSource))+
      facet_wrap(~CD)+
      coord_flip()+
      theme_classic()+
      xlab("")+
      ylab("Request Volume")+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  })
  
  ############geo/ demo RT##############
  
  output$incomert <-renderPlot({
    ggplot(incomert(),aes(x=total.income,y=count,group=RequestType))+
      geom_point(aes(color=RequestType),alpha=0.7)+
      xlab("Median Income ")+
      ylab("Request Volume")+
      theme_classic()
  })
  output$racert<-renderPlot({
    racert() %>%
      filter(RequestType %in% c("Bulky Items", "Graffiti Removal", "Metal/Household Appliances",
                                "Illegal Dumping Pickup", "Electronic Waste",
                                "Dead Animal Removal")) %>%
      ggplot(aes(x = total.income, y = count, color = race)) +
      geom_point(size = 2, alpha = 0.8) +
      facet_wrap(~RequestType) + xlab("Median Income") + ylab("Request Volume") +
      ggtitle("Top 6 Request Types Distribution by Most Representative Race") + theme_light() +
      theme(legend.title=element_blank()) +
      scale_color_manual(values = c("#f768a1", "#feb24c", "#41b6c4", "#8856a7"))
    
  })
  
  output$lamap<-renderPlot({
    m1 = ggplot(lap(), aes(x = long, y = lat, fill = count, group = group)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "white", high = "#dd1c77", guide = F) + theme_light() +
      ggtitle("Tracking Data  Request Distribution by Zip Code") + xlab("")
    
    m2 = ggplot(lab(), aes(x = long, y = lat, fill = count, group = group)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "white", high = "#43a2ca", guide = F) + theme_light() +
      ggtitle("Service Request Distribution by Zip Code") + xlab("") + ylab("")
    
    grid.arrange(m1, m2, nrow = 1, bottom = "long")
    
  })
  
}


shinyApp(ui,server)




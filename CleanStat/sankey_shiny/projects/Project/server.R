#SERVICE PAGE OF SHINY APP

#loading libraries
library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(stringr)
library(gridExtra)
library(wordcloud)

shinyServer(function(input, output) {

  #loading the rda file with the cleaned, transformed datasets
  load("FinalData1.rda")
 
## -----------------------------------    
## PANEL: CURRENT TRENDS
## -----------------------------------  

# BUTTON (OVERALL TREND) --------------------------------------
  
  #subsetting data to include only top 6 request types
  Year2016_1 = reactive({
    servicerequest %>%
      filter(RequestSource %in% c("Call", "Mobile App", "Driver Self Report", "Email", "Self Service")) %>%
      filter(Year == 2016)})
  
  #plotting distribution of calls in 2016
  observeEvent(input$button1, {
    output$p2_1 = renderPlot({
      ggplot(Year2016_1(), aes(x = RequestType, fill = RequestSource)) +
        geom_bar(position = position_dodge()) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        facet_wrap(~Month) +
        ylab("") +
        xlab("") +
        ggtitle("Distribution of Calls in 2016")})
  }) #END OF BUTTON
  
# SLIDER EVENT (CHOOSE MONTH) --------------------------------------  
  observeEvent(input$slider1, {
    
    Year2016 = reactive({
      servicerequest %>%
        filter(RequestSource %in% c("Call", "Mobile App", "Driver Self Report", "Email", "Self Service")) %>%
        filter(Year == 2016) %>%
        filter(Month == input$slider1)})
    
    output$p2_1 = renderPlot({
      ggplot(Year2016(), aes(x = RequestType, fill = RequestSource)) +
        geom_bar(position = position_dodge()) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        ylab("") +
        xlab("") +
        ggtitle(paste("Distribution of calls in Month", isolate(input$slider1)))})
    
  }) #END OF SLIDER
  
  #-----------------------------
  #HEAT MAPS
  #-----------------------------
  
  #OVERALL
  
  call <- servicerequest %>%
    filter(RequestSource == "Call")
  
  mobile <- servicerequest %>%
    filter(RequestSource == "Mobile App")
  
  email <- servicerequest %>%
    filter(RequestSource == "Email")
  
  driver <- servicerequest %>%
    filter(RequestSource == "Driver Self Report")
  
  self <- servicerequest %>%
    filter(RequestSource == "Self Service")
  
  heatmap <- servicerequest %>%
    group_by(Day, Hour) %>%
    summarise(count = n())
  
  
  observeEvent(input$select, {
    
    if (input$select == 0) {
      output$p2_3 = renderPlot({
        ggplot(heatmap, aes(x = Day,
                            y = factor(Hour),
                            fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "darkred") +
          ylab("Hour of the Day") +
          ggtitle("Heatmap - Week Day vs Hour (All Records)") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())})
    }else if(input$select == 1){
      output$p2_3 = renderPlot({
        
        call %>%
          group_by(RequestType) %>%
          summarise(n()) %>%
          arrange(desc(`n()`))
        
        call_bulkyitems <- call %>%
          filter(RequestType == "Bulky Items") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c1 <- ggplot(call_bulkyitems, aes(x = Day,
                                          y = factor(Hour),
                                          fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Bulky Items") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        call_grafitti <- call %>%
          filter(RequestType == "Graffiti Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c2 <- ggplot(call_grafitti, aes(x = Day,
                                        y = factor(Hour),
                                        fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Grafitti Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        call_metal <- call %>%
          filter(RequestType == "Metal/Household Appliances") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c3 <- ggplot(call_metal, aes(x = Day,
                                     y = factor(Hour),
                                     fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Metal/Household Appliances") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        call_dumping <- call %>%
          filter(RequestType == "Illegal Dumping Pickup") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c4 <- ggplot(call_dumping, aes(x = Day,
                                       y = factor(Hour),
                                       fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Illegal Dumping Pickup") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        
        call_waste <- call %>%
          filter(RequestType == "Electronic Waste") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c5 <- ggplot(call_waste, aes(x = Day,
                                     y = factor(Hour),
                                     fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Electronic Waste") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        call_animal <- call %>%
          filter(RequestType == "Dead Animal Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        c6 <- ggplot(call_animal, aes(x = Day,
                                      y = factor(Hour),
                                      fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Dead Animal Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        grid.arrange(c1, c2, c3, c4, c5, c6, ncol = 3,
                     top = "Heatmap - Week Day vs Hour (Various Request Types)")
        
        
        })
    }else if(input$select == 2){
      
      output$p2_3 = renderPlot({
        
        mobile %>%
          group_by(RequestType) %>%
          summarise(n()) %>%
          arrange(desc(`n()`))
        
        mobile_bulkyitems <- mobile %>%
          filter(RequestType == "Bulky Items") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m1 <- ggplot(mobile_bulkyitems, aes(x = Day,
                                            y = factor(Hour),
                                            fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Bulky Items") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        mobile_grafitti <- mobile %>%
          filter(RequestType == "Graffiti Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m2 <- ggplot(mobile_grafitti, aes(x = Day,
                                          y = factor(Hour),
                                          fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Grafitti Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        mobile_metal <- mobile %>%
          filter(RequestType == "Metal/Household Appliances") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m3 <- ggplot(mobile_metal, aes(x = Day,
                                       y = factor(Hour),
                                       fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Metal/Household Appliances") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        mobile_dumping <- mobile %>%
          filter(RequestType == "Illegal Dumping Pickup") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m4 <- ggplot(mobile_dumping, aes(x = Day,
                                         y = factor(Hour),
                                         fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Illegal Dumping Pickup") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        
        mobile_waste <- mobile %>%
          filter(RequestType == "Electronic Waste") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m5 <- ggplot(mobile_waste, aes(x = Day,
                                       y = factor(Hour),
                                       fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Electronic Waste") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        mobile_homeless <- mobile %>%
          filter(RequestType == "Homeless Encampment") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        m6 <- ggplot(mobile_homeless, aes(x = Day,
                                          y = factor(Hour),
                                          fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Homeless Encampment") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        grid.arrange(m1, m2, m4, m6, m5, m3, ncol = 3,
                     top = "Heatmap - Week Day vs Hour (Various Request Types)")
        
        
      })
      
    }else if(input$select == 3){
      
      output$p2_3 = renderPlot({
        #Email
        
        email %>%
          group_by(RequestType) %>%
          summarise(n()) %>%
          arrange(desc(`n()`))
        
        email_bulkyitems <- email %>%
          filter(RequestType == "Bulky Items") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e1 <- ggplot(email_bulkyitems, aes(x = Day,
                                           y = factor(Hour),
                                           fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Bulky Items") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        email_grafitti <- email %>%
          filter(RequestType == "Graffiti Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e2 <- ggplot(email_grafitti, aes(x = Day,
                                         y = factor(Hour),
                                         fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Grafitti Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        email_metal <- email %>%
          filter(RequestType == "Metal/Household Appliances") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e3 <- ggplot(email_metal, aes(x = Day,
                                      y = factor(Hour),
                                      fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Metal/Household Appliances") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        email_dumping <- email %>%
          filter(RequestType == "Illegal Dumping Pickup") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e4 <- ggplot(email_dumping, aes(x = Day,
                                        y = factor(Hour),
                                        fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Illegal Dumping Pickup") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        
        email_light <- email %>%
          filter(RequestType == "Single Streetlight Issue") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e5 <- ggplot(email_light, aes(x = Day,
                                      y = factor(Hour),
                                      fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Single Streetlight Issue") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        email_waste <- self %>%
          filter(RequestType == "Electronic Waste") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        e6 <- ggplot(email_waste, aes(x = Day,
                                      y = factor(Hour),
                                      fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Electronic Waste") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        grid.arrange(e1,e2,e4,e5,e3,e6, ncol = 3,
                     top = "Heatmap - Week Day vs Hour (Various Request Types)")
        
      })
      
    }else if(input$select == 4){
      
      output$p2_3 = renderPlot({
        #Driver Self Report
        
        driver %>%
          group_by(RequestType) %>%
          summarise(n()) %>%
          arrange(desc(`n()`))
        
        driver_bulkyitems <- driver %>%
          filter(RequestType == "Bulky Items") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        d1 <- ggplot(driver_bulkyitems, aes(x = Day,
                                            y = factor(Hour),
                                            fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Bulky Items") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        driver_grafitti <- driver %>%
          filter(RequestType == "Graffiti Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        d2 <- ggplot(driver_grafitti, aes(x = Day,
                                          y = factor(Hour),
                                          fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          ggtitle("Grafitti Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        driver_dumping <- driver %>%
          filter(RequestType == "Illegal Dumping Pickup") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
    
        
        grid.arrange(d2,d1, ncol = 2,
                     top = "Heatmap - Week Day vs Hour (Various Request Types)")
        
      })
      
    }else if(input$select == 5){
      
      output$p2_3 = renderPlot({
        #Self Service
        
        self %>%
          group_by(RequestType) %>%
          summarise(n()) %>%
          arrange(desc(`n()`))
        
        self_bulkyitems <- self %>%
          filter(RequestType == "Bulky Items") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s1 <- ggplot(self_bulkyitems, aes(x = Day,
                                          y = factor(Hour),
                                          fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Bulky Items") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        self_grafitti <- self %>%
          filter(RequestType == "Graffiti Removal") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s2 <- ggplot(self_grafitti, aes(x = Day,
                                        y = factor(Hour),
                                        fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Grafitti Removal") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        self_metal <- self %>%
          filter(RequestType == "Metal/Household Appliances") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s3 <- ggplot(self_metal, aes(x = Day,
                                     y = factor(Hour),
                                     fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Metal/Household Appliances") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        self_dumping <- self %>%
          filter(RequestType == "Illegal Dumping Pickup") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s4 <- ggplot(self_dumping, aes(x = Day,
                                       y = factor(Hour),
                                       fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Illegal Dumping Pickup") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        
        self_light <- self %>%
          filter(RequestType == "Single Streetlight Issue") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s5 <- ggplot(self_light, aes(x = Day,
                                     y = factor(Hour),
                                     fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Single Streetlight Issue") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        self_homeless <- self %>%
          filter(RequestType == "Homeless Encampment") %>%
          group_by(Day, Hour) %>%
          summarise(count = n())
        
        s6 <- ggplot(self_homeless, aes(x = Day,
                                        y = factor(Hour),
                                        fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          ylab("Hour of the Day") +
          xlab("") +
          ggtitle("Homeless Encampment") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.line = element_blank())
        
        grid.arrange(s2, s1, s5, s4, s6, s3, ncol = 3,
                     top = "Heatmap - Week Day vs Hour (Various Request Types)")
        
      })
      
    }
    
      
    })

  
  #-----------------------------------------
  #TOP 6 REQUEST TYPES BY REQUEST SOURCE
  #-----------------------------------------

  #loading package to process images
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(jpeg, png, ggplot2, grid, neuropsychology)
  
  
  #subsetting dataset
  bulky <- servicerequest %>%
    filter(RequestType == "Bulky Items") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:5)
  
  graffiti <- servicerequest %>%
    filter(RequestType == "Graffiti Removal") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:5)
  
  metal <- servicerequest %>%
    filter(RequestType == "Metal/Household Appliances") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:4)
  
  dumping <- servicerequest %>%
    filter(RequestType == "Illegal Dumping Pickup") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:5)
  
  waste <- servicerequest %>%
    filter(RequestType == "Electronic Waste") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:4)
  
  animal <- servicerequest %>%
    filter(RequestType == "Dead Animal Removal") %>%
    group_by(RequestSource) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:5)
  
  
  #loading images for background
  image_graffiti <- jpeg::readJPEG("Picture1.jpg")
  image_bulky <- jpeg::readJPEG("Picture2.jpg")
  image_dumping <- jpeg::readJPEG("Picture3.jpg")
  image_metal <- jpeg::readJPEG("Picture4.jpg")
  image_waste <- jpeg::readJPEG("Picture5.jpg")
  image_animal <- jpeg::readJPEG("Picture6.jpg")
  
  
  #plots
  observeEvent(input$select_0, {
    
    output$p2_2 = renderPlot({
      
      
      if (input$select_0 == 0) {
        output$p2_2 = renderPlot({
          ggplot(bulky, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_bulky,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "pink", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Bulky Item Request") +
            scale_y_continuous(breaks = seq(0, 406000, 50000)) +
            geom_text(aes(label = count), vjust = -1)
          
        })
      }else if(input$select_0 == 1){
        output$p2_2 = renderPlot({
          
          #graffiti
          ggplot(graffiti, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_graffiti,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "lightyellow", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Graffiti Removal Request") +
            scale_y_continuous(breaks = seq(0, 160000, 10000)) +
            geom_text(aes(label = count), vjust = -1)
          
        })
      }else if(input$select_0 == 2){
        
        output$p2_2 = renderPlot({
          #animal removal
          ggplot(animal, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_animal,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "#8B7355", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Dead Animal Removal Request") +
            scale_y_continuous(breaks = seq(0, 30000, 5000)) +
            geom_text(aes(label = count), vjust = -1)
          
          
        })
        
      }else if(input$select_0 == 3){
        
        output$p2_2 = renderPlot({
          
          #dumping
          ggplot(dumping, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_dumping,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Illegal Dumping Request") +
            scale_y_continuous(breaks = seq(0, 50000, 5000)) +
            geom_text(aes(label = count), vjust = -1)
          
          
        })
        
      }else if(input$select_0 == 4){
        
        output$p2_2 = renderPlot({
          
          #metal
          ggplot(metal, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_metal,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "#00688B", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Metal/Household Appliances") +
            scale_y_continuous(breaks = seq(0, 70000, 5000)) +
            geom_text(aes(label = count), vjust = -1)
        })
        
      }else if(input$select_0 == 5){
        
        output$p2_2 = renderPlot({
          
          #electronic waste
          ggplot(waste, aes(x = reorder(RequestSource, desc(count)), y = count)) +
            annotation_custom(rasterGrob(image_waste,
                                         width = unit(1,"npc"),
                                         height = unit(1,"npc")),
                              -Inf, Inf, -Inf, Inf) +
            geom_bar(stat = "identity", fill = "darkslategray4", width = 0.5) +
            xlab("Request Source") +
            ylab("Number of Calls") +
            ggtitle("Top Request Sources for Electronic Waster Request") +
            scale_y_continuous(breaks = seq(0, 70000, 5000)) +
            geom_text(aes(label = count), vjust = -1)
          
          
        })
        
      }
      
    })
    
    
    
  })


### BOXPLOTS (to show median time taken to service a request)
  
  #new variable created to measure time difference between UpdatedDate and CreatedDate
  servicerequest1 <- servicerequest %>%
    mutate(TimeDiff = round(difftime(servicerequest$UpdatedDate, servicerequest$CreatedDate, units = "mins"),2))
  
  servicerequest1$TimeDiff = round((servicerequest1$TimeDiff)/(24*60),2)

#plot  
  output$p2_4 = renderPlot({
    servicerequest1 %>%
      group_by(RequestType) %>%
      ggplot(aes(x = RequestType, y = as.numeric(TimeDiff))) +
      geom_boxplot(color = "black", fill = "lightblue") +
      coord_cartesian(ylim = c(0,40)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      xlab("Request Types") +
      ylab("Time (Days)")
  })
  
  
  
  #-------------------------------------------------------------
  #                 HISTORICAL DATA - PANEL
  #-------------------------------------------------------------
  
  #subsetting data
  count_dept = data1 %>%
    group_by(Department.Abbreviation) %>%
    summarise(count = n()) %>%
    filter(Department.Abbreviation != "") %>%
    arrange(desc(count))
  
  #plot
  output$p1_1 = renderPlot({
    count_dept %>%
      filter(count>10000) %>%
      ggplot(aes(x = reorder(Department.Abbreviation, desc(count)), y = count)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_classic() +
      xlab("Department") +
      ylab("Number of Calls") +
      ggtitle("Call Requests by Department")})
  
  #plot
  output$p1_2 = renderPlot({
      ggplot(change1, aes(x = reorder(Call.Resolution, percent), y = percent*100, fill = colorcode)) +
      geom_bar(stat = "identity") +
      facet_wrap(~Year) +
      geom_text(aes(label = percent*100, hjust = 0), size = 3) +
      ylab("% of the total") +
      ggtitle("Percentage of Call Resolution types (2011-2014)") +
      coord_flip() +
      scale_fill_manual(name = "Changing Trend", values = c("lightblue", "blue", "red"))})
  
  
  count_dept_year = data1 %>%
    group_by(Department.Name, Year, Department.Abbreviation) %>%
    filter(Department.Abbreviation %in% c("LADBS", "BOS", "LAPD", "BPW", "DOT", "BSS")) %>%
    filter(Year != 2015) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  output$p1_3 = renderPlot({
    count_dept_year %>%
      ggplot(aes(x = reorder(Department.Abbreviation, desc(count)), y = count, fill = factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.50) +
      theme_classic() +
      xlab("Department") +
      ylab("Number of Calls") +
      ggtitle("Top Department Names with respect to Number of Calls") +
      scale_y_continuous(breaks = seq(0, 390000, 50000)) +
      guides(fill = guide_legend(title = "Year"))
  })


  referData <- subset(data1, data1$Call.Resolution %in% c('Referred To 411', 'Referred To County', 'Referred To Other Governmental', 'Referred To State'))
  referData$ResolveRefer = 'Referred' 
  resolveData <- subset(data1, data1$Call.Resolution %in% c('Gave Caller Information', 'Service Request Processed'))
  resolveData$ResolveRefer = 'Resolved'
  escalateData <- subset(data1, data1$Call.Resolution %in% c('Escalate To Supervisor', 'Escalated To Office of Finance'))
  escalateData$ResolveRefer = 'Escalated'
  transferData <- subset(data1, data1$Call.Resolution %in% c('Transfer (City)', 'Transferred To 411', 'Warm Transfer (City)'))
  transferData$ResolveRefer = 'Transferred'
  blankData <- subset(data1,data1$Call.Resolution %in% c('Caller Hung Up', 'Static/Ghost Call', 'N/A'))
  blankData$ResolveRefer = 'Blank'
  failData <- subset(data1, data1$Call.Resolution %in% c('Got Voicemail (City)', 'Info Not Available (Non-City)', 'Line Busy (City)'))
  failData$ResolveRefer = 'Failed'
  newDataBind <- rbind(referData,resolveData,escalateData,transferData,blankData, failData)
  
  newDataBind <- subset(newDataBind, Year < 2015)
  
  newDataBindSum <- newDataBind %>%
    group_by(ResolveRefer, Year, Month) %>%
    summarise(count = n())
  
  #resolveData$Date <- mdy(resolveData$Date)
  resolveData$Month <- month(resolveData$Date, label = T)
  #resolveData$Year <- year(resolveData$Date)

  resolveData <- subset(resolveData, Year < 2015)
  
  newResolveSum <- resolveData %>%
    group_by(ResolveRefer, Year, Month) %>%
    summarise(count = n())

  
  output$p1_4 = renderPlot({
    ggplot(newDataBindSum, aes(x = Year, y = count*100/sum(count), fill = ResolveRefer)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      theme_dark() +
      xlab("Year") +
      ylab("Percentage of Calls") +
      ggtitle("Percentage of Calls per year")
    })
  
  #newDataBind$Date <- mdy(newDataBind$Date)
  #newDataBind$Month <- month(newDataBind$Date, label = T)
  #newDataBind$Year <- year(newDataBind$Date)

  data1$Month = month(data1$Date, label = T)
  
  monthData <- data1 %>%
    filter(Month != 'NA') %>%
    group_by(Year, Month) %>%
    filter(Year != "2015") %>%
    summarise(count = n())
  
  output$p1_6 = renderPlot({
    ggplot(monthData, aes(x = Month, y = count, group = Year, color = factor(Year))) +
      geom_line() +
      scale_color_manual(values = c("blue", "red", "green", "yellow", "black")) +
      xlab("Month") +
      ylab("Number of Calls") +
      ggtitle("Call Distribution by Month and Year")
  })
  
  
  output$p1_5 = renderPlot({
    ggplot(newResolveSum, aes(x = Year, y = count*100/sum(count))) +
      geom_bar(stat = "identity", fill = "turquoise") +
      facet_wrap(~Month, ncol = 3) +
      theme_dark() +
      xlab("Year") +
      ylab("Percentage of Calls") +
      ggtitle("Percentage of Resolved Calls")
  })
  

# ------------------------------------
#       GEOGRAPHIC TRENDS
# ------------------------------------
  
  #loading map
  map = qmap("Los Angeles", maptype = "roadmap")
  
  top_bulky <- servicerequest %>%
    filter(ZipCode %in% c("91331", "90011", "90026", "91342",
                          "90044", "90003", "90042", "90019", "90037", "91335"))
  
  #plotting points over LA map
  t1 <- map +
    stat_bin2d(data = top_bulky, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Bulky Items")
  
  top_graffiti <- servicerequest %>%
    filter(ZipCode %in% c("90011", "90037", "90026", "90033",
                          "90003", "90744", "90007", "90023", "90731", "90006"))
  t2 <- map +
    stat_bin2d(data = top_graffiti, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Graffiti Removal")
  
  top_metal <- servicerequest %>%
    filter(ZipCode %in% c("90731", "91342", "90026", "91331",
                          "90019", "90042", "90011", "91344", "90065", "90044"))
  t3 <-map +
    stat_bin2d(data = top_metal, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Metal/Household Appliances")
  
  
  top_dumping <- servicerequest %>%
    filter(ZipCode %in% c("90003", "90026", "90011", "90044",
                          "91331", "90037", "90731", "90018", "90016", "91335"))
  t4 <- map +
    stat_bin2d(data = top_dumping, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Illegal Dumping")
  
  top_waste <- servicerequest %>%
    filter(ZipCode %in% c("91331", "91342", "90011", "91335",
                          "90042", "90026", "90003", "91402", "90731", "90044"))
  t5 <- map +
    stat_bin2d(data = top_waste, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Electronic Waste") 
  
  top_animal <- servicerequest %>%
    filter(ZipCode %in% c("90011", "91331", "90003", "90044",
                          "91342", "90047", "90037", "91335", "90018", "91406"))
  t6 <- map +
    stat_bin2d(data = top_animal, aes(x = Longitude, y = Latitude),
               bins = 100, alpha = 0.5) +
    scale_fill_continuous(low = "black", high = "darkblue") +
    ggtitle("Dead Animal Removal")
  
  
  
  output$p3_1 = renderPlot({
  
    grid.arrange(t1,t2,t3,t4,t5,t6, ncol = 3)

  })

  #COUNCIL DISTRICT - GEOGRAPHIC TRENDS
  
  #plottong points over map
  cd1 <- map +
    geom_point(data = cd_1, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 1")
  
  cd2 <- map +
    geom_point(data = cd_2, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 2")
  
  cd3 <- map +
    geom_point(data = cd_3, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 3")
  
  cd4 <- map +
    geom_point(data = cd_4, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 4")
  
  cd5 <- map +
    geom_point(data = cd_5, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 5")
  
  cd6 <- map +
    geom_point(data = cd_6, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 6")
  
  cd7 <- map +
    geom_point(data = cd_7, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 7")
  
  cd8 <- map +
    geom_point(data = cd_8, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 8")
  
  cd9 <- map +
    geom_point(data = cd_9, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 9")
  
  cd10 <- map +
    geom_point(data = cd_10, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 10")
  
  cd11 <- map +
    geom_point(data = cd_11, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 11")
  
  cd12 <- map +
    geom_point(data = cd_12, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 12")
  
  cd13 <- map +
    geom_point(data = cd_13, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 13")
  
  cd14 <- map +
    geom_point(data = cd_14, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 14")
  
  cd15 <- map +
    geom_point(data = cd_15, aes(x = Longitude, y = Latitude),
               color = "blue", alpha = 0.01) +
    ggtitle("Council District 15")
  
  
  output$p3_3 = renderPlot({
    grid.arrange(cd1, cd2, cd3, cd4, cd5,
                 cd6, cd7, cd8, cd9, cd10,
                 cd11, cd12, cd13, cd14, cd15,
                 nrow = 3)

    
    cdData <- servicerequest %>%
      group_by(CD, RequestType) %>%
      summarise(count = n()) %>%
      arrange(CD, -count)
    
    #filtering for top 6 request types
    cdDataFilter <- subset(cdData, cdData$RequestType %in% c('Bulky Items', 'Graffiti Removal', 'Metal/Household Appliances',
                                                             'Illegal Dumping Pickup', 'Electronic Waste', 'Dead Animal Removal')  & CD!='NA')
    
    #plot
    output$p3_2 = renderPlot({
      ggplot(cdDataFilter, aes(x = factor(CD), y = count, fill = factor(CD))) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~RequestType, ncol = 3) +
        xlab("Council District") +
        ylab("Number of Requests") +
        theme_classic()
    })
  
    
  })
  
  
  ## -----------------------------------    
  ## MAIN PAGE
  ## -----------------------------------    
  
  #wordcloud #1
  
  #  y <- data1 %>%
  #   group_by(Department.Abbreviation) %>%
  #  summarise(count = n()) %>%
  #  filter(Department.Abbreviation != "")
  
  #words = y$Department.Abbreviation
  #freq = y$count
  #d <- data.frame(word = words, freq = freq)
  
  #pal <- brewer.pal(9, "Set1")
  #pal <- pal[(1:5)]
  
  
  #output$p0_1 = renderPlot(
  # wordcloud(d$word,d$freq, random.order = FALSE, random.color = FALSE, colors = pal) 
  #)
  
  #wordcloud #2
  
  # Service Name
  #wc1 <- servicerequest %>%
  # group_by(RequestType) %>%
  # summarise(count = n())
  
  #wc1_1 <- data.frame(word = wc1$RequestType, freq = wc1$count)
  
  #pal1 <- brewer.pal(9, "BuPu")
  #pal1 <- pal1[-(1:5)]
  
  #levels(wc1_1$word)
  
  #output$p0_2 = renderPlot(
  # wordcloud(wc1_1$word,wc1_1$freq,colors = pal1, random.color = FALSE)
  #)
  
  # wordcloud #3
  
  #wc2 <- servicerequest %>%
  #  group_by(RequestSource) %>%
  # summarise(count = n())
  
  #wc2_1 <- data.frame(word = wc2$RequestSource, freq = wc2$count)
  
  #pal2 <- brewer.pal(9, "YlGnBu")
  #pal2 <- pal2[-(1:3)]
  
  #levels(wc1_1$word)
  
  #wordcloud(wc2_1$word,wc2_1$freq,colors = pal2, random.color = FALSE)
  
  
  # worldcloud #4
  #wc3 <- servicerequest %>%
  #  group_by(ZipCode) %>%
  # summarise(count = n())
  
  #wc3_1 <- data.frame(word = wc3$ZipCode, freq = wc3$count)
  
  #pal3 <- brewer.pal(9, "PuBuGn")
  #pal3 <- pal3[-(1:3)]
  
  #levels(wc1_1$word)
  
  #wordcloud(wc3_1$word,wc3_1$freq,colors = pal3, random.color = FALSE, scale = c(2, 0.5))
  
  # worldcloud #5
  # Police Precinct
  #wc4 <- servicerequest %>%
  # group_by(PolicePrecinct) %>%
  #summarise(count = n())
  
  #wc4_1 <- data.frame(word = wc4$PolicePrecinct, freq = wc4$count)
  
  #pal4 <- brewer.pal(9, "RdPu")
  #pal4 <- pal[-(4:6)]
  
  
  #wordcloud(wc4_1$word,wc4_1$freq,colors = pal4, random.color = FALSE, scale = c(1,0.5))
  
  # worldcloud #6
  # CALL RESOLUTION
  
  #wc5 <- data1 %>%
  #group_by(Call.Resolution) %>%
  #summarise(count = n())
  
  #wc5_1 <- data.frame(word = wc5$Call.Resolution, freq = wc5$count)
  
  #display.brewer.pal(9, "RdYlBu")
  #pal5 <- brewer.pal(9, "RdYlBu")
  #pal5 <- pal[-(5:6)]
  
  
  #wordcloud(wc5_1$word,wc5_1$freq,colors = pal5, random.color = FALSE, scale = c(1,0.5))
  
  
}) #end of fluid page
  

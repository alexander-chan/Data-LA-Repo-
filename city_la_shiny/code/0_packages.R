# load pacakges
pkgs <- c("shiny", "shinydashboard", "tidyverse", "stringr", 
          "lubridate", "plotly", "leaflet", "googleVis", "zoo",
          "dplyr", "ggmap", "readr","RSocrata","gsheet", "reshape2",
         "leaflet.extras")
sapply(pkgs, require, character.only = TRUE)

#library(shinyjs)
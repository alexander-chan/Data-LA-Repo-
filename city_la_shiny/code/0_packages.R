# load pacakges

#Note, leaflet.extras is not supported on CRAN, need to use installr


pkgs <- c("shiny", "shinydashboard", "tidyverse", "stringr", 
          "lubridate", "plotly", "leaflet", "googleVis", "zoo",
          "dplyr", "ggmap", "readr","RSocrata","gsheet", "reshape2",
         "devtools")
sapply(pkgs, require, character.only = TRUE)
devtools::install_github('bhaskarvk/leaflet.extras')
#library(shinyjs)


# sankey diagram data
q2 <- read.csv("./city_LA_shiny/data/Clean_Streets_Index_Grids_2016_Q2.csv")
q3 <- read.csv("./city_LA_shiny/data/LACityCleanStat_Q32016_Grids.csv")

# issue 4 data
miles_sewage_cleaned <- read_csv('./city_LA_shiny/data/LASAN__Miles_of_Sewer_Cleaned.csv')
#miles_sewage_cleaned <- read.socrata("https://data.lacity.org/A-Livable-and-Sustainable-City/LASAN-Miles-of-Sewer-Cleaned/iyyp-p2fx")

# issue 5 data
sewer_overflow <- read_csv('./city_LA_shiny/data/LASAN___Sanitary_Sewer_Overflows.csv')
#sewer_overflow <- read.socrata("https://data.lacity.org/A-Livable-and-Sustainable-City/LASAN-Sanitary-Sewer-Overflows/cynx-j8d5")

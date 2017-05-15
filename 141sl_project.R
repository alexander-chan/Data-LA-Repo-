library(RSocrata)

#getting cleaner where, dirtier
#cs round = 1 cleanest

clean <- ls.socrata("http://geohub.lacity.org/datasets?q=cleanstat")

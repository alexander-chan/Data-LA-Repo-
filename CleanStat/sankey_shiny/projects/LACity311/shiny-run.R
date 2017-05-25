install.packages('rsconnect')
library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='dso510',
                          token='1761DD5AE3DF63F355BC0499A04E698E',
                          secret='b/+ixLL5PfbSazeuMrIu/L+JpkmH+fU8rlAB87HN')

setwd("C:/Users/Chris/Desktop/DSO-545/Project/shiny2")
runApp()

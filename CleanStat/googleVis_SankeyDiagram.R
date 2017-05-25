#install.packages("googleVis")
df <- data.frame(origin=c(
  rep("Q2 #1",3), rep("Q2 #2",3), rep("Q2#3",3)),
  visit=c(
    rep(c("Q3 #1", "Q3 #2", "Q3 #3"),3)),
  weights=c(
    c(917, 34, 64, 29, 13, 32, 14, 4, 28) ) )

require(googleVis)
plot(
  gvisSankey(df, from="origin", 
             to="visit", weight="Count",
             options=list(
               height=500,
               width=1000,
               sankey="{link:{color:{fill:'lightblue'}}}"
             ))
)
?gvisSankey

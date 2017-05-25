library(magrittr)
q2q3 <- read.csv("merge.csv")
names(q2q3)
q2q31 <- q2q3[,c("NameInt","Q2_CS_Score_Round", "score_round_q3")]

q2 <- read.csv("Clean_Streets_Index_Grids_2016_Q2.csv")
q3 <- read.csv("LACityCleanStat_Q32016_Grids.csv")

names(q2)
names(q3)

q2q3 <- merge(q2, q3, by = "NameInt")
q2q3 <- q2q3[,c("NameInt","Q2_CS_Score_Round", "GridScrRounded")]
q2q3 %>% head

q2q3$q2q3_change <- paste(q2q3$Q2_CS_Score_Round %>% as.character(), q2q3$GridScrRounded %>% as.character(), sep = "")
q2q3 %>% head
transition <- table(q2q3$q2q3_change)
transition

## Network D3

#install.packages("networkD3")
library(networkD3)
nodes <- data.frame(c("Q2 #1's", "Q2 #2's", "Q2 #3's", "Q3 #1's", "Q3 #2's", "Q3 #3's"))
#nodes <- data.frame(c('Q2_1','Q2_2','Q2_3','Q3_1','Q3_2','Q3_3'))
#names(nodes) <- "nodes"
names(nodes) <- "name"
nodes$name <- as.character(nodes$name)
nodes


links <- data.frame(source = c(rep("1", 3), rep("2", 3), rep("3", 3)), target = rep(1:3, 3) )
links$value <- as.integer(transition)
links$source <- as.integer(paste(links$source))
links <- rbind( c(0,1,1015), links)
links

cleanstat <- list(nodes = nodes, links = links)

sankeyNetwork(Links = cleanstat$links, Nodes = cleanstat$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name')

## GoogleVis Sankey
df <- data.frame(W2=c(rep("Q2 #1",3), rep("Q2 #2",3), rep("Q2 #3",3)),
  Q3=c(rep(c("Q3 #1", "Q3 #2", "Q3 #3"),3)),
  weights=c(
    c(917, 34, 64, 29, 13, 32, 14, 4, 28) ) )

require(googleVis)
cleanStat_sankey <- gvisSankey(df, from="Q2", 
                               to="Q3", weight="Count",
                               options=list(
                                 height=500,
                                 width=1000,
                                 caption="Flow of CleanStat Ratings in Los Angeles from 2016 Q2 to Q3",
                                 sankey="{link:{color:{fill:'lightblue'}}}"
                               ))
plot(cleanStat_sankey)
cleanStat_sankey$html

df

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

install.packages("networkD3")
library(networkD3)
nodes <- data.frame(c("Q2 #1's", "Q2 #2's", "Q2 #3's", "Q3 #1's", "Q3 #2's", "Q3 #3's"))
#nodes <- data.frame(c('Q2_1','Q2_2','Q2_3','Q3_1','Q3_2','Q3_3'))
#names(nodes) <- "nodes"
names(nodes) <- "name"
nodes$name <- as.character(nodes$name)
nodes


links <- data.frame(source = c(rep("1", 3), rep("2", 3), rep("3", 3)), target = rep(4:6, 3) )
links$value <- as.integer(transition)
links$source <- as.integer(paste(links$source))
str(links)
links

cleanstat <- list(nodes = nodes, links = links)

sankeyNetwork(Links = cleanstat$links, Nodes = cleanstat$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name')





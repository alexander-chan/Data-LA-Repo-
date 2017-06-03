##############################
# Calculating sankey weights #
##############################

# compute the weights for sankey diagram
q2q3 <- merge(q2, q3, by = "NameInt")
q2q3 <- q2q3[,c("NameInt","Q2_CS_Score_Round", "GridScrRounded")]
q2q3 %>% head

q2q3$q2q3_change <- paste(q2q3$Q2_CS_Score_Round %>% as.character(), q2q3$GridScrRounded %>% as.character(), sep = "")
q2q3 %>% head
transition <- table(q2q3$q2q3_change)
weights_transition <- as.vector(transition)
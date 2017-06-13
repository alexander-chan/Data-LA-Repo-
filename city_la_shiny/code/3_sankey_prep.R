
##############################
# Calculating sankey weights #
##############################

# compute the weights for sankey diagram
q2q3 <- merge(q2, q3, by = "NameInt")
q2q3 <- q2q3[,c("NameInt","Q2_CS_Score_Round", "GridScrRounded")]
names(q4)[3] <- "NameInt"

# subset observations of q4 that match with q2 and q3
q4_subset<- q4[which(unique(q2q3$NameInt) %in% q4$NameInt),c("NameInt", "CS_RoundScore") ]

# merge dataframe
q234 <- merge(q4_subset, q2q3, by = "NameInt", all.x = TRUE)

# remove duplicates
q234 <- q234[which(!base::duplicated(q234$NameInt)),]

# prep for weight counts
q234$q2q3_change <- paste(q234$Q2_CS_Score_Round %>% as.character(), q234$GridScrRounded %>% as.character(), sep = "")
q234$q3q4_change <- paste(q234$GridScrRounded %>% as.character(), q234$CS_RoundScore %>% as.character(), sep = "")

# weight counts from q2 to q3
levs <- levels(as.factor(q234$q2q3_change))
q2q3_weights <-  factor(q234$q2q3_change, levels = c(levs[1], levs[2], levs[3],
                                                     levs[4], levs[5], levs[6], 
                                                     levs[7], "32", levs[8]))  # "32" had 0 counts
transition_q2q3 <- table(q2q3_weights)

# weight counts from q3 to q4
levs <- levels(as.factor(q234$q3q4_change))
q3q4_weights <- factor(q234$q3q4_change, levels = c(levs[1], levs[2], levs[3],
                                                    levs[4], levs[5], "23",
                                                    levs[6], levs[7], levs[8])) # "23" had 0 counts
transition_q3q4 <- table(q3q4_weights)

# convert to vector
weights_transition_q2q3 <- as.vector(transition_q2q3)
weights_transition_q3q4 <- as.vector(transition_q3q4)

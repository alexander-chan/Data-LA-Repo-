LAdata <- read.csv("Data.csv")
library(ggplot2)
library(dplyr)

#Bulky Items
LAdataRequestTypeBulk <- LAdata %>%
  filter(RequestType %in% c("Bulky Items")) %>%
  group_by(Owner_Full) %>%
  summarise(Count = n()) %>%
  mutate(CountforAxis = Count/1000)

ggplot(LAdataRequestTypeBulk, aes(x=reorder(Owner_Full, CountforAxis), y= CountforAxis)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  xlab("Department") +
  ylab("Number of Requests (in thousands)") +
  ggtitle("Distribution of Bulky Item Requests by Department")
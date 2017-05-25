library(lubridate)
library(ggplot)
library(ggmap)
library(dplyr)


### checking the most sources of the data ###
Source <- RequestData %>%
  group_by(RequestSource) %>%
  summarise(count = n())
ggplot(Source, aes(x = reorder(RequestSource, desc(count)), y = count/1000)) +
  geom_bar(stat = "identity", fill = c("#326FA8")) +
  xlab("Request Source") +
  ylab("Count (Unit: K)") +
  ggtitle("Sources of Request Data") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))


### requests by day of week volume ###
ggplot(RequestData, aes(x = dow, fill = RequestType)) +
  geom_bar() +
  scale_fill_discrete(guide = F) +
  xlab("Day of Week") +
  ylab("Count") +
  ggtitle("Number of Requests by Day of Week and Request Type") +
  facet_wrap(~RequestType, ncol = 3) +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), 
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))



### percent of request type of the cancelled requests ###
RequestData %>%
  filter(Status == "Cancelled") %>%
  group_by(RequestType) %>%
  summarise(count = n()) %>%
  mutate("Percent" = count/sum(count)*100) %>%
  ggplot(aes(x = reorder(RequestType, desc(Percent)), y = Percent)) +
  geom_bar(stat = "identity", fill = c("#326FA8")) +
  xlab("Request Type") +
  ylab("Percent (%)") +
  ggtitle("Request Type of Cancelled Requests") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.minor.y = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))


### request status by day of week broken out with facet_wrap ###
RequestData %>%
  group_by(dow, Status) %>%
  ggplot(aes(x = dow, fill = Status)) +
  geom_bar() +
  scale_fill_manual(values = c("#8B6DED", "#FFAE00", "#B6F5AB", "#823953", "#1A6791"), guide = F) +
  facet_wrap(~ Status) +
  xlab("Day of Week") +
  ylab("Count") +
  ggtitle ("Request Status by Day of Week and Status") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(color = "darkgrey", fill = NA),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = c("#E6E6E6")),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), 
        strip.background = element_rect(color = "lightgrey", fill = "white"),
        strip.text = element_text(size = 9, lineheight = 0.3))


library(twitteR)
library(tm)
library(syuzhet)

google_news = read.csv("/Users/pksharma/Desktop/DataMiner.csv")
google_news$Title = as.character(google_news$Title)
google_news$Source = as.character(google_news$Source)
google_news$TIme = dmy(google_news$TIme)
google_news$Text = as.character(google_news$Text)
google_news = mutate(google_news, Month = month(TIme,label = T, abbr = F))
google_news = mutate(google_news, Day = wday(TIme,label = T, abbr = F))
google_news = mutate(google_news, Year = year(TIme))

head(google_news)

google_news$Text=str_replace_all(google_news$Text,"[^[:graph:]]", " ") 

wordCorpus <- Corpus(VectorSource(google_news$Text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

install.packages("wordcloud")
library(wordcloud)


## get the colors

pal = brewer.pal(9, "YlGnBu")
?brewer.pal
display.brewer.pal(9, "YlGnBu")

### Drop the first 4 colors from the palett (not easy to see)

pal = pal[-c(1:4)]

wordcloud(words = wordCorpus, colors = pal)


## Sentiment Analysis
analytics_sentiment = get_nrc_sentiment(google_news$Text)
google_news = cbind(google_news, analytics_sentiment)

sentimentTotals = data.frame(colSums(analytics_sentiment))

## clean the dataframe

sentimentTotals$Sentiments = rownames(sentimentTotals)
colnames(sentimentTotals) = c("Count", "Sentiment")
rownames(sentimentTotals) = NULL

ggplot(sentimentTotals, aes(reorder(Sentiment,Count),Count)) +
  geom_bar(stat = "identity", aes(fill = Sentiment)) +
  theme(legend.position = "none") +
  xlab("Sentiment") + 
  ylab("Total Count")


### Tweets over time


ggplot(google_news, aes(x = factor(Year), fill = positive)) + geom_bar()

ggplot(tweets, aes(x = wday(created,label = TRUE))) + geom_bar()

ggplot(tweets, aes(x = factor(hour(created)))) + geom_bar()


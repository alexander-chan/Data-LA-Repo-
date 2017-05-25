
library(twitteR)
install.packages("twitteR")
install.packages("tm")
library(tm)
install.packages("syuzhet")
library(syuzhet)

library(ggplot2)
library(dplyr)
library(lubridate)

### Authenticate our app

# Get these details from application you created at app.twitter.com

api_key = "sie16JKQ2W4cee5NtZg31dDI9"
api_secret = "mebdb9GUQGosu0yEiImZMD13L9kM1nKpAuCdts3YW8JXgEU3as"
access_token = "798241688791576576-BJC96c0gNHAm9TI5BBmhzkKHcE04ZaQ"
access_token_secret = "jDqv3CrJpj8F79DbiZSXv4TrGFX9mP9uAW0XWinlTyAhV"

## Setting up connection
setup_twitter_oauth(api_key,api_secret,access_token, access_token_secret)


LA311Tweet = searchTwitter("MyLA311",n = 100, since = '2010-01-01')
myLA=userTimeline('myLA311',n=3600)
myLA311.df=twListToDF(LA311Tweet)
tweets = myLA311.df 

### \w represents a word character
### \w+ represents one character or more
### we need the escape \ in order to include \w in the pattern

nohandles = str_replace_all(tweets$text, pattern = "@\\w+",
                            replacement = "") 
# deleting the usernames and saving tweets in nohandles
# nohandles will be a vector


wordCorpus <- Corpus(VectorSource(nohandles)) %>%
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
analytics_sentiment = get_nrc_sentiment(tweets$text)
tweets = cbind(tweets, analytics_sentiment)

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

tweets$created = ymd_hms(tweets$created)
ggplot(tweets, aes(x = month(created,label = TRUE))) + geom_bar()

ggplot(tweets, aes(x = wday(created,label = TRUE))) + geom_bar()

ggplot(tweets, aes(x = factor(hour(created)))) + geom_bar()


require(twitteR)
require(RCurl)
consumer_key <- "KMuO0zebtiZVhfcI7KrNxH9mm"
consumer_secret <- 'dUA3ZB2gElutqAvx3Oj7i9TxSF4ESDtQp7qcf6c3R8g7V1L1N9'
access_token <- '619294871-3DCU7ah1yqJOyLfwKBtFSqgmUOfkCpjv9GbJMoMn'
access_secret<- 'crWdmwhXJ1a4lRqkK0kitVAE6JZSrZBHrFhwwvA39zFmA'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
demonetization_tweets<-searchTwitter("Demonetization", n=500, lang="en") 
demonetization_tweets 
library(tm)
library(wordcloud)
demonetization_tweets<-searchTwitter('Demonetization', since='2016-11-08',
                                     until='2016-12-12', n=100, lang="en",
                                     resultType = "recent")
str(demonetization_tweets)
demonetization_tweets
demonetization_text<-sapply(demonetization_tweets, function(x) x$getText()) 
demonetization_corpus<-Corpus(VectorSource(demonetization_text)) 
demonetization_clean<-tm_map(demonetization_corpus, removePunctuation)  
demonetization_clean<-tm_map(demonetization_clean, content_transformer(tolower))  
demonetization_clean<-tm_map(demonetization_clean, removeWords,c(stopwords("english"),"Demonetization")) 
demonetization_clean<-tm_map(demonetization_clean, removeNumbers)  
# Remove all white space created due to above cleaning 
demonetization_clean<-tm_map(demonetization_clean, stripWhitespace)  
# You may want to remove search words that will be obviously be very frequent 
demonetization_clean<-tm_map(demonetization_clean, removeWords, "demonetization") 
wordcloud(demonetization_clean, random.order=F,max.words=100,colors=rainbow(10))   

#Word Cloud module
#loading up required libraries
library("ROAuth")
library("twitteR")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("stringr") #For emojis and non-Unicode character
#Direct authentication to twitter account
setup_twitter_oauth(cred$consumerKey,cred$consumerSecret,cred$oauthKey,cred$oauthSecret)
#Setting up search string
search.string<-"#demonetization OR demonetization OR #noteban OR note ban"
#Setting up the no of tweets to be retrieved
no.of.tweets<-1500
#Retrieving tweets data
tweets <- searchTwitter(search.string,n=no.of.tweets,lang = 'en')
#get texts of the tweets
tweets.text <- sapply(tweets, function(x) x$getText())
#removing non-Unicode characters
tweets.text<-str_replace_all(tweets.text,"[^[:graph:]]", " ")
#convert all text to lower case
tweets.text <- tolower(tweets.text)
# Replace blank space (“rt”)
tweets.text <- gsub("rt", "", tweets.text)
# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)
# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)
# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)
# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)
#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
#Creating a word cloud of 100 most frequent words
wordcloud(tweets.text.corpus, scale=c(4,0.7),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 100)
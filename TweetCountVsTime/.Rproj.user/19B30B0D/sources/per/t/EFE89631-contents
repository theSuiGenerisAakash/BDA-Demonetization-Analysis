#Tweets over time

#loading up required libraries
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)

#Direct authentication to twitter API
setup_twitter_oauth(cred$consumerKey,cred$consumerSecret,cred$oauthKey,cred$oauthSecret)

#Search string that species what we're looking for in a tweet
search.string<-'#demonetization OR demonetization OR note ban OR #noteban'

#Retrieve as many tweets as possible
tweets<-searchTwitteR(search.string,n = 1500)

#Tweets vs hour of the day
#Extract the 'created' field of S4 tweet object for all tweets retrieved
tweets.time<-sapply(tweets,function(x)x$created)
#Map the timestamp to only its hour value and overwrite the same list
tweets.time<-sapply(tweets.time,function(x)as.POSIXlt(x,origin="1970-01-01")$hour)
#plot tweets vs hour of the day
qplot(tweets.time,
geom="histogram",
binwidth = 0.5,
main = "Histogram for no of tweets per hour",
xlab = "Hour of the day", ylab = "No of tweets",
fill=I("blue"),
alpha=I(.5),
xlim=c(0,23))

#==========================================================
#Retweets vs hour of the day

#list of retweets' "created"(double value)
tweets.rt.time<-invisible(sapply(tweets, function(x)if(x$isRetweet){x$created}))
#convert it to POSIX timestamps and extract the hour
tweets.rt.time<-sapply(tweets.rt.time,function(x)as.POSIXlt(x[[1]],origin='1970-01-01')$hour)
#creating a copy variable for redundancy
tweets.rt.time.copy<-lapply(tweeets.rt.time,function(x)x)
tweets.rt.time.copy<-as.numeric(unlist(tweets.rt.time.copy))
#Create the plot retweets vs hour of the day
qplot(tweets.rt.time,
geom="histogram",
binwidth = 0.5,
main = "Histogram for no of re-tweets per hour",
xlab = "Hour of the day", ylab = "No of re-tweets",
fill=I("red"),
alpha=I(.5),
xlim=c(0,24))

#============================================================
#Retweets vs minute of the hour

#Reintializing tweets.rt.time
tweets.rt.time.min<-tweets.rt.time.copy
#convert it to POSIX timestamps and extract the minute
tweets.rt.time.min<-sapply(tweets.rt.time.min,function(x)as.POSIXlt(x,origin="1970-01-01")$min)

#Create the plot retweets vs minute of the hour
qplot(tweets.rt.time.min,
geom="histogram",
binwidth = 0.4,
main = "Histogram for no of re-tweets per minute",
xlab = "Minute of the hour", ylab = "No of re-tweets",
fill=I("green"),
alpha=I(.7),
xlim=c(0,59))

#=============================================================
#Tweet popularity
#Popularity = tweet$retweets + tweets$favouritedCount

#create a list of the popularity value
tweets.popularity<-sapply(tweets,function(x)x$retweetCount+x$favoriteCount)
#create a list of corresponding tweet ids
tweets.id<-sapply(tweets,function(x)x$id)

#create a dataframe of ids lined up against corresponding popularity count
df.popularity<-data.frame(tweets.id,tweets.popularity)

#sort the data frame in the descending order on tweets.popularity
df.popularity<-arrange(df.popularity,desc(tweets.popularity))

#lookup the tweet with a tweet id from the reordered tweet-id list and store it
i=1
tweets.popular<-list()
for(q in df.popularity[,c("tweets.id")] ){
tweets.popular[i]<-unlist(lapply(tweets,function(x)if(x$id==q)x))
i<-i+1
}





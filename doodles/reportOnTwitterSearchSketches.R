require(twitteR)
#The original example used the twitteR library to pull in a user stream
#rdmTweets <- userTimeline("psychemedia", n=100)
#Instead, I'm going to pull in a search around a hashtag.
searchTerm='#ukgc12'
rdmTweets <- searchTwitter(searchTerm, n=500)
# Note that the Twitter search API only goes back 1500 tweets (I think?)

#Create a dataframe based around the results
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df,3)

counts=table(df$screenName)
barplot(counts)

# Let's do something hacky:
# Limit the data set to show only folk who tweeted twice or more in the sample
cc=subset(counts,counts>1)
barplot(cc,las=2,cex.names =0.3)


#Whilst tinkering, I came across some errors that seemed
# to be caused by unusual character sets
#Here's a hacky defence that seemed to work...
df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))

#A helper function to remove @ symbols from user names...
trim <- function (x) sub('@','',x)

#A couple of tweet parsing functions that add columns to the dataframe
#We'll be needing this, I think?
library(stringr)
#Pull out who a message is to
df$to=sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))
df$to=sapply(df$to,function(name) trim(name))

#And here's a way of grabbing who's been RT'd
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))

require(ggplot2)
ggplot()+geom_bar(aes(x=na.omit(df$rt)))+opts(axis.text.x=theme_text(angle=-90,size=6))+xlab(NULL)
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

#Plot of tweet behaviour by user over time
#Based on @mediaczar's http://blog.magicbeanlab.com/networkanalysis/how-should-page-admins-deal-with-flame-wars/
require(plyr)
#Make use of a handy dataframe creating twitteR helper function
tw.df=twListToDF(rdmTweets)
#@mediaczar's plot uses a list of users ordered by accession to user list
## 1) find earliest tweet in searchlist for each user [ http://stackoverflow.com/a/4189904/454773 ]
tw.dfx=ddply(tw.df, .var = "screenName", .fun = function(x) {return(subset(x, created %in% min(created),select=c(screenName,created)))})
## 2) arrange the users in accession order
tw.dfxa=arrange(tw.dfx,-desc(created))
## 3) Use the username accession order to order the screenName factors in the searchlist
tw.df$screenName=factor(tw.df$screenName, levels = tw.dfxa$screenName)
#ggplot seems to be able to cope with time typed values...
ggplot(tw.df)+geom_point(aes(x=created,y=screenName))

#TO DO: If we limit tweets according to eg RT, we should be able to get a view of RT activity?
#Alternatively, we can colour the RTs...
tw.df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
tw.df$rtt=sapply(tw.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')
ggplot(tw.df)+geom_point(aes(x=created,y=screenName,col=rtt))

#Generate a plot showing how a person is RTd
tw.df$rtof=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
#Note that this doesn't show how many RTs each person got in a given time period if they got more than one...
ggplot(subset(tw.df,subset=(!is.na(rtof))))+geom_point(aes(x=created,y=rtof))

#We can start to get a feel for who RTs whom...
require(gdata)
#We don't want to display screenNames of folk who tweeted but didn't RT
tw.df.rt=drop.levels(subset(tw.df,subset=(!is.na(rtof))))
#Order the screennames of folk who did RT by accession order (ie order in which they RTd)
tw.df.rta=arrange(ddply(tw.df.rt, .var = "screenName", .fun = function(x) {return(subset(x, created %in% min(created),select=c(screenName,created)))}),-desc(created))
tw.df.rt$screenName=factor(tw.df.rt$screenName, levels = tw.df.rta$screenName)
# Plot who RTd whom
ggplot(subset(tw.df.rt,subset=(!is.na(rtof))))+geom_point(aes(x=screenName,y=rtof))+opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)
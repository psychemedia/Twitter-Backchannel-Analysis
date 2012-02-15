require(twitteR)
#The original example used the twitteR library to pull in a user stream
#rdmTweets <- userTimeline("psychemedia", n=100)
#Instead, I'm going to pull in a search around a hashtag.
searchTerm='#dev8d'
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
tw.df$rt=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
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

##Note: there are also some handy, basic Twitter related functions here:
##https://github.com/matteoredaelli/twitter-r-utils
#For example:
RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}
#Then for example:
tweets <- as.vector(sapply(tw.df$text, RemoveAtPeople))

##Wordcloud - scripts available from various sources; I used:
#http://rdatamining.wordpress.com/2011/11/09/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/
#Call with eg: tw.c=generateCorpus(tw.df$text)
generateCorpus= function(df,my.stopwords=c()){
  #Install the textmining library
  require(tm)
  tw.corpus= Corpus(VectorSource(df))
  # remove punctuation
  ## I wonder if it would make sense to remove @d names first?
  tw.corpus = tm_map(tw.corpus, removePunctuation)
  #normalise case
  tw.corpus = tm_map(tw.corpus, tolower)
  # remove stopwords
  tw.corpus = tm_map(tw.corpus, removeWords, stopwords('english'))
  tw.corpus = tm_map(tw.corpus, removeWords, my.stopwords)

  tw.corpus
}

wordcloud.generate=function(corpus,min.freq=3){
  require(wordcloud)
  doc.m = TermDocumentMatrix(corpus, control = list(minWordLength = 1))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  wc=wordcloud(d$word, d$freq, min.freq=min.freq)
  wc
}

print(wordcloud.generate(generateCorpus(tweets,'dev8d'),7))
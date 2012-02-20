require(twitteR)

username='twitterapi'

#the most tweets we can bring back from a user timeline is the most recent 3200...
mht=userTimeline(username,n=250)
tw.df=twListToDF(mht)

#As I've done in previous scripts, pull out the names of folk who have been "old-fashioned RTd"...
require(stringr)
trim <- function (x) sub('@','',x)

tw.df$rt=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
tw.df$rtt=sapply(tw.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')

require(ggplot2)
ggplot(tw.df)+geom_point(aes(x=created,y=screenName))

#------

tw.dfs=subset(tw.df,subset=((Sys.time()-created)<8000))
ggplot(tw.dfs)+geom_point(aes(x=created,y=screenName))

#------

require(plyr)
#Order the replyToSN factor levels in the order in which they were first created
tw.dfx=ddply(tw.dfs, .var = "replyToSN", .fun = function(x) {return(subset(x, created %in% min(created),select=c(replyToSN,created)))})
tw.dfxa=arrange(tw.dfx,-desc(created))
tw.dfs$replyToSN=factor(tw.dfs$replyToSN, levels = tw.dfxa$replyToSN)

#and plot the result
ggplot(tw.dfs)+geom_point(aes(x=created,y=replyToSN))

#------

ggplot()+geom_point(data=subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=created,y=replyToSN),col='red') + geom_point(data=subset(tw.dfs,subset=(!is.na(rt))),aes(x=created,y=rt),col='blue') + geom_point(data=subset(tw.dfs,subset=(is.na(replyToSN) & is.na(rt))),aes(x=created,y=screenName),col='green')

#------

#First we need to count how many replies a user gets...
#http://stackoverflow.com/a/3255448/454773
r_table <- table(tw.dfs$replyToSN)
#..rank them...
r_levels <- names(r_table)[order(-r_table)]
#..and use this ordering to order the factor levels...
tw.dfs$replyToSN <- factor(tw.dfs$replyToSN, levels = r_levels) 

#Then we can plot the chart...
ggplot(subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=replyToSN)) + geom_bar(aes(y = (..count..))) + opts(axis.text.x=theme_text(angle=-90,size=6))

#------


#------

head(table(tw.dfs$replyToSN))
#eg returns:
#psychemedia        wilm     ambrouk    sheilmcn  dajbelshaw  manmalik 
394          66          59          53          48        43     
#Hmm..can we generalise this?
topTastic=function(dfc,num=5){
  r_table <- table(dfc)
  r_levels <- names(r_table)[order(-r_table)]
  head(table(factor(dfc, levels = r_levels)),num)
}
#so now, for example, I should be able to display the most old-style retweeted folk?
topTastic(tw.dfs$rt)
#or the 10 most replied to...
topTastic(tw.dfs$replyToSN,10)

#------

#label a tweet with the month number
tw.dfs$month=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$mon})
#label a tweet with the hour
tw.dfs$hour=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tw.dfs$wday=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$wday})

ggplot(tw.dfs)+geom_jitter(aes(x=wday,y=hour))

#------

#We can also generate barplots showing the distribution of tweet count over time:
ggplot(tw.dfs,aes(x=created))+geom_bar(aes(y = (..count..)))
#Hmm... I'm not sure how to manually set binwidth= sensibly, though?!

#------

#We can also plot the number of tweets within particular hour or time bins...
ggplot(tw.dfs,aes(x=wday))+geom_bar(aes(y = (..count..)),binwidth=1)
ggplot(tw.dfs,aes(x=hour))+geom_bar(aes(y = (..count..)),binwidth=1)

#------

require(xts)
#The xts function creates a timeline from a vector of values and a vector of timestamps.
#If we know how many tweets we have, we can just create a simple list or vector containing that number of 1s
ts=xts(rep(1,times=nrow(tw.dfs)),tw.dfs$created)

#We can now do some handy number crunching on the timeseries, such as applying a formula to values contained with day, week, month, quarter or year time bins.
#So for example, if we sum the unit values in daily bin, we can get a count of the number of tweets per day
ts.sum=apply.daily(ts,sum) 
#also apply. weekly, monthly, quarterly, yearly

#If for any resason we need to turn the timeseries into a dataframe, we can:
#http://stackoverflow.com/a/3387259/454773
ts.sum.df=data.frame(date=index(ts.sum), coredata(ts.sum))

colnames(ts.sum.df)=c('date','sum')

#We can then use ggplot to plot the timeseries...
ggplot(ts.sum.df)+geom_line(aes(x=date,y=sum))

#------

#Having got the data in a timeseries form, we can do timeseries based things to it... such as checking the autocorrelation:
acf(ts.sum)

#------

source('thirdpartyScripts/calendarHeatmap.R')
calendarHeat(ts.sum.df$date, ts.sum.df$sum, varname=paste(username,"Twitter activity"))

#------

#hashtag processing via http://stackoverflow.com/a/9360445/454773
hashtagAugment=function(tmp){
  tags <- str_extract_all(tmp$text, '#[a-zA-Z0-9]+')
  index <- rep.int(seq_len(nrow(tmp)), sapply(tags, length))
  tagged <- tw.dfs[index, ]
  tagged$tags <- unlist(tags)
  has_no_tag <- sapply(tags, function(x) length(x) == 0L)
  not_tagged <- tmp[has_no_tag, ]
  not_tagged$tags <- NA
  rbind(tagged, not_tagged)
}
tw.dfst=hashtagAugment(tw.dfs)
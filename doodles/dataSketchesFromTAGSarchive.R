#Sketch demonstrating how to pull data from one of @mhawksey's TAGS spreadsheet archives
##ALso includes a handful of demo charts

require(stringr)
require(RCurl)
require(ggplot2)
gsqAPI = function(key,query,gid=0){
  url=paste( sep="",'http://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid)
  print(paste("Trying to load file: ",url))
  return( read.csv( url ) ) 
}

trim <- function (x) sub('@','',x)

twCounts=function(df){
  print("Counting @'d users")
  to.count=data.frame(table(df$to))
  colnames(to.count)=c('Name','toCount')
  print('Counting senders')
  from.count=data.frame(table(df$from_user))
  colnames(from.count)=c('Name','fromCount')
  print('Counting rtof users')
  rtof.count=data.frame(table(df$rtof))
  colnames(rtof.count)=c('Name','rtofCount')
  print('Counting rtby users')
  rtby.count=data.frame(table(df$rtby))
  colnames(rtby.count)=c('Name','rtbyCount')
  print('Merging datasets')
  tmp=merge(rtof.count,to.count)
  tmp=merge(tmp,rtby.count)
  tmp=merge(tmp,from.count)
  tmp$Name=factor(tmp$Name)
  
  return(tmp)
}


twViz.scatter=function(df){
  ggplot(na.omit(df))+geom_text(aes(x=fromCount,y=toCount,label=Name,size=rtCount,angl=45))
}

twArchParse=function(key,gid){
  print('Getting data')
  df=gsqAPI(key,'select *',gid)
  print('Got data')
  print('Parsing @ messages')
  df$to=sapply(df$text,function(tweet) trim(str_extract(tweet,"^(@[[:alnum:]_]*)")))
  print('Parsing RT: messages')
  #THe str_match approach is really slow - I'm using it here rather than str_extract purely as a demo
  df$rtof=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
  print('Parsing RT: senders')
  df$rtby=paste(df$rtof,df$from_user)
  df$rtby=sapply(df$rtby,function(dfx) if (word(dfx,1)=='NA') NA else word(dfx,2))
  return(df)
}

barsorter=function (dfc){
  htable= table(dfc)
  hlevels=names(htable)[order(htable)]
  return(factor(dfc, levels = hlevels))
}

twViz.scatter2=function(df,xax='fromCount',yax='toCount',zsz='rtofCount'){
  ggplot(na.omit(df))+geom_text(aes_string(x=xax,y=yax,label='Name',size=zsz,angl=45))
}


#Example usage: 
key='0AqGkLMU9sHmLdG82MTNkNEd4Y1BzVEdfQ0RQOF9TTFE'
gid=82

lwf12.data=twArchParse(key,gid)
lwf12.counts=twCounts(lwf12.data)


df.data=lwf12.data
df.counts=lwf12.counts

#plot a bar chart of RT of counts
ggplot() + geom_bar(aes(x=na.omit(df.data$rtof))) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)

#sorted plot based on computed counts - "RT of"
df.data$hrt=barsorter(df.data$rtof)
ggplot() + geom_bar(aes(x=na.omit(df.data$hrt))) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)


#plot a bar chart of 'to' computed counts
ggplot() + geom_bar(aes(x=na.omit(df.data$to))) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)

#plot a bar chart of 'from' computed counts
ggplot() + geom_bar(aes(x=na.omit(df.data$from_user))) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)

#plot an ordered bar chart of 'from' tabulated counts
df.counts$Name <- reorder(df.counts$Name, df.counts$toCount)
ggplot(df.counts) + geom_bar(stat = "identity",aes(x=Name,y=toCount)) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)

#plot a scatterplot displaying to and from counts on x and ya axes, and label size as RT count
twViz.scatter2(df.counts)
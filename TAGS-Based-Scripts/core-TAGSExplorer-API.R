## Core functions for grabbing data from a TAGSExplorer spreadsheet
## I think we can gain access to this from an arbitrary script via something like:
## source("TAGS-Based-Scripts/core-TAGSExplorer-API.R")

require(RCurl)
require(stringr)

##TO DO: I think it would be really handy of we had a way of cacheing spreadsheet calls?
gsqAPI = function(key,query,gid=0){
  url=paste( sep="",'http://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid)
  return( read.csv( url ) ) 
}

twArchParse=function(key,gid){
  #Get data
  df=gsqAPI(key,'select *',gid)
  #Parsing @ messages
  df$to=sapply(df$text,function(tweet) trim(str_extract(tweet,"^(@[[:alnum:]_]*)")))
  #Parsing RT: messages
  #The str_match approach is really slow - I'm using it here rather than str_extract purely as a demo
  df$rtof=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
  #Parsing RT: senders
  df$rtby=paste(df$rtof,df$from_user)
  df$rtby=sapply(df$rtby,function(dfx) if (word(dfx,1)=='NA') NA else word(dfx,2))
  return(df)
}


twCounts=function(df){
  #Counting @'d users
  to.count=data.frame(table(df$to))
  colnames(to.count)=c('Name','toCount')
  #Counting senders
  from.count=data.frame(table(df$from_user))
  colnames(from.count)=c('Name','fromCount')
  #Counting rtof users
  rtof.count=data.frame(table(df$rtof))
  colnames(rtof.count)=c('Name','rtofCount')
  #Counting rtby users
  rtby.count=data.frame(table(df$rtby))
  colnames(rtby.count)=c('Name','rtbyCount')
  #Merging datasets
  tmp=merge(rtof.count,to.count)
  tmp=merge(tmp,rtby.count)
  tmp=merge(tmp,from.count)
  tmp$Name=factor(tmp$Name)
  
  return(tmp)
}

trim <- function (x) sub('@','',x)
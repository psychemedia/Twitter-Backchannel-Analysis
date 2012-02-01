## Core functions for grabbing data from a TAGSExplorer spreadsheet
## I think we can gain access to this from an arbitrary script via something like:
## source("TAGS-Based-Scripts/core-TAGSExplorer-API.R")

require(RCurl)

gsqAPI = function(key,query,gid=0){
  url=paste( sep="",'http://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid)
  print(paste("Trying to load file: ",url))
  return( read.csv( url ) ) 
}
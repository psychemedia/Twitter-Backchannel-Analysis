# This script reads data generated from the TAGS Advance menu options and generates SNA metrics using igraph.
# For each user the script gernates betweenness and eigenvector centrality, pagerank and degree (in, out, all)
# The script also complies a top 10 list for all these measures. The results are then pushed back and integrated
# into the original spareadsheet. For more information on usage read http://mashe.hawksey.info/2012/01/tags-r/
# Related file: generate-sna-stats-push-back-to-spreadsheet.js
# Compatibility: TAGS 3.1+


library(igraph)
library(reshape)
library(plyr)
require(RJSONIO)
require(RCurl)

gsqAPI = function(key,query,gid=0){
  url=paste( sep="",'http://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid)
  return( read.csv( url ) ) 
}

# Google Spreadsheet key (must be published to the web)
key='0AqGkLMU9sHmLdGwtdXB3QTEwUUx3b0E0R3dKeEZCUXc'
# Sheet gid name
gid=104
# Publsih as service url
serviceUrl='https://docs.google.com/macros/exec?service=AKfycbxKEXMbS4MTNDcc1y9qoKI1ozbMczmn_qmiDZBC'
# A secret set in the Script Editor of the spreadsheet to prevent unauthorised data entry
secret='fudgebananana'

# Read data from spreadsheet
ss = gsqAPI(key, 'select *',gid)

#pass to igraph
g <- graph.data.frame(ss, directed = T)


# calculate some stats
betweenness_centrality <- betweenness(g,v=V(g),directed = TRUE)
eigenvector_centrality<-evcent(g)
pagerank<-page.rank(g)$vector
degree<-degree(g, v=V(g), mode = "total")
degree_in<-degree(g, v=V(g), mode = "in")
degree_out<-degree(g, v=V(g), mode = "out")
screen_name<-V(g)$name

# might want to get centralization stats http://igraph.wikidot.com/r-recipes (not included for now)

# bind into matrice
datagrid<-data.frame(I(screen_name),degree,degree_in,degree_out,betweenness_centrality,eigenvector_centrality[[1]],pagerank)
cc <- c("screen_name", "degree","degree_in","degree_out","betweenness_centrality","eigenvector_centrality","pagerank")
colnames(datagrid) <- cc
rownames(datagrid) <- screen_name
# convert to JSON
test <- data.matrix(datagrid)
dg = toJSON(data.matrix(datagrid))
dglabels =  toJSON(rownames(datagrid))

#get top results from data.frame http://stackoverflow.com/a/3320420/1027723 
datagrid.m <- melt(datagrid, id = 1)
a.screen_name <- cast(datagrid.m, screen_name ~ . | variable)
a.screen_name.max <- aaply(a.screen_name, 1, function(x) arrange(x, desc(`(all)`))[1:10,])
datagrid.screen_name.max <- adply(a.screen_name.max, 1)
toptens <- cbind(datagrid.screen_name.max[,2],datagrid.screen_name.max[,3],datagrid.screen_name.max[,1])
toptens = toJSON(toptens)
toptenslabels = toJSON(colnames(datagrid))


# write back to spreadsheet http://mashe.hawksey.info/2011/10/google-spreadsheets-as-a-database-insert-with-apps-script-form-postget-submit-method/
# SSL fix http://code.google.com/p/r-google-analytics/issues/detail?id=1#c3
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
# post form
postForm(serviceUrl, "datagrid" = dg, "datagridlabels" = dglabels, "toptens" = toptens, "toptenslabels" = toptenslabels,  "secret" = secret)


require(RCurl)
library(tm)
library(wordcloud)
require(RColorBrewer)
gsqAPI = function(key,query,gid=0){ return( read.csv( paste( sep="",'http://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid), header = T) ) }

# Google Spreadsheet key (must be published to the web first)
key='0AqGkLMU9sHmLdHNHSkkxLXB5ZW9PYzdQTWNrVWhUOXc'
# Sheet gid name
gid=109
# column of text to analyse - query can be more complex using WHERE, LIMIT etc
query = 'select P'

# Read data from spreadsheet
dataset = gsqAPI(key,query,gid)


# this bit from http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
# note if you are pulling in multiple columns you may needd to change which one 
# in the dataset is select e.g. dataset[,2] etc
ap.corpus <- Corpus(DataframeSource(data.frame(as.character(dataset[,1]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
# additional stopwords can be used as shown below 
#ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, c("ukoer","oer")))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
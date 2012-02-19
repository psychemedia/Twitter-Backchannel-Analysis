require(twitteR)
searchTerm='#dev8d'
#Grab the tweets
rdmTweets <- searchTwitter(searchTerm, n=500)
#Use a handy helper function to put the tweets into a dataframe
tw.df=twListToDF(rdmTweets)

##Note: there are some handy, basic Twitter related functions here:
##https://github.com/matteoredaelli/twitter-r-utils
#For example:
RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}
#Then for example, remove @'d names
tweets <- as.vector(sapply(tw.df$text, RemoveAtPeople))

##Wordcloud - scripts available from various sources; I used:
#http://rdatamining.wordpress.com/2011/11/09/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/

#Install the textmining library
require(tm)

#Call with eg: tw.c=generateCorpus(tw.df$text)
generateCorpus= function(df,my.stopwords=c()){
  #The following is cribbed and seems to do what it says on the can
  tw.corpus= Corpus(VectorSource(df))
  # remove punctuation
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
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq)
  wc
}

print(wordcloud.generate(generateCorpus(tweets,'dev8d'),7))

##Generate an image file of the wordcloud
png('test.png', width=600,height=600)
wordcloud.generate(generateCorpus(tweets,'dev8d'),7)
dev.off()

#We could make it even easier if we hide away the tweet grabbing code. eg:
tweets.grabber=function(searchTerm,num=500){
  require(twitteR)
  rdmTweets = searchTwitter(searchTerm, n=num)
  tw.df=twListToDF(rdmTweets)
  as.vector(sapply(tw.df$text, RemoveAtPeople))
}
#Then we could do something like:
tweets=tweets.grabber('ukgc12')
wordcloud.generate(generateCorpus(tweets),3)


#wordcloud stuff form wordcloud.R
mk1=subset(mk,Modularity.Class==1)
require(tm)
require(plyr)

gc=function(x) generateCorpus(x$desc)
wcg=wordcloud.generate(gc(x))
gc2=function(x) generateCorpus(x$desc,c('milton','keynes'))
wcg2=function(x) wordcloud.generate(gc2(x))
wcg3=function(x) wordcloud.generate(gc2(x),3)

mk2=mk
mk2=mk2[with(mk2, order(Modularity.Class)), ]
View(mk2)
ddply(mk,.(Modularity.Class),wcg3)
require(plyr)
ddply(mk,.(Modularity.Class),wcg3)
ddply(mk,.(Modularity.Class),wcg3)
reqiore(tm)
require(tm)
ddply(mk,.(Modularity.Class),wcg3)
tw.corpus= Corpus(VectorSource(mk))
tw.corpus
tw.corpus= Corpus(VectorSource(mk$desc))
tw.corpus
tw.corpus?png= Corpus(VectorSource(mk$desc))
?png
png
?svg
svg("tmp.svg")
wordcloud.generate(generateCorpus(mk))
dev.off()
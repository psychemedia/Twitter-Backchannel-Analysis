#DEPENDENCIES
require(tm)
require(plyr)
require(wordcloud)
source('doodles/wordcloud.R')


xlist=c('milton','keynes')

stub='TEDxMK'
#wordcloud stuff form wordcloud.R
mk1=subset(mk,Modularity.Class==1)

gc=function(x) generateCorpus(x$desc)

gc2=function(x) generateCorpus(x$desc,xlist)
wcg2=function(x) wordcloud.generate(gc2(x))
wcg3=function(x) wordcloud.generate(gc2(x),4)
wcg4=function(x) {
  fn=paste('./output/',stub,'tmp',x$Modularity.Class,'.svg',sep='')
  svg(fn)
  wcg3(x)
  dev.off()
}
  
wcg=wordcloud.generate(gc(mk1))

xlist=c('')
wcdata=comact4

wcdata=subset(ou,Eigenvector.Centrality>0)

wcdata=comact4
stub='comact4'
wcdata=wcdata[with(wcdata, order(Modularity.Class)), ]
ddply(wcdata,.(Modularity.Class),wcg4)
ddply(wcdata,.(Modularity.Class),wcg3)

ddply(mk,.(Modularity.Class),wcg3)

ddply(mk,.(Modularity.Class),wcg4)

tw.corpus= Corpus(VectorSource(mk$desc))
tw.corpus
tw.corpus?png= Corpus(VectorSource(mk$desc))

svg("output/tmp.svg")
wordcloud.generate(generateCorpus(mk))
dev.off()
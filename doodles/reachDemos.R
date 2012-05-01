ddj_ncount <- read.csv("~/code/twapps/newt/reports/tmp/lak12_ncount.csv")
#Convert the datetime string to a time object
ddj_ncount$ttime=as.POSIXct(strptime(ddj_ncount$datetime, "%a, %d %b %Y %H:%M:%S"),tz='UTC')

#Order the newuser factor levels into the order in which they first use the tag
dda=subset(ddj_ncount,select=c('ttime','newuser'))
dda=arrange(dda,-desc(ttime))
ddj_ncount$newuser=factor(ddj_ncount$newuser, levels = dda$newuser)

#Plot when each user first used the tag against time
ggplot(ddj_ncount) + geom_point(aes(x=ttime,y=newuser)) + opts(axis.text.x=theme_text(size=6),axis.text.y=theme_text(size=4))

#Plot the cumulative and union flavours of increasing possible audience size, as well as the difference between them
ggplot(ddj_ncount) + geom_line(aes(x=ttime,y=count,col='Unique followers')) + geom_line(aes(x=ttime,y=crudetot,col='Cumulative followers')) + geom_line(aes(x=ttime,y=crudetot-count,col='Repeated followers')) + labs(colour='Type') + xlab(NULL)

#Number of new unique followers introduced at each time step
ggplot(ddj_ncount)+geom_line(aes(x=ttime,y=count-previousCount,col='Actual delta'))

#Try to get some idea of how many of the followers of a new user are actually new potential audience members
ggplot(ddj_ncount) + opts(axis.text.x=theme_text(angle=-90,size=4)) + geom_linerange(aes(x=newuser,ymin=0,ymax=userFoCount,col='Follower count')) + geom_linerange(aes(x=newuser,ymin=0,ymax=(count-previousCount),col='Actual new audience'))

#This is still a bit experimental
#I'm playing around trying to see what proportion or number of a users followers are new to, or subsumed by, the potential audience of the tag to date...
ggplot(ddj_ncount) + geom_linerange(aes(x=newuser,ymin=0,ymax=1-(count-previousCount)/userFoCount)) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)






#require(igraph)

#Read in the graph: the graphs contain nodes representing Twitter users connected by directed weighted edges that represent 'is followed by' relations. The weights correspond to the number of hashtagged messages published by the from-node over the sample period 
#g2=read.graph('/Users/ajh59/code/twapps/newt/reports/tmp/ddj_ncount.graphml',format='graphml')

g2=ddj_ncount
summary(g2)
#The summary provides an overview of the graph, The number of nodes corresponds to the number of folk in the union of the set of hashtaggers and their followers, for example.

#We can count how many nodes have a particular in-degree count (where' in-degree represents the number of hashtaggers the node follows)
g.nodes=as.data.frame(table(degree(g2,mode='in')))
g.nodes$Var1=as.numeric(levels(g.nodes$Var1)[as.integer(g.nodes$Var1)])

#Check: if we sum the node occurrence frequencies, we should get the total number of nodes as a result
sum(g.nodes$Freq)

#We can then chart the result to look at the distribution of how many hashtaggers are followed by how many people
require(ggplot2)
ggplot(g.nodes)+geom_linerange(aes(x=Var1,ymin=0,ymax=Freq)) + scale_y_log10() + xlab('In-degree of followers')







#The incoming edges to follower nodes are weighted according to the number of tagged tweets the corresponding hashtagger published in the sample period.
#What this means is that we can count the total number of tagged tweets seen by each follower by summing the weights of edges incident on each node
g.weights=as.data.frame(table(graph.strength(g2,mode='in')))
g.weights$Var1=as.numeric(levels(g.weights$Var1)[as.integer(g.weights$Var1)])
#If we sum the product of message counts and frequencies, we see how many potential "receipts" of a tagged tweet there were.
sum(g.weights$Var1*g.weights$Freq)

#We can also plot the distribution of the number of tagged tweets potentially received by each follower
ggplot(g.weights)+geom_linerange(aes(x=Var1,ymin=0,ymax=Freq)) + scale_y_log10() + xlab('Incoming tagged message count')





#It's also easy enough to chart the distribution of the follower counts for each hashtagger:
tagger.nodes=subset(as.data.frame(table(degree(g2,mode='out'))),subset=(Var1!='0'))
tagger.nodes$Var1=as.numeric(levels(tagger.nodes$Var1)[as.integer(tagger.nodes$Var1)])
#Quick check on the number of taggers
sum(tagger.nodes$Freq)

#And the distribution of how many followers they have
ggplot(tagger.nodes)+geom_histogram(aes(x=Var1,ymin=0,ymax=Freq),binwidth=250)  + xlab('Follower count')
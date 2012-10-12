
require(wordcloud)
require(ggplot2)


counts_a=read.csv("~/code/newt/reports/scmvESP/NewsInt/thetimes/counts_testX.csv")
counts_b= read.csv("~/code/newt/reports/scmvESP/NewsInt/thesunnewspaper/_2012-09-18-09-56-14/counts_testX.csv")

#Multiply this nromalised follower proportion by 1000 and round down to get an integer between 0 and 1000 representing a score relative to the proportion of filtered hashtagger who follow each person in the interest list.
counts_a$normIn=as.integer(counts_a$inNorm*1000)
counts_a$normIn=as.integer(counts_a$inNorm*1000)

#ANother filtering step: we're going to plot similarities and differences between folk followed by at least 25% of the corresponding filtered hashtaggers
a=subset(counts_a,select=c(username,inNorm),subset=(inNorm>=0.15))
b=subset(counts_b,select=c(username,inNorm),subset=(inNorm>=0.15))

#Now generate a dataframe
qtvnn=merge(a,b,by="username",all=T)
colnames(qtvnn)=c('username','a','b')


#Dangerous fudge - assume data collected about the same time and that we can use the av follower count
#popular folk have fo_count increasing on a second by second basis
foc=unique(rbind(subset(counts_a,select=c(username)),subset(counts_b,select=c(username),by=c('username'))))
foc=merge(foc,subset(counts_a,select=c(username,fo_count)),by='username')
foc=merge(foc,subset(counts_b,select=c(username,fo_count)),by='username')
foc$av_fo_count=rowMeans(foc[,c("fo_count.x","fo_count.x")])
qtvnnc=merge(subset(counts_a,subset=indegree>0,select=c(username,indegree)),subset(counts_b,subset=indegree>0,select=c(username,indegree)),by='username')
qtvnnc=merge(qtvnnc,subset(foc,select=c(username,av_fo_count)),by='username')

a.x=merge(subset(counts_a,subset=(inNorm>=0.15)),subset(foc,select=c(username,av_fo_count)),by='username')
b.x=merge(subset(counts_b,subset=(inNorm>=0.15)),subset(foc,select=c(username,av_fo_count)),by='username')

#funnel
number=a.x$av_fo_count
p=a.x$inNorm

p.se <- sqrt((p*(1-p)) / (number))
df <- data.frame(p, number, p.se)

## common effect (fixed effect model)
p.fem <- weighted.mean(p, 1/p.se^2)

## lower and upper limits for 95% and 99.9% CI, based on FEM estimator
#TH: I'm going to alter the spacing of the samples used to generate the curves
number.seq <- seq(1000, max(number), 1000)
number.ll95 <- p.fem - 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq))
number.ul95 <- p.fem + 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq))
number.ll999 <- p.fem - 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq))
number.ul999 <- p.fem + 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq))
dfCI <- data.frame(number.ll95, number.ul95, number.ll999, number.ul999, number.seq, p.fem)


fp <- ggplot(aes(x = number, y = p), data = df) +
  geom_point(shape = 1) +
  geom_line(aes(x = number.seq, y = number.llN), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ulN), data = dfCI) +
  ylim(0,1)
fp
## draw plot
#TH: note that we need to tweak the limits of the y-axis
fp <- ggplot(aes(x = number, y = p), data = df) +
  geom_point(shape = 1) +
  geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ll999, linetype = 2), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul999, linetype = 2), data = dfCI) +
  geom_hline(aes(yintercept = p.fem), data = dfCI) +
  scale_y_continuous(limits = c(0,0.0004)) +
  xlab("number") + ylab("p") + theme_bw()

fp



#-----
#replace the NA cell values (where for example someone in the bbcqt list is not in the newsnight list
qtvnn[is.na(qtvnn)] <- 0

mat <- as.matrix(qtvnn[-1])
dimnames(mat)[1] <- qtvnn[1]
comparison.cloud(term.matrix = mat)
commonality.cloud(term.matrix = mat)


g=ggplot(subset(qtvnn,a>0 & b>0))
g=g+ geom_point(aes(x=a,y=b))
print(g)

g=ggplot(subset(qtvnn,a>0 & b>0))
g=g+ geom_text(aes(x=a,y=b,label=username),angle=45,size=4)
g=g+ geom_abline(intercept=0, slope=1,colour='grey')
print(g)

require(reshape)
mq=melt(qtvnn,id=('username'))
g=ggplot(subset(mq,value>0))
g=g+ geom_text(aes(x=variable,y=value,label=username),size=2)
g=g+ylim(0,0.6)
print(g)

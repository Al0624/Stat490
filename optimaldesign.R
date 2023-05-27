
library(AlgDesign)

#### example 1: 3 factors, each has 2 levels, 2 replicates in each treatment 

dat<-gen.factorial(levels=2,nVars=3,varNames=c("A","B","C"))

dat1<-dat[rep(seq_len(nrow(dat)), each = 2), ]
dat1

desD<-optFederov(~ A * B * C, dat1,nTrials=12,eval=TRUE)
desD

#### example 2

dat2<-gen.factorial(levels=c(5,2,2),nVars=3,varNames=c("A","B","C"))
dat2

desD<-optFederov(~ quad(A) + B + C, dat2,nTrials=12,eval=TRUE)

desD

#### other example

dat<-gen.factorial(levels=3,nVars=3,varNames=c("A","B","C"))

desA<-optFederov(~quad(.),dat,nTrials=14,eval=TRUE,crit="A")
desA

desI<-optFederov(~quad(.),dat,nTrials=14,eval=TRUE,crit="I")
desI

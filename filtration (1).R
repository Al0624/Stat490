filtration<-read.csv("filtration_24.csv", header=T)


yates_24<-yates(filtration$Rate, labels=c("A", "B", "C", "D"))
yates_24

yates_24<-as.data.frame(as.matrix(yates_24))
yates_24$name <-row.names(yates_24)

yates_24

m5<-aov(data=filtration, Rate ~ A * B * C * D)
summary(m5)

SST<-var(filtration$Rate) * (dim(filtration)[1]-1)
SST

varcomp<-summary(m5)[[1]]
varcomp$var_pct <-summary(m5)[[1]]$'Sum Sq'/SST*100
varcomp

ggplot(yates_24,aes(sample = V1)) + geom_text(label=yates_24$name[order(yates_24$V1)], stat="qq") + 
  stat_qq() + stat_qq_line()

#########

m6 <- aov(data=filtration, Rate ~ A + C + D + A * C + A * D)
summary(m6)

m6<- lm(data=filtration, Rate ~ A + C + D + A * C + A * D)
summary(m6)

plotPlane(m6, plotx1="A", plotx2="C")
contour(m6, C ~ A)

plotPlane(m6, plotx1="A", plotx2="D")
contour(m6, D ~ A)


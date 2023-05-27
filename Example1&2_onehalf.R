library(unrepx)
library(ggplot2)
library(dplyr)
library(moderndive)

###### example 1

filtration<-read.csv("filtration_24_fractional.csv", header=T)

filtration

yates(filtration$Rate)


yates_24<-yates(filtration$Rate)

yates_24<-as.data.frame(as.matrix(yates_24))
yates_24$name <-row.names(yates_24)

yates_24

ggplot(yates_24,aes(sample = V1)) + geom_text(label=yates_24$name[order(yates_24$V1)], stat="qq") + 
  stat_qq() + stat_qq_line()

m1<-aov(data=filtration, Rate ~ A + C + D + A * C + A * D)
summary(m1)

m1<-lm(data=filtration, Rate ~ A + C + D + A * C + A * D)
summary(m1)

############# example 2 

circuit<-read.csv("circuit_25_fractional.csv", header=T)
circuit

yates(circuit$Yield)

yates_25<-yates(circuit$Yield)

yates_25<-as.data.frame(as.matrix(yates_25))
yates_25$name <-row.names(yates_25)

yates_25

ggplot(yates_25,aes(sample = V1)) + geom_text(label=yates_25$name[order(yates_25$V1)], stat="qq") + 
  stat_qq() + stat_qq_line()

m2<-aov(data=circuit, Yield ~ A + B + C + A * B)
summary(m2)

m2<-lm(data=circuit, Yield ~ A + B + C + A * B)
summary(m2)


interaction.plot(x.factor = circuit$A, #x-axis variable
                 trace.factor = circuit$B, #variable for lines
                 response = circuit$Yield, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Yield",
                 xlab = "A",
                 col = c("pink", "blue", "Red"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "B")

points<-get_regression_points(m2)

ggplot(data=points, aes(sample=residual)) + stat_qq() + stat_qq_line()
ggplot(data=points, aes(x=Yield_hat, y=residual)) + geom_point() + geom_hline(yintercept=0)






library(unrepx)
library(ggplot2)
library(dplyr)
library(moderndive)

blade<-read.csv("blade_28_blk_fractional.csv", header=T)
blade$Block<-as.factor(blade$Block)
blade <- blade %>% mutate(logSD=log(SD))
blade

yates(blade$logSD)

yates_28<-yates(blade$logSD)

yates_28<-as.data.frame(as.matrix(yates_28))
yates_28$name <-row.names(yates_28)

yates_28

ggplot(yates_28,aes(sample = V1)) + geom_text(label=yates_28$name[order(yates_28$V1)], stat="qq") + 
  stat_qq() + stat_qq_line()

interaction.plot(x.factor = blade$A, #x-axis variable
                 trace.factor = blade$D, #variable for lines
                 response = blade$logSD, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "logSD",
                 xlab = "A",
                 col = c("pink", "blue", "Red"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "D")


m4<-aov(data=blade, logSD ~ A + B + D + A * D + Block)
summary(m4)

m4<-lm(data=blade, logSD ~ A + B + D + A * D + Block)
summary(m4)


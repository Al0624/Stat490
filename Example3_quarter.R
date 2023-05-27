library(unrepx)
library(ggplot2)
library(dplyr)
library(moderndive)

##### example 3: one-quarter design: 2^(6-2), resolution IV 

molding<-read.csv("molding_26_fractional.csv", header=T)
molding

yates_26<-yates(molding$Shrinkage)

yates_26<-as.data.frame(as.matrix(yates_26))
yates_26$name <-row.names(yates_26)

yates_26

###### QQ plot for the effects: find potential model 

ggplot(yates_26,aes(sample = V1)) + geom_text(label=yates_26$name[order(yates_26$V1)], stat="qq") + 
  stat_qq() + stat_qq_line()

m3<-aov(data=molding, Shrinkage ~ A * B)
summary(m3)

m3<-lm(data=molding, Shrinkage ~ A * B)
summary(m3)

##### QQ plot and residual plot: normality Ok, but cone shape in residual plot 

points<-get_regression_points(m3)
mold2<- molding %>% select(C, D, E, F)
points<-cbind(mold2, points)
points

ggplot(data=points, aes(sample=residual)) + stat_qq() + stat_qq_line()
ggplot(data=points, aes(x=Shrinkage_hat, y=residual)) + geom_point() + geom_hline(yintercept=0)

##### investigate the dispersion effect 
##### dispersion function is to calculate the log of ratio (variance at +1 and variance at -1) for each effect 
##### the other three way interaction ABC=E, BCD=F 
##### it can be seen that at factor C, the variances of residuals are much different at +1 and -1

points <- points %>% mutate(AB=A*B, AC=A*C, AD=A*D, AE=A*E, AF=A*F, BD=B*D, BF=B*F, ABD=A*B*D, ACD=A*C*D)
varlist<-c("A", "B", "C", "D", "E", "F", "AB", "AC", "AD", "AE", "AF", "BD", "BF", "ABD", "ACD")
disp<-rep(0,15)

dispersion <- function (data, class, var) {
  summ<-as.matrix(data %>% group_by(.data[[class]]) %>% summarise(varres=var(.data[[var]])))
  log(summ[2,2]/summ[1,2])
}

for(i in 1:15){
  disp[i]<-dispersion(points, class=varlist[i], var="residual")
}

disp

dispeffect <- data.frame(varlist, disp)
dispeffect

ggplot(dispeffect,aes(sample = disp)) + geom_text(label=dispeffect$varlist[order(dispeffect$disp)], stat="qq") + 
  stat_qq() + stat_qq_line()

ggplot(data=points, aes(x=C, y=residual)) + geom_point() + geom_hline(yintercept=0)


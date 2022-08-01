source("testStatistic.R")
library(pracma)
library(poweRlaw)

#check test statistic
set.seed(28072022)
n=10000
xmin=1
alpha=2
x=rplcon(n,xmin,alpha)

F<-function(x){
  pplcon(x,xmin,alpha)
}


max(x)
distanceGeneral(x,F,xmin,3*max(x))
distancePowerLaw(x,xmin,alpha)
distancePowerLaw2(x,xmin)




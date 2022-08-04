source("testStatistic.R")
library(pracma)

#check test statistic for exponential distribution
set.seed(28072022)
n=10000
lambda=1
x=rexp(n,rate=lambda)

F<-function(x){
  pexp(x,rate=lambda)
}



res=distanceGeneral(x,F,0)
distanceExponentialDistribution(x,lambda)





source("testStatistic.R")
library(pracma)

#check test statistic
set.seed(28072022)
n=5
lambda=5
x=rexp(n,rate=lambda)

F<-function(x){
  pexp(x,rate=lambda)
}



testStatisticGeneral(x,F)
testStatisticGeneral(x,F,0,2)
testStatisticExponentialDistribution(x,lambda)





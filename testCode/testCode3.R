source("testStatistic.R")
library(pracma)

#check minimum distance estimator for exponential distribution

#set.seed(28072022)
n=100
lambda=3
x=rexp(n,rate=lambda)

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}


distance2<-function(x,param){
  F<-function(t){
    pexp(t,rate=param)
  }
  
  res=distanceGeneral(x,F,0)
  return(res$value)
}


minDistanceEstimator(x,distance,lambda)

minDistanceEstimator(x,distance2,lambda)


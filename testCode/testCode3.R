source("testStatistic.R")
library(pracma)

#check minimum distance estimator for exponential distribution

#set.seed(28072022)
n=10
lambda=1
x=rexp(n,rate=lambda)

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}

minDistanceEstimator(x,distance,lambda)





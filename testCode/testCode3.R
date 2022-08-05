source("testStatistic.R")
library(pracma)

#check minimum distance estimator for exponential distribution

#set.seed(28072022)
n=100
lambda=2
x=rexp(n,rate=lambda)
interval=c(0.1,5)

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}

minDistanceEstimator(x,distance,lambda, interval)$minimum



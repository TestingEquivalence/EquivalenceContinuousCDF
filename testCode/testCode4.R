source("testStatistic.R")
library(pracma)
library(poweRlaw)

#check minimum distance estimator for power law
set.seed(28072022)
n=1000
xmin=5
alpha=2.5
x=rplcon(n,xmin,alpha)

distance1<-function(x, param){
  distancePowerLaw(x, xmin,param)
}

distance2<-function(x, param){
  distancePowerLaw(x, param, alpha)
}

distance3<-function(x, param){
  distancePowerLaw(x, param[1], param[2])
}

est1=minDistanceEstimator(x,distance1,param = alpha, interval = c(1,3))
est2=minDistanceEstimator(x,distance2,param = xmin, interval=c(0.5,10))
est3=minDistanceEstimator(x,distance3,param = c(xmin,alpha))

est1
est2
est3

testStatistic(x,distance1,est1)
testStatistic(x,distance2,est2)
testStatistic(x,distance3,est3)


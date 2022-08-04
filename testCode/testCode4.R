source("testStatistic.R")
library(pracma)
library(poweRlaw)

#check minimum distance estimator for power law
set.seed(28072022)
n=100
xmin=1.0
alpha=2.0
x=rplcon(n,xmin,alpha)

distance<-function(x, param){
  distancePowerLaw(x, param[1], param[2])
}

distancePowerLaw(x,xmin,alpha)

startParameter=c(xmin,alpha)
minDistanceEstimator(x,distance,startParameter)





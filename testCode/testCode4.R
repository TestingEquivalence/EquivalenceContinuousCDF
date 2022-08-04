source("testStatistic.R")
library(pracma)
library(poweRlaw)

#check minimum distance estimator for power law
set.seed(28072022)
n=1000
xmin=1.0
alpha=4
x=rplcon(n,xmin,alpha)

distance<-function(x, param){
  distancePowerLaw(x, xmin, param)
}

distancePowerLaw(x,xmin,alpha)

minDistanceEstimator(x,distance,2.1)





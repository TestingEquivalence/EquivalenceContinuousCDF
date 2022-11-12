source("testStatistic.R")
library(pracma)
library(poweRlaw)
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")


# simulate variance of the test statistic and
# test the performance of different variance estimators for power law distribution

parameter=list()
set.seed(11112022)

n=100
alpha=2.5
xmin=1
parameter$interval=c(2.001,3)
parameter$nSimulation=200
parameter$xmin=xmin

distance<-function(x, param){
  distancePowerLaw(x,xmin,param)
}

m=1000
vTS=rep(0,m)
vB=rep(0,m)
vA=rep(0,m)

for (i in c(1:m)){
  x=rplcon(n,xmin,alpha)
  #x=rexp(n,lambda)
  parameter$x=x
  est=minDistanceEstimator(x,distance,alpha, parameter$interval)
  res=list()
  res$estimator=est
  #vTS[i]=testStatistic(x,distance,est)
  #vB[i]=bootstrapStandardDeviation(parameter,res)
  vA[i]=standardDeviationPowerLaw(parameter,res)
  print(i)
}

#sd(vTS)
#mean(vB)
mean(vA)


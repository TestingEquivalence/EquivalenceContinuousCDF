source("testStatistic.R")
library(pracma)
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")


# simulate variance of the test statistic and
# test the performance of different variance estimators for exponential distribution

parameter=list()
set.seed(28072022)

n=200
lambda=1
parameter$interval=c(0.1,5)
parameter$nSimulation=200
parameter$startValue=c(lambda)

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}

m=1000
vTS=rep(0,m)
vB=rep(0,m)
vA=rep(0,m)

for (i in c(1:m)){
  x=rlnorm(n,0,1)
  #x=rexp(n,lambda)
  parameter$x=x
  est=minDistanceEstimator(x,distance,lambda, parameter$interval)
  res=list()
  res$estimator=est
  #vTS[i]=testStatistic(x,distance,est)
  vB[i]=bootstrapStandardDeviation(parameter,res)
  #vA[i]=standardDeviationExponential(parameter,res)
  print(i)
}

#sd(vTS)
mean(vB)
#mean(vA)

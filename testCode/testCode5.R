source("testStatistic.R")
library(pracma)
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")


# simulate variance of the test statistic
# test the performance of different variance estimators

parameter=list()
set.seed(28072022)

n=10
lambda=1
parameter$interval=c(0.1,5)
parameter$nSimulation=1000

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}

m=1000
vTS=rep(0,m)
vB=rep(0,m)
vA=rep(0,m)

for (i in c(1:m)){
  x=rlnorm(n,0,1)
  # x=rexp(n,lambda)
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
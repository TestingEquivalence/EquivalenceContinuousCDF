source("testStatistic.R")
library(pracma)
library(poweRlaw)
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")


# simulate variance of the test statistic and
# test the performance of different variance estimators for power law distribution   
distance<-function(x, param){
  distancePowerLaw(x,param[1],param[2])
}

parameter=list()
set.seed(11112022)

n=100
alpha=2.5
xmin=1
parameter$nSimulation=200
parameter$startValue=c(xmin,alpha)
parameter$lower=c(0.5,2.1)
parameter$upper=c(1.5,5)
parameter$distance=distance


m=1000
vTS=rep(0,m)
vB=rep(0,m)
vA=rep(0,m)

for (i in c(1:m)){
  #x=rplcon(n,xmin,alpha)
  x=rexp(n,1)+1
  parameter$x=x
  est=minDistanceEstimator(x,distance,param = c(xmin,alpha), lower=c(0.5,2.1), upper = c(1.5,5))
  res=list()
  res$estimator=est
  #vTS[i]=testStatistic(x,distance,est)
  vB[i]=bootstrapStandardDeviation(parameter,res)
  #vA[i]=standardDeviationPowerLaw(parameter,res)
  print(i)
}

#sd(vTS)
mean(vB)
#mean(vA)


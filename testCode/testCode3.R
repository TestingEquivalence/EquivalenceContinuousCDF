source("testStatistic.R")
library(pracma)
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")

#check minimum distance estimator for exponential distribution

set.seed(28072022)
n=100
lambda=2
x=rexp(n,rate=lambda)
interval=c(0.1,5)

distance<-function(x, param){
  distanceExponentialDistribution(x,param)
}

est=minDistanceEstimator(x,distance,lambda, interval)
est
testStatistic(x,distance,est)

# define parameter for the tests
parameter=list()
parameter$x=x
parameter$distance=distance 
parameter$start_value=lambda
parameter$interval=interval
parameter$alpha=0.05
parameter$nSimulation=1000

# check asymptotic test with bootstrap volatility
res1=asymptoticTestBootstrapVariance(parameter)
res1

# check asymptotic test
parameter$standard_deviation=standardDeviationExponential
res2=asymptoticTest(parameter)
res2

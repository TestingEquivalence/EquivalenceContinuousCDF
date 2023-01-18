source("testStatistic.R")
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")
source("empiricalBootstrapTest.R")
source("bootstrapTestTPercentile.R")
source("distributions//dataSetsExponential.R")
source("distributions//mixedDistribution.R")
source("simulation/size.R")
source("simulation/power.R")
source("simulation/simulation.R")
library(MASS)
#library(extraDistr)

# prepare data
parameter=list()
parameter$x=dataSetRearBreaks
parameter$distance=distanceExponentialDistribution
parameter$startValue=1
parameter$interval=c(0.000001,1)
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationExponential
parameter$nSimulation=1000

# compare estimators first
rate.md<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  est=minDistanceEstimator(parameter)
  return(est)
}

est.md=boot(parameter$x,rate.md,R=1000)
est.md$t0
mean(est.md$t)
sd(est.md$t)

rate.ml<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  est=fitdistr(x,"exponential")
  return(est$estimate)
}

est.ml=boot(x,rate.ml,R=1000)
est.ml$t0
mean(est.ml$t)
sd(est.ml$t)

# perform tests

rAT=asymptoticTest(parameter)
rATBV=asymptoticTestBootstrapVariance(parameter)
rEB=empiricalBootstrapTest(parameter)
rPB=tPercentileBootstrapTest(parameter)

rAT$min.epsilon
rATBV$min.epsilon
rEB$min.epsilon
rPB$min.epsilon

# simulate power at estimated distribution
rAT=asymptoticTest(parameter)
parameter$nSimulation=200

test<-function(x){
  parameter$x=x
  # r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtExponential(test,rAT$estimator,n=length(parameter$x), nSimulation = 1000)
fn=paste0("size_tPB_200.csv")
write.csv(res,fn)

# simulate power at random boundary points

parameter$eps=25

test<-function(x){
  parameter$x=x
  r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtBoundary(parameter,test)
write.csv(res,"power_AT_25.csv")


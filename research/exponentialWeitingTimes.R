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
library(extraDistr)

# prepare data

x=dataSetWaitingTimes
interval=c(0.00001,1)
start=1

parameter=list()
parameter$x=x
parameter$distance=distanceExponentialDistribution
parameter$startValue=start
parameter$interval=interval
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationExponential
parameter$nSimulation=200

# compare estimators first
minDistanceEstimator(parameter)

rate.md<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  p=parameter
  p$x=x
  est=minDistanceEstimator(p)
  return(est)
}

set.seed(10071977)
est.md=boot(x,rate.md,R=1000)
est.md$t0
mean(est.md$t)
sd(est.md$t)

rate.ml<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  est=fitdistr(x,"exponential")
  return(est$estimate)
}

set.seed(10071977)
est.ml=boot(x,rate.ml,R=1000)
est.ml$t0
mean(est.ml$t)
sd(est.ml$t)

# perform tests
rAT=asymptoticTest(parameter)
rAT$distance
rAT$min.epsilon

set.seed(10071977)
parameter$nSimulation=10000
rATBV=asymptoticTestBootstrapVariance(parameter)
rATBV$min.epsilon

set.seed(10071977)
rPB=tPercentileBootstrapTest(parameter)
rPB$min.epsilon

parameter$nSimulation=1000
parameter$nSimulationVariance=200
set.seed(10071977)
rPBBV=tPercentileBootstrapTest_BootstrapVariance(parameter)
rPBBV$min.epsilon

# simulate power at estimated distribution
rAT=asymptoticTest(parameter)
parameter$nSimulation=500
parameter$nSimulationVariance=50

res=simulatePowerAtExponential(tPercentileBootstrapTest_BootstrapVariance,rAT$estimator,n=length(parameter$x),
                               nSimulation = 1000, parameter, orderName = "size_PTBV_WeitingTimes_500_50")
fn=paste0("size_PTBV_WeitingTimes_500_50.csv")
write.csv(res,fn)


# simulate power at random boundary points
rAT=asymptoticTest(parameter)

parameter$eps=0.08

parameter$lambda=rAT$estimator

parameter$basePoint<-function(m){
  rexp(m,rate=parameter$lambda)
}

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


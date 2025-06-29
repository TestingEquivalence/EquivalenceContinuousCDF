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
library(poweRlaw)

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
  p=parameter
  p$x=x
  est=minDistanceEstimator(p)
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

est.ml=boot(parameter$x,rate.ml,R=1000)
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
parameter$nSimulation=10000
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
                               nSimulation = 1000, parameter, orderName = "rearBreaks_PTBV_500_50")
fn=paste0("size_PTBV_RearBreaks_500_50.csv")
write.csv(res,fn)

# simulate power at random boundary points
rAT=asymptoticTest(parameter)


parameter$eps=20
parameter$lambda=rAT$estimator

parameter$basePoint<-function(m){
  rexp(m,rate=parameter$lambda)
}

parameter$nSimulation=200
parameter$nSimulationVariance=50

test<-function(x){
  parameter$x=x
  r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  # r = tPercentileBootstrapTest_BootstrapVariance(parameter)
  return(r$min.epsilon)
}

bndPoints=generateBoundaryPoints(nPoints = 100, parameter)
fname="boundary_points.rds"
saveRDS(bndPoints,file=fname)

fname="boundary_points.rds"
bndPoints=readRDS(fname)
res=simulatePowerAtBoundary(parameter,test, bndPoints)
write.csv(res,"power_RearBreaks_tPBT_BV_e20_200_50.csv")
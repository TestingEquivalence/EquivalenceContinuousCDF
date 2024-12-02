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

xmin=1000
x=readVector("C:\\data\\words.csv")
x=x[x>=xmin]
interval=c(1.5,2.5)

parameter=list()
parameter$x=x
distance<-function(x, param){
  distancePowerLaw(x, xmin,param)
}
parameter$distance=distance
parameter$interval=interval
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationPowerLaw
parameter$nSimulation=1000
parameter$startValue=1.9
parameter$xmin=xmin
x=NULL

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
sd(est.md$t)

rate.ml<-function(dat,ind){
  x=dat[ind]
  m = conpl$new(x)
  m$setXmin(parameter$xmin)
  estimate_pars(m)$pars[1]
}

set.seed(10071977)
est.ml=boot(x,rate.ml,R=1000)
est.ml$t0
sd(est.ml$t)

# perform tests
set.seed(10071977)
rAT=asymptoticTest(parameter)
set.seed(10071977)
rATBV=asymptoticTestBootstrapVariance(parameter)
set.seed(10071977)
rPB=tPercentileBootstrapTest(parameter)

rAT$distance
rAT$min.epsilon
rATBV$min.epsilon
rPB$min.epsilon

# simulate power at estimated distribution
rAT=asymptoticTest(parameter)
parameter$nSimulation=200
n=length(parameter$x)

res=simulatePowerAtPowerLaw(test=tPercentileBootstrapTest, 
                            beta=rAT$estimator,xmin=parameter$xmin,n=n, nSimulation = 1000,
                            parameter, orderName = "worldFreqSize")
fn=paste0("size_world_freq_tPBT_200.csv")
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


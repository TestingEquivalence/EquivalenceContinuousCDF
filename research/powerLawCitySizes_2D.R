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

#vector of city sizes in Germany
x=readVector("C:\\data\\list_ge.csv")
x=x[x>=10000]

parameter=list()
parameter$x=x
distance<-function(x, param){
  distancePowerLaw(x, param[1],param[2])
}
parameter$distance=distance
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationPowerLaw
parameter$nSimulation=1000
parameter$startValue=c(20000,2.3)
parameter$lower=c(10000,2)
parameter$upper=c(30000,3)
parameter$control=list(parscale=c(1000,1))

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
sd(est.md$t[,1])
sd(est.md$t[,2])

rate.ml<-function(dat,ind){
  x=dat[ind]
  m = conpl$new(x)
  m$setXmin(est.md$t0[1])
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

test<-function(x){
  parameter$x=x
  r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtPowerLaw(test, rAT$estimator[2],rAT$estimator[1],n=length(parameter$x),nSimulation =1000 )
fn=paste0("size_AT.csv")
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


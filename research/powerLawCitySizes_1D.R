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
parameter=list()
parameter$interval=c(2.01,3)
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationPowerLaw
parameter$nSimulation=1000
parameter$startValue=2.5
parameter$xmin=20000

x=readVector("C:\\data\\list_ge.csv")
parameter$x=x[x>=parameter$xmin]
x=NULL

distance<-function(x, param){
  distancePowerLaw(x, parameter$xmin,param)
}
parameter$distance=distance

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

parameter$nSimulation=1000
set.seed(10071977)
rPB=tPercentileBootstrapTest(parameter)

rAT$distance
rAT$min.epsilon
rATBV$min.epsilon
rPB$min.epsilon

# simulate power at estimated distribution
rAT=asymptoticTest(parameter)
parameter$nSimulation=1000
n=length(parameter$x)

res=simulatePowerAtPowerLaw(test=tPercentileBootstrapTest, 
                            beta=rAT$estimator,xmin=parameter$xmin,n=n, nSimulation = 1000,
                            parameter)
fn=paste0("size_pTBV_1000.csv")
write.csv(res,fn)

# simulate power at random boundary points
rAT=asymptoticTest(parameter)
parameter$eps=200
parameter$beta=rAT$estimator

parameter$basePoint<-function(m){
  rplcon(m,parameter$xmin,parameter$beta)
}

test<-function(x){
  parameter$x=x
  # r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

bndPoints=generateBoundaryPoints(nPoints = 1, parameter)
fname="boundary_points.rds"
saveRDS(bndPoints,file=fname)

fname="boundary_points.rds"
bndPoints=readRDS(fname)
res=simulatePowerAtBoundary(parameter,test, bndPoints)
write.csv(res,"power_tPBT_200.csv")



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

x=readVector("C:\\data\\words.csv")
xlow=500
x=x[x>=xlow]

parameter=list()
parameter$x=x
distance<-function(x, param){
  distancePowerLaw(x, param[1],param[2])
}
parameter$distance=distance
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationPowerLaw
parameter$nSimulation=1000
parameter$startValue=c(1000,2.1)
parameter$lower=c(xlow,1.6)
parameter$upper=c(1500,2.5)
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
rAT=asymptoticTest(parameter)
rAT$distance
rAT$min.epsilon

set.seed(10071977)
parameter$nSimulation=2000
rATBV=asymptoticTestBootstrapVariance(parameter)
rATBV$min.epsilon

set.seed(10071977)
parameter$nSimulation=2000
rPB=tPercentileBootstrapTest(parameter)
rPB$min.epsilon

parameter$nSimulation=1000
parameter$nSimulationVariance=50
set.seed(10071977)
rPBBV=tPercentileBootstrapTest_BootstrapVariance(parameter)
rPBBV$min.epsilon

# simulate power at estimated distribution
rAT=asymptoticTest(parameter)
parameter$nSimulation=200
parameter$nSimulationVariance=50
n=length(parameter$x)

res=simulatePowerAtPowerLaw(test=tPercentileBootstrapTest, 
                            beta=rAT$estimator,xmin=parameter$xmin,n=n, nSimulation = 1000,
                            parameter, orderName = "CitySize1D_tPBT_200_50")  
fn=paste0("size_city_sizes_tPBT_200_50.csv")
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





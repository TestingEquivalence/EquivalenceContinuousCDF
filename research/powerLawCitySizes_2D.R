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

#vector of city sizes in Germany
x=readVector("C:\\data\\list_ge.csv")
xmin=20000
interval=NULL
start=c(2,xmin)

parameter=list()
parameter$x=x
parameter$distance=distancePowerLaw
parameter$startValue=start
parameter$interval=interval
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationExponential()
parameter$nSimulation=1000

# compare estimators first
distance_fixedCutOff<-function(x, param){
  distancePowerLaw(x, xmin,param)
}

distance<-function(x, param){
  distancePowerLaw(x, param[1], param[2])
}

minDistanceEstimator(x,distance_fixedCutOff,param=2, interval=c(1,4))
minDistanceEstimator(x,distance,param=c(20000,2),lower=c(10000,1), 
                     upper=c(30000,3))

rate.md<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  est=minDistanceEstimator(x,distancePowerLaw,start,interval)
  return(est)
}



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

test<-function(x){
  parameter$x=x
  # r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtExponential(test,rAT$estimator,n=length(parameter$x), nSimulation = 1000)
fn=paste0("size_tPB.csv")
write.csv(res,fn)

# simulate power at random boundary points

parameter$eps=20
test<-function(x){
  parameter$x=x
  r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtBoundary(parameter,test)



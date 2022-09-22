source("testStatistic.R")
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")
source("empiricalBootstrapTest.R")
source("bootstrapTestTPercentile.R")
source("distributions//dataSetsExponential.R")
source("simulation/size.R")
library(MASS)


#compare estimators first
x=dataSetWaitingTimes
interval=c(0.00001,1)
start=1

rate.md<-function(dat,ind){
  x=dat[ind]
  # compute minimum distance estimator
  est=minDistanceEstimator(x,distanceExponentialDistribution,start,interval)
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
parameter=list()
parameter$x=x
parameter$distance=distanceExponentialDistribution
parameter$start_value=start
parameter$interval=interval
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationExponential
parameter$nSimulation=1000

asymptoticTest(parameter)$min.epsilon
asymptoticTestBootstrapVariance(parameter)$min.epsilon
empiricalBootstrapTest(parameter)$min.epsilon
tPercentileBootstrapTest(parameter)$min.epsilon


# simulate power at estimated distribution

test<-function(x){
  
}

simulatePowerAtExponential(asymptoticTest,
fn=paste0("size_as_",parameter$n,".csv")
write.csv(res,fn)
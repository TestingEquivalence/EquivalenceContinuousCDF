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

x=dataSetLeukemia
interval=c(0.00001,1)
start=1

parameter=list()
parameter$x=x
parameter$distance=distanceExponentialDistribution
parameter$start_value=start
parameter$interval=interval
parameter$alpha=0.05
parameter$standard_deviation=standardDeviationExponential
parameter$nSimulation=1000

# compare estimators first
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
    r=asymptoticTest(parameter)
  # r=asymptoticTestBootstrapVariance(parameter)
  # r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtExponential(test,rAT$estimator,n=length(parameter$x), nSimulation = 1000)
fn=paste0("size_EB.csv")
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

# 
# # Weibull distribution
# est=fitdistr(parameter$x, "weibull")
# 
# CDF<-function(x){
#   pweibull(x,est$estimate[1],est$estimate[2])
# }
# 
# dst=distanceGeneral(parameter$x,CDF,0,10000)
# dst$value
# 
# wx=rweibull(1e4,est$estimate[1],est$estimate[2])
# parameter$x=wx
# asymptoticTest(parameter)
# 
# rdf<-function(n){
#   rweibull(n,est$estimate[1],est$estimate[2])
# }
# 
# res=simulatePowerAtDistribution(test,rdf,n=length(parameter$x), nSimulation = 1000)
# fn=paste0("size_AT.csv")
# write.csv(res,fn)
# 
# # Lognormal distribution
# est=fitdistr(parameter$x, "log-normal")
# 
# CDF<-function(x){
#   plnorm(x,est$estimate[1],est$estimate[2])
# }
# 
# dst=distanceGeneral(parameter$x,CDF,0,100000)
# dst$value
# 
# wx=rlnorm(1e4,est$estimate[1],est$estimate[2])
# parameter$x=wx
# asymptoticTest(parameter)
# 
# # Gamma distribution
# est=fitdistr(parameter$x, "gamma")
# est
# 
# CDF<-function(x){
#   pgamma(x,est$estimate[1],est$estimate[2])
# }
# 
# dst=distanceGeneral(parameter$x,CDF,0,100000)
# dst$value
# 
# wx=rgamma(1e4,est$estimate[1],est$estimate[2])
# parameter$x=wx
# asymptoticTest(parameter)
# 
# # Half normal distribution
# start=list()
# start$sigma=1
# est=fitdistr(parameter$x, dhnorm,start)
# est
# 
# CDF<-function(x){
#   phnorm(x,est$estimate[1])
# }
# 
# dst=distanceGeneral(parameter$x,CDF,0,100000)
# dst$value
# 
# parameter$x=rhnorm(1e4,est$estimate[1])
# asymptoticTest(parameter)

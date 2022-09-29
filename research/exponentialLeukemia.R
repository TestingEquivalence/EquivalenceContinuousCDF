source("testStatistic.R")
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")
source("empiricalBootstrapTest.R")
source("bootstrapTestTPercentile.R")
source("distributions//dataSetsExponential.R")
source("simulation/size.R")
library(MASS)


#compare estimators first
x=dataSetLeukemia
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
   r=empiricalBootstrapTest(parameter)
  # r=tPercentileBootstrapTest(parameter)
  return(r$min.epsilon)
}

res=simulatePowerAtExponential(test,rAT$estimator,n=length(parameter$x), nSimulation = 1000)
fn=paste0("size_EB.csv")
write.csv(res,fn)

# fitting to the common alternative simulation and power calculation
###################################################################

# Weibull distribution
est=fitdistr(parameter$x, "weibull")

CDF<-function(x){
  pweibull(x,est$estimate[1],est$estimate[2])
}

dst=distanceGeneral(parameter$x,CDF,0,10000)
dst$value

wx=rweibull(1e4,est$estimate[1],est$estimate[2])
parameter$x=wx
asymptoticTest(parameter)

rdf<-function(n){
  rweibull(n,est$estimate[1],est$estimate[2])
}

res=simulatePowerAtDistribution(test,rdf,n=length(parameter$x), nSimulation = 1000)
fn=paste0("size_AT.csv")
write.csv(res,fn)

# Lognormal distribution
est=fitdistr(parameter$x, "log-normal")

CDF<-function(x){
  plnorm(x,est$estimate[1],est$estimate[2])
}

dst=distanceGeneral(parameter$x,CDF,0,100000)
dst$value

wx=rlnorm(1e4,est$estimate[1],est$estimate[2])
parameter$x=wx
asymptoticTest(parameter)

# rdf<-function(n){
#   rweibull(n,est$estimate[1],est$estimate[2])
# }
# 
# res=simulatePowerAtDistribution(test,rdf,n=length(parameter$x), nSimulation = 1000)
# fn=paste0("size_AT.csv")
# write.csv(res,fn)














f<-function(u){
  as.numeric(quantile(x, type = 4, probs = u))
}

t=seq(0,1,0.01)
y=sapply(t,f)
y=as.vector(y)
plot(t, y)

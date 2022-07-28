source("testStatistic.R")
source("asymptoticTest.R")
source("asymptoticTestBootstrapVariance.R")
source("empiricalBootstrapTest.R")
source("BootstrapTestTPercentile.R")

set.seed(18032021)
n=50
x=runif(n)

F<-function(x){
  x
}

parameter=list()
parameter$x=x
parameter$F=F
parameter$alpha=0.05
parameter$nSimulation=1000

asymptoticTest(parameter)

asymptoticTestBootstrapVariance(parameter)
empiricalBootstrapTest(parameter)
tPercentileBootstrapTest(parameter)

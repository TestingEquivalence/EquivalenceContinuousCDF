source("testStatistic.R")
source("asymptoticTestBootstrapVariance.R")
source("asymptoticTest.R")
source("empiricalBootstrapTest.R")
source("bootstrapTestTPercentile.R")


# compare test results of different tests

set.seed(07092022)
parameter=list()

parameter$interval=c(0.1,5)
parameter$nSimulation=1000
parameter$alpha=0.05
parameter$distance=distanceExponentialDistribution
parameter$standard_deviation=standardDeviationExponential
parameter$start_value=1


# tests at the exponential distribution
n=100
lambda=1
parameter$x=rexp(n,lambda)
rAT=asymptoticTest(parameter)
rATBV=asymptoticTestBootstrapVariance(parameter)
rEB=empiricalBootstrapTest(parameter)
rPB=tPercentileBootstrapTest(parameter)

# lambdas
rAT$estimator
rATBV$estimator
rEB$estimator
rPB$estimator

# distances
rAT$distance
rATBV$distance
rEB$distance
rPB$distance

# min. epsilon
rAT$min.epsilon
rATBV$min.epsilon
rEB$min.epsilon
rPB$min.epsilon

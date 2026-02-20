library(parallel)

# Calculate the number of cores
getCluster<-function(){
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores,'SOCK')
  clusterExport(cl,c("rMixed","simulatePowerAtDistribution","parameter","asymptoticTest",
                     "asymptoticTestBootstrapVariance","standardDeviationExponential",
                     "minDistanceEstimator","testStatistic","bootstrapStandardDeviation",
                     "boot","tPercentileBootstrapTest", "parameter", "rplcon","distancePowerLaw",
                     "tPercentileBootstrapTest_BootstrapVariance"))
  
  return(cl)
}

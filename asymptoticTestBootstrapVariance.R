library(boot)

bootstrapStandardDeviation<-function(parameter, results){
  
  #calculate bootstrap volatility
  vol.fun<-function(dat,ind){
    x=dat[ind]
    # compute minimum distance estimator
    est=minDistanceEstimator(x,parameter$distance, 
                             results$estimator,
                             parameter$interval)
    # compute von-Mises distance
    testStatistic(x,parameter$distance,est)
  }
  
  bres=boot(parameter$x,vol.fun,R=parameter$nSimulation)
 
  return(sd(bres$t))
}

asymptoticTestBootstrapVariance<-function(parameter){
  # parameter should contain x, distance, start_value,interval, alpha
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                         parameter$start_value,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #compute standard deviation (square root of the variance)
  
  stDev = bootstrapStandardDeviation(parameter, results = r)
  qt=qnorm(1-parameter$alpha,0,1)
  
  r$epsilon = r$distance + qt*stDev
  r$standard_deviation=stDev
  return(r)
}

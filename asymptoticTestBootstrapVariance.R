library(boot)

bootstrapVolatility<-function(parameter, results){
  
  #calculate bootstrap volatility
  vol.fun<-function(dat,ind){
    x=dat[ind]
    # compute minimum distance estimator
    est=minDistanceEstimator(x,parameter$distance, 
                             results$minimum.distance.estimator,
                             parameter$interval)
    # compute von Mises distance
    testStatistic(x,est)
  }
  
  bres=boot(parameter$x,vol.fun,R=parameter$nSimulation)
 
  return(sd(bres$t))
}

asymptoticTestBootstrapVariance<-function(parameter){
  # parameter should contain x, distance, startValue,interval, alpha
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$minimum.distance.estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                         parameter$startValue,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance)
  
  #compute 
  
  vol = bootstrapVolatility(parameter)
  qt=qnorm(1-parameter$alpha,0,1)
  
  min_eps = r$distance + qt*vol
  return(min_eps)
}

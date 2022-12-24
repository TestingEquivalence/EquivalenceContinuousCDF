library(boot)

bootstrapStandardDeviation<-function(parameter, results){
   
  vol.fun<-function(x){
    
    if (length(parameter$startValue)==1){
      est=minDistanceEstimator(x,parameter$distance, 
                             results$estimator,
                             parameter$interval)
      r= testStatistic(x,parameter$distance,est)
      return(r)
    }
    
    est=minDistanceEstimator(x,parameter$distance,
                             param = parameter$startValue, lower=parameter$lower, 
                             upper=parameter$upper)
    r= testStatistic(x,parameter$distance,est)
    return(r)
   
  }
  
  res=rep(0,parameter$nSimulation)
  m=1
  size=length(parameter$x)
  
  while (m<=parameter$nSimulation) {
    skip_to_next=FALSE
    tryCatch({
      x=sample(parameter$x,size,replace = TRUE)
      res[m]=vol.fun(x)
      m=m+1
    },error = function(e) { skip_to_next=TRUE})
    if(skip_to_next) { next }  
  }
  
  #bres=boot(parameter$x,vol.fun,R=parameter$nSimulation)
 
  return(sd(res))
}

asymptoticTestBootstrapVariance<-function(parameter){
  # parameter should contain x, distance, startValue,interval, alpha, nSimulation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                         parameter$startValue,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #compute standard deviation (square root of the variance)
  
  stDev = bootstrapStandardDeviation(parameter, results = r)
  qt=qnorm(1-parameter$alpha,0,1)
  
  r$min.epsilon = r$distance + qt*stDev
  r$standard_deviation=stDev
  return(r)
}

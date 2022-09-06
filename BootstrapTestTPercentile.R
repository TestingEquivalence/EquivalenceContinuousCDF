tPercentileBootstrapTest<-function(parameter){
  # parameter should contain x, distance, start_value,interval, alpha, nSimulation,  standard_deviation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                   parameter$start_value,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #compute standard deviation (square root of the variance)
  
  stDev = parameter$standard_deviation(parameter, results=r)
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind,parameter){
    x=dat[ind]
    # compute minimum distance estimator
    br=list()
    br$estimator=minDistanceEstimator(x,parameter$distance, 
                             results$estimator,
                             parameter$interval)
    # compute von-Mises distance
    dstBst=testStatistic(x,parameter$distance,br$estimator)
    
    parameter$x=x
    stDevBst=parameter$standard_deviation(parameter, results=br)
    return((dstBst-r$distance)/stDevBst)
  }
  
  res=boot(U,t.fun,R=parameter$nSimulation, parameter)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  r$min.epsilon=r$distance-stDev*qt
  return(r)
}
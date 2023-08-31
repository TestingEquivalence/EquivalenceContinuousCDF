tPercentileBootstrapTest<-function(parameter){
  # parameter should contain x, distance, startValue,interval, alpha, nSimulation,  standard_deviation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #compute standard deviation (square root of the variance)
  
  stDev = parameter$standard_deviation(parameter, results=r)
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    x=dat[ind]
    # compute minimum distance estimator
    p=parameter
    p$x=x
    br=list()
    br$estimator=minDistanceEstimator(p)
    # compute von-Mises distance
    dstBst=testStatistic(x,parameter$distance,br$estimator)
    
    bp=parameter
    bp$x=x
    stDevBst=parameter$standard_deviation(parameter=bp, results=br)
    return((dstBst-r$distance)/stDevBst)
  }
  
  res=boot(parameter$x,t.fun,R=parameter$nSimTPercentile)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  r$min.epsilon=r$distance-stDev*qt
  return(r)
}

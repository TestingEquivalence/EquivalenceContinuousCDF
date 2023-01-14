empiricalBootstrapTest<-function(parameter){
  # parameter should contain x, distance, startValue,interval, alpha, nSimulation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    x=dat[ind]
    p=parameter
    p$x=x
    # compute minimum distance estimator
    est=minDistanceEstimator(p)
    # compute von-Mises distance
    testStatistic(x,parameter$distance,est)
  }
  
  res=boot(parameter$x,t.fun,R=parameter$nSimulation)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  r$min.epsilon=2*r$distance-qt
  return(r)
}

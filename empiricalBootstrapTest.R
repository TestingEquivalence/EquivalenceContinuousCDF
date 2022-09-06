empiricalBootstrapTest<-function(parameter){
  # parameter should contain x, distance, start_value,interval, alpha, nSimulation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                   parameter$start_value,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    x=dat[ind]
    # compute minimum distance estimator
    est=minDistanceEstimator(x,parameter$distance, 
                             results$estimator,
                             parameter$interval)
    # compute von-Mises distance
    testStatistic(x,parameter$distance,est)
  }
  
  res=boot(U,t.fun,R=parameter$nSimulation)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  r$min.epsilon=-qt+2*distance
  return(r)
}
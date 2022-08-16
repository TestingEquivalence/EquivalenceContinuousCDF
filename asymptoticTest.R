

standardDeviationExponential<-function(parameter, results){
  x=parameter$x
  lambda=results$estimator
  n=length(x)
  
  ef<-function(m,lambda,t){
    (m-1)*t-exp(-lambda*t)/lambda
  }
  
  def<-function(m,lambda,s,t){
    ef(m,lambda,t)-ef(m,lambda,s)
  }
  
  pef<-function(i){
    if (i==0){
      return(def(0,lambda,0,x[1]))
    } 
    
    if (i==n){
      return(-ef(1,lambda,x[n]))
    }
    
    return(def(i/n,lambda,x[i],x[i+1]))
  }
  
  
  r=0
  for (i in c(0:n)){
    p2=pef(i)
    
    for (j in c(0:n)){
      p1=min(i,j)/n-i*j/(n^2)
      p3=pef(j)
        
      r=r+p1*p2*p3
    }
  }
  return(4*r)
}

asymptoticTest<-function(parameter){
  # parameter should contain x, distance, start_value,interval, alpha, standard_deviation
  
  # list for results
  r=list()
  
  # compute minimum distance estimator
  r$estimator=minDistanceEstimator(parameter$x,parameter$distance,
                                   parameter$start_value,parameter$interval)
  
  # compute von Mises distance
  r$distance=testStatistic(parameter$x,parameter$distance,  r$estimator)
  
  #compute standard deviation (square root of the variance)
  
  stDev = parameter$standard_deviation(parameter, results=r)
  qt=qnorm(1-parameter$alpha,0,1)
  
  r$epsilon = r$distance + qt*stDev
  r$standard_deviation=stDev
  return(r)
}
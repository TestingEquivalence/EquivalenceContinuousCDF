

standardDeviationExponential<-function(parameter, results){
  x=parameter$x
  x=sort(x)
  lambda=results$estimator
  n=length(x)
  
  ef<-function(m,lambda,t){
    (m-1)*t-exp(-lambda*t)/lambda
  }
  
  def<-function(m,lambda,s,t){
    ef(m,lambda,t)-ef(m,lambda,s)
  }
  
  pef<-function(k){
    if (k==0){
      return(def(0,lambda,0,x[1]))
    } 
    
    if (k==n){
      return(-ef(1,lambda,x[n]))
    }
    
    return(def(k/n,lambda,x[k],x[k+1]))
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
  r=4*r
  r=sqrt(r/n)
  return(r)
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
  
  r$min.epsilon = r$distance + qt*stDev
  r$standard_deviation=stDev
  return(r)
}

standardDeviationPowerLaw<-function(parameter, results){
  x=parameter$x
  x=sort(x)
  if (length(results$estimator)==1){
    xmin=parameter$xmin
    alpha=results$estimator
  }
  else{
    xmin=results$estimator[1]
    alpha=results$estimator[2]
  }
  
  if (alpha==2){
    alpha=alpha+0.0000001
  }
  n=length(x)
  
  ef<-function(m,alpha,t){
    (m-1)*t+t^(2-alpha)/(2-alpha)
  }
  
  def<-function(m,alpha,s,t){
    ef(m,alpha,t)-ef(m,alpha,s)
  }
  
  pef<-function(k){
    if (k==0){
      return(def(0,alpha,xmin,x[1]))
    } 
    
    if (k==n){
      return(-ef(1,alpha,x[n]))
    }
    
    return(def(k/n,alpha,x[k],x[k+1]))
  }
  
  
  r=0
  for (i in c(1:n)){
    p2=pef(i)
    
    for (j in c(1:n)){
      p1=min(i,j)/n-i*j/(n^2)
      p3=pef(j)
      
      r=r+p1*p2*p3
    }
  }
  r=4*r
  r=sqrt(r/n)
  return(r)
}
randomExteriorPoint<-function(parameter){
  repeat{
    n=length(parameter$x)
    x=sample(parameter$x,n, replace = TRUE,)
    
    # linear interpolated empirical CDF function
    f<-function(u){
      as.numeric(quantile(x, type = 4, probs = u))
    }
    
    # generate large sample from f
    nx=runif(n*10)
    nx=sapply(nx, f)
    
    # calculate distance from nx to parametric distribution
    lambda=minDistanceEstimator(nx,parameter$distance, parameter$start_value,parameter$interval)
    
    # compute von Mises distance
    dst=testStatistic(nx,parameter$distance, lambda)
    
    
    
    if (dst>=parameter$eps){
      return(f)
    }
  }
}


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
    
    
    res=list()
    
    if (dst>=parameter$eps){
      res$f=f
      res$lambda=lambda
      return(res)
    }
  }
}

boundaryPoint<-function(parameter, extPoint){
  n=length(parameter$x)
  
  rf<-function(m){
    x=runif(m)
    x=sapply(x, extPoint$f)
    return(x)
  }
  
  re<-function(m){
    rexp(m,rate=extPoint$lambda)
  }
  
  
  target<-function(w){
    nx=rMixed(n*10,w,rf,re)
    
    # calculate distance from nx to parametric distribution
    lambda=minDistanceEstimator(nx,parameter$distance, parameter$start_value,parameter$interval)
    
    # compute von Mises distance
    dst=testStatistic(nx,parameter$distance, lambda)
    
    return(dst-parameter$eps)
  }
  
  res=uniroot(target,c(0,1))
  resf<-function(m){
    rMixed(m,res$root,rf,re)
  }
  
  return(resf)
}


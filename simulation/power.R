randomExteriorPoint<-function(parameter){
  repeat{
    n=length(parameter$x)
    x=sample(parameter$x,n, replace = TRUE,)
    
    # linear interpolated empirical CDF function
    f<-function(u){
      as.numeric(quantile(x, type = 4, probs = u))
    }
    
    # generate large sample from f
    nx=runif(n*100)
    nx=sapply(nx, f)
    
    # calculate distance from nx to parametric distribution
    p=parameter
    p$x=nx
    lambda=minDistanceEstimator(p)
    
    # compute von Mises distance
    dst=testStatistic(nx,parameter$distance, lambda)
    
    
    res=list()
    
    if (dst>=parameter$eps*1.10){
      res$f=f
      # ff=ecdf(parameter$x)
      # h=ecdf(x)
      # plot(ff,col="blue")
      # lines(h,col="red")
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
    set.seed(30112022)
    nx=rMixed(n*100,w,rf,re)
    
    # calculate distance from nx to parametric distribution
    lambda=minDistanceEstimator(nx,parameter$distance, parameter$startValue,parameter$interval)
    
    # compute von Mises distance
    dst=testStatistic(nx,parameter$distance, lambda)
    
    return(dst-parameter$eps)
  }
  
  res=uniroot(target,c(0,1) )
  resf<-function(m){
    rMixed(m,res$root,rf,re)
  }
  
  return(resf)
}

simulatePowerAtBoundary<-function(parameter, test){
  set.seed(12112022)
  exteriorPoints=list()
  bndPoints=list()
  nPoints=100
  
 #generate alternatives from H0
  for (i in c(1:(nPoints))){
    exteriorPoints[[i]]=randomExteriorPoint(parameter)
  }
  
  
  for (i in c(1:nPoints)){
    ls=list()
    ls$rdst=boundaryPoint(parameter,exteriorPoints[[i]])
    ls$nr=i
    bndPoints[[i]]=ls
  }
  
  # cl=getCluster()
  # power=parSapply(cl,bndPoints, simulatePowerAtDistribution, test=test,  
  #                 n=length(parameter$x), nSimulation=1000, eps=parameter$eps)
  # stopCluster(cl)
  
  # power=sapply(bndPoints, simulatePowerAtDistribution, test=test,  
  #              n=length(parameter$x), nSimulation=1000, eps=parameter$eps)

  power=rep(0,nPoints)
  for (i in c(1:nPoints)){
    power[i]=simulatePowerAtDistribution(bndPoints[[i]], test, n=length(parameter$x), nSimulation=1000,
                                  parameter$eps)
    print(i)
  }

  for (i in c(1:nPoints)){
    fname=paste0("r",i,".csv")
    file.remove(fname)
  }
  return(power)
}



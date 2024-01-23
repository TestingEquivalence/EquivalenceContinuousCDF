randomExteriorPoint<-function(parameter){
  repeat{
    n=length(parameter$x)
    x=sample(parameter$x,n, replace = TRUE)
    # x=sample(parameter$x,100, replace = TRUE,)
    
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
    # print(dst)
    
    if (dst>=parameter$eps*1.1){
      # ff=ecdf(parameter$x)
      # h=ecdf(x)
      # plot(ff,col="blue")
      # lines(h,col="red")
      return(f)
    }
  }
}

boundaryPoint<-function(parameter, extPoint){
  n=length(parameter$x)
  
  rf<-function(m){
    x=runif(m)
    x=sapply(x, extPoint)
    return(x)
  }
  
  
  target<-function(w){
    set.seed(30112022)
    nx=rMixed(n*100,w,rf,parameter$basePoint)
    
    p=parameter 
    p$x=nx
    # calculate distance from nx to parametric distribution
    lambda=minDistanceEstimator(p)
    
    # compute von Mises distance
    dst=testStatistic(nx,parameter$distance, lambda)
    
    return(dst-parameter$eps)
  }
  
  res=uniroot(target,c(0,1) )
  resf<-function(m){
    rMixed(m,res$root,rf,parameter$basePoint)
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
    print(paste0("ext point: ",i))
  }
  print("exterior points found!")
  
  for (i in c(1:nPoints)){
    ls=list()
    ls$rdst=boundaryPoint(parameter,exteriorPoints[[i]])
    ls$nr=i
    bndPoints[[i]]=ls
  }
  print("boundary points found")
  
  cl=getCluster()
  power=parSapply(cl,bndPoints, simulatePowerAtDistribution, test=test,
                  n=length(parameter$x), nSimulation=1000, eps=parameter$eps)
  stopCluster(cl)
  
  # power=sapply(bndPoints, simulatePowerAtDistribution, test=test,  
  #              n=length(parameter$x), nSimulation=1000, eps=parameter$eps)

  # power=rep(0,nPoints)
  # for (i in c(1:nPoints)){
  #   power[i]=simulatePowerAtDistribution(bndPoints[[i]], test, n=length(parameter$x), nSimulation=1000,
  #                                 parameter$eps)
  #   print(i)
  # }

  for (i in c(1:nPoints)){
    fname=paste0("r",i,".csv")
    file.remove(fname)
  }
  return(power)
}



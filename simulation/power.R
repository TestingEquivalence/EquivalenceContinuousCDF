randomExteriorPoint<-function(parameter){
  repeat{
    # n=length(parameter$x)
    n=200
    x=sample(parameter$x,n, replace = TRUE)
    
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
    print(dst)
    
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

generateBoundaryPoints<-function(nPoints,parameter){
  
  exteriorPoints=list()
  bndPoints=list()
  
  #generate alternatives from H0
  set.seed(12112022)
  
  for (i in c(1:(nPoints+10))){
    exteriorPoints[[i]]=randomExteriorPoint(parameter)
    print(paste0("ext point: ",i))
  }
  print("exterior points found!")
  
 i=1
 nSuccess=0
 while (nSuccess<nPoints){
  tryCatch({
    ls=list()
    ls$rdst=boundaryPoint(parameter,exteriorPoints[[i]])
    nSuccess=nSuccess+1
    ls$nr=nSuccess
    bndPoints[[nSuccess]]=ls
    print(paste0("bnd point: ",nSuccess))
  }, error = function(e) {
    print(paste0("error: ",i))
  })
   i=i+1
 }
    
  print("boundary points found")
  return(bndPoints)
}

simulatePowerAtBoundary<-function(parameter, test, bndPoints){
  cl=getCluster()
  power=parSapply(cl,bndPoints, simulatePowerAtDistribution, test=test,
                  n=length(parameter$x), nSimulation=1000, eps=parameter$eps)
  stopCluster(cl)
  
  # power=sapply(bndPoints, simulatePowerAtDistribution, test=test,  
  #              n=length(parameter$x), nSimulation=1000, eps=parameter$eps)

  nPoints=length(bndPoints)
  
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



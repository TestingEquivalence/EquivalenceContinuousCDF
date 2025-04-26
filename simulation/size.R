
simulatePowerAtExponential<-function(test, rate, n, nSimulation, parameter, orderName="temp"){
  
  if(!dir.exists(orderName)){
    dir.create(orderName)
  }
  
  # compute points at given exponential distribution
  set.seed(10071977)
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=rexp(n,rate)
  }
  
  res=rep(0,nSimulation)
  # perform simulation
  for (i in c(1:nSimulation)){
    fname=paste0("r",i,".rds")
    fname=file.path(orderName,fname)
    
    if (file.exists(fname)){
      res[i]=readRDS(fname)
      print(i)
      set.seed(as.integer(Sys.time()) %% .Machine$integer.max)
    }
    else {
      parameter$x=sim[[i]]
      testRes=test(parameter)
      res[i]=testRes$min.epsilon
      saveRDS(res[i],fname)
      print(i)
    }
    
  }
  
  unlink(orderName,recursive = TRUE)
  return(res)
}

simulatePowerAtDistribution<-function(point, test, n, nSimulation, eps){
  rdst=point$rdst
  nr=point$nr
  set.seed(10071977)
  fname=paste0("r",nr,".csv")
  
  if (file.exists(fname)){
    s=read.csv(fname)
    return(s$x)
  }
  
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=rdst(n)
  }
  
  res=rep(0,nSimulation)
  for (i in c(1:nSimulation)){
    res[i]=test(sim[[i]])
    # print(i)
  }
  
  r=sum(res<=eps)/nSimulation
  write.csv(r,fname)
  return(r)
}

simulatePowerAtPowerLaw<-function(test, beta, xmin, n, nSimulation,parameter,orderName="temp"){
  set.seed(10071977)
  #prepare
  if(!dir.exists(orderName)){
    dir.create(orderName)
  }
  
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=rplcon(n,xmin,beta)
  }
  
  res=rep(0,nSimulation)
  for (i in c(1:nSimulation)){
    fname=paste0("r",i,".rds")
    fname=file.path(orderName,fname)
    
    if (file.exists(fname)){
      res[i]=readRDS(fname)
      set.seed(as.integer(Sys.time()) %% .Machine$integer.max)
      
    } 
    else 
    {
      parameter$x=sim[[i]]
      testRes=test(parameter)
      res[i]=testRes$min.epsilon
      saveRDS(res[i],fname)
    }
    
    
    print(i)
  }
  
  unlink(orderName,recursive = TRUE)
  return(res)
}
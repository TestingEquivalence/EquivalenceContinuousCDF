
simulatePowerAtExponential<-function(test, rate, n, nSimulation){
  set.seed(10071977)
  
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=rexp(n,rate)
  }
  
  res=rep(0,nSimulation)
  for (i in c(1:nSimulation)){
    res[i]=test(sim[[i]])
    print(i)
  }

  return(res)
}

simulatePowerAtDistribution<-function(test, rdst, n, nSimulation, eps,nr){
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
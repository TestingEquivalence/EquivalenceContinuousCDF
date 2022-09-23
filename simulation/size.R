
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

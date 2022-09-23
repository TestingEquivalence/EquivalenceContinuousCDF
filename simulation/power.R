simulatePower<-function(test, parameter, nSimulation,rDistribution){
  set.seed(10071977)
  
  sim=list()
  for (i in c(1:(nSimulation+10))){
    sim[[i]]=rDistribution(parameter$n)
  }
  
  res=rep(0,nSimulation)
  
  j=1
  i=1
  while(i<=nSimulation){
    parameter$x=sim[[j]]
    tryCatch({
        res[i]=test(parameter)
        print(i)
        i=i+1
      }, 
    error=function(e){
      print("error")
    },finally = {
    })
    print(j)
    j=j+1
  }
  
  return(res)
}
theoreticADDistance<-function(H,G){
  ff<-function(x){
    f<-function(s){
      (H(s)-G(s))^2/(G(s)*(1-G(s)))  
    }
    return(sapply(x, f))
  }
  integrate(ff,0,1)
}

listCDF<-function(){
  ls=list()
  
  ls[[1]]<-function(x){
    pweibull(x,0.8)
  } 
  
  return(ls)
}

listRDG<-function(){
  ls=list()
  
  ls[[1]]<-function(x){
    rbeta(x,0.5,0.5)
  } 
  
  return(ls)
}


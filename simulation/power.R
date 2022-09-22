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
    pbeta(x,0.5,0.5)
  } 
  
  ls[[2]]<-function(x){
    pbeta(x,0.5,1)
  } 
  
  ls[[3]]<-function(x){
    pbeta(x,0.5,1.5)
  } 
  
  ls[[4]]<-function(x){
    pbeta(x,0.5,2)
  } 
  
  ls[[5]]<-function(x){
    pbeta(x,1,1.5)
  } 
  
  ls[[6]]<-function(x){
    pbeta(x,1,2)
  } 
  
  ls[[7]]<-function(x){
    pbeta(x,1.5,1.5)
  } 
  
  ls[[8]]<-function(x){
    pbeta(x,1.5,2)
  } 
  
  ls[[9]]<-function(x){
    pbeta(x,2,2)
  } 
  
  ls[[10]]<-function(x){
    pA(x,0.25)
  }
  
  ls[[11]]<-function(x){
    pA(x,0.5)
  }
  
  ls[[12]]<-function(x){
    pA(x,1.5)
  }
  
  ls[[13]]<-function(x){
    pA(x,2)
  }
  
  ls[[14]]<-function(x){
    pA(x,2.5)
  }
  
  ls[[15]]<-function(x){
    pA(x,3)
  }
  
  ls[[16]]<-function(x){
    pB(x,0.25)
  }
  
  ls[[17]]<-function(x){
    pB(x,0.5)
  }
  
  ls[[18]]<-function(x){
    pB(x,1.5)
  }
  
  ls[[19]]<-function(x){
    pB(x,2)
  }
  
  ls[[20]]<-function(x){
    pB(x,2.5)
  }
  
  ls[[21]]<-function(x){
    pB(x,3)
  }
  
  ls[[22]]<-function(x){
    pC(x,0.25)
  }
  
  ls[[23]]<-function(x){
    pC(x,0.5)
  }
  
  ls[[24]]<-function(x){
    pC(x,1.5)
  }
  
  ls[[25]]<-function(x){
    pC(x,2)
  }
  
  ls[[26]]<-function(x){
    pC(x,2.5)
  }
  
  ls[[27]]<-function(x){
    pC(x,3)
  }
  return(ls)
}

listRDG<-function(){
  ls=list()
  
  ls[[1]]<-function(x){
    rbeta(x,0.5,0.5)
  } 
  
  ls[[2]]<-function(x){
    rbeta(x,0.5,1)
  } 
  
  ls[[3]]<-function(x){
    rbeta(x,0.5,1.5)
  } 
  
  ls[[4]]<-function(x){
    rbeta(x,0.5,2)
  } 
  
  ls[[5]]<-function(x){
    rbeta(x,1,1.5)
  } 
  
  ls[[6]]<-function(x){
    rbeta(x,1,2)
  } 
  
  ls[[7]]<-function(x){
    rbeta(x,1.5,1.5)
  } 
  
  ls[[8]]<-function(x){
    rbeta(x,1.5,2)
  } 
  
  ls[[9]]<-function(x){
    rbeta(x,2,2)
  } 
  
  ls[[10]]<-function(x){
    rA(x,0.25)
  }
  
  ls[[11]]<-function(x){
    rA(x,0.5)
  }
  
  ls[[12]]<-function(x){
    rA(x,1.5)
  }
  
  ls[[13]]<-function(x){
    rA(x,2)
  }
  
  ls[[14]]<-function(x){
    rA(x,2.5)
  }
  
  ls[[15]]<-function(x){
    rA(x,3)
  }
  
  ls[[16]]<-function(x){
    rB(x,0.25)
  }
  
  ls[[17]]<-function(x){
    rB(x,0.5)
  }
  
  ls[[18]]<-function(x){
    rB(x,1.5)
  }
  
  ls[[19]]<-function(x){
    rB(x,2)
  }
  
  ls[[20]]<-function(x){
    rB(x,2.5)
  }
  
  ls[[21]]<-function(x){
    rB(x,3)
  }
  
  ls[[22]]<-function(x){
    rC(x,0.25)
  }
  
  ls[[23]]<-function(x){
    rC(x,0.5)
  }
  
  ls[[24]]<-function(x){
    rC(x,1.5)
  }
  
  ls[[25]]<-function(x){
    rC(x,2)
  }
  
  ls[[26]]<-function(x){
    rC(x,2.5)
  }
  
  ls[[27]]<-function(x){
    rC(x,3)
  }
  return(ls)
}


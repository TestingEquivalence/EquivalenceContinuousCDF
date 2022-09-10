
dMixed<-function(x,w,f,g){
  return(w*f(x)+(1-w)*g(x))
}

pMixed<-function(x,w,H,G){
  return(w*H(x)+(1-w)*G(x))
}

rMixed<-function(n,w,rF,rG){
  vw=rbinom(n=n, size=1,prob=w)
  vF=rF(n)
  vG=rG(n)
  res=vw*vF+(1-vw)*vG
  return(res)
}

boundaryPoint<-function(epsilon,H){
  G<-function(x){
    x
  }
  
  f<-function(w){
    CDF<-function(x){
      pMixed(x,w,H,G)
    }

    dst=theoreticADDistance(CDF,G)$value
    return(dst-epsilon)
  }
  
  res=uniroot(f,c(0,1))
  return(res$root)
}


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

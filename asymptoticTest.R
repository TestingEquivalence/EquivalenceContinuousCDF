

asymptoticVarianceExpomential<-function(parameter, results){
  x=parameter$x
  lambda=results$estimator
  n=length(x)
  
  ef<-function(m,lambda,t){
    (m-1)*t-exp(-lambda*t)/lambda
  }
  
  def<-function(m,lambda,s,t){
    f(m,lambda,t)-f(m,lambda,s)
  }
  
  pef<function(m,i){
    if (i==0){
      return(def(0,lambda,0,x[1]))
    } 
    
    if (i==n){
      return(-ef(1,lambda,x[n]))
    }
    
    return(def(i/n,lambda,x[i],x[i+1]))
  }
  
  
  for (i in c(0:n)){
    p2=0
    
    for (l in c(0:n)){
      p1=min(k,l)/n-k*l/(n^2)
      p3=pf(l,n,U)
        
      r=r+p1*p2*p3
    }
  }
  return(4*r)
}

asymptoticTest<-function(parameter){
  #calculate asymptotic min eps
  U=uTransform(parameter$x, parameter$F)
  n=length(parameter$x)
  vol = asymptoticVariance(U)/n
  vol=sqrt(vol)
  qt=qnorm(1-parameter$alpha,0,1)
  
  distance=testStatisticAD(U)
  min_eps = distance + qt*vol
  return(min_eps)
}
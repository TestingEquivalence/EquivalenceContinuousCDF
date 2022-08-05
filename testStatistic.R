library(minpack.lm)


distanceGeneral<-function(x,F,lower=-Inf,upper=Inf){
  Fn=ecdf(x)
  
  n=length(x)
  ff<-function(x){
    (Fn(x)-F(x))^2
  }
  integrate(ff,lower,upper)
}
distanceExponentialDistribution<-function(x,lambda){
  x=sort(x)
  n=length(x)
  
  g<-function(x,lambda,mu){
    ((mu-1)^2)*x-(2/lambda)*(mu-1)*exp(-lambda*x)-(1/(2*lambda))*exp(-2*x*lambda)
  }
  
  dg<-function(s,t,lambda,mu){
    g(t,lambda,mu)-g(s,lambda,mu)
  }
  
  s=dg(0,x[1],lambda,0)
  
  for (i in c(1:(n-1))) {
    s=s+dg(x[i],x[i+1],lambda, i/n)
  }
  
  s=s+0-g(x[n],lambda,1)
  return(s)
}

distancePowerLaw<-function(x, xmin, alpha){
  if (alpha==2){
    alpha=alpha+0.0000001
  }
  x=sort(x)
  n=length(x)
  
  g<-function(x, m, xmin, alpha){
    x*(m-1)^2-2*x*(m-1)*(x/xmin)^(1-alpha)/(alpha-2)-xmin*(x/xmin)^(3-2*alpha)/(2*alpha-3)
  }
  
  dg<-function(s,t,xmin,alpha,m){
    g(t,m,xmin,alpha)-g(s,m,xmin,alpha)
  }
  
  s=dg(xmin,x[1],xmin,alpha,0)
  
  for (i in c(1:(n-1))) {
    s=s+dg(x[i],x[i+1],xmin,alpha, i/n)
  }
  
  s=s+0-g(x[n],1,xmin,alpha)
  return(s)
}

minDistanceEstimator<-function(x, distance, param,interval=NULL){
  
  dst<-function(param){
    v=distance(x,param)
    return(v)
  }
  
  if (length(param)==1){
    res=optimise(dst,interval)
    return(res)
  }
   
  
  return(res)
}
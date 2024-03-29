library(minpack.lm)


distanceGeneral<-function(x,F,lower=-Inf,upper=Inf){
  Fn=ecdf(x)
  
  n=length(x)
  ff<-function(x){
    (Fn(x)-F(x))^2
  }
  integrate(ff,lower,upper, subdivisions = 1e6)
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
  x=x[x>=xmin]
  n=length(x)
  
  g<-function(x, m){
    x*(m-1)^2-2*x*(m-1)*(x/xmin)^(1-alpha)/(alpha-2)-xmin*(x/xmin)^(3-2*alpha)/(2*alpha-3)
  }
  
  dg<-function(s,t,m){
    g(t,m)-g(s,m)
  }
  
  s=dg(xmin,x[1],0)
  
  for (i in c(1:(n-1))) {
    s=s+dg(x[i],x[i+1],i/n)
  }
  
  s=s+0-g(x[n],1)
  
  if (!is.finite(s)){
    print("distance is not finite")
  }
  
  return(s)
}

minDistanceEstimator<-function(parameter){
  
  dst<-function(param){
    v=parameter$distance(parameter$x,param)
    return(v)
  }
  
  if (length(parameter$startValue)==1){
    res=optimise(dst,parameter$interval)
    return(res$minimum)
  }
   
  res=optim(par = parameter$startValue,fn=dst, method="L-BFGS-B",
            lower=parameter$lower, upper=parameter$upper, control = parameter$control)
  return(res$par)
}

testStatistic<-function(x, distance, param){
  return(distance(x,param))
}

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
  if(alpha==2){
    return(distancePowerLaw2(x,xmin))
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

distancePowerLaw2<-function(x, xmin){
  x=sort(x)
  n=length(x)
  
  g<-function(x, m, xmin){
    2*(m-1)*xmin*log(x)-(-m*m*x*x+2*m*x*x+xmin^2-x^2)/x
  }
  
  dg<-function(s,t,xmin,m){
    g(t,m,xmin)-g(s,m,xmin)
  }
  
  s=dg(xmin,x[1],xmin,0)
  
  for (i in c(1:(n-1))) {
    s=s+dg(x[i],x[i+1],xmin, i/n)
  }
  
  #s=s+0-g(x[n],1,xmin)
  return(s)
}
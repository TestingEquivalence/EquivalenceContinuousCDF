testStatisticGeneral<-function(x,F,lower=-Inf,upper=Inf){
  Fn=ecdf(x)
  
  n=length(x)
  ff<-function(x){
    (Fn(x)-F(x))^2
  }
  integrate(ff,lower,upper)
}
testStatisticExponentialDistribution<-function(x,lambda){
  x=sort(x)
  n=length(x)
  
  g<-function(x,lambda,mu){
    ((mu-1)^2)*x-2*(mu-1)*(1/lambda)*exp(-lambda*x)-(0.5/lambda)*exp(-2*lambda*x)
  }
  
  dg<-function(s,t,lambda,mu){
    g(t,lambda,mu)-g(s,lambda,mu)
  }
  
  s=dg(0,x[1],lambda,0)
  
  for (i in c(1:(n-1))) {
    s=s+dg(x[i],x[i+1],lambda, i/n)
  }
  
  s=s+exp(-lambda*x[n])/lambda
  return(s)
}

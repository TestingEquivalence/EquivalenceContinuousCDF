
m=1
lambda=1
s=1
t=10000

ef<-function(m, lambda,t){
  (m-1)*t-exp(-lambda*t)/lambda
}

def<-function(m,lambda,s,t){
  ef(m,lambda,t)-ef(m,lambda,s)
}

ff<-function(x){
  m-1+exp(-lambda*x)
}

integrate(ff, s,t)
def(m,lambda,s,t)

# -------------------------
# compute theoretic asymptotic variance
library(pracma)

F<-function(x){
  plnorm(x,0,1)  
}

x=rlnorm(1e6,0,1)

lambda=minDistanceEstimator(x, distanceExponentialDistribution,1 ,c(0,20))

G<-function(x){
  pexp(x,rate=lambda)
}

fi<-function(x,y){
  4*(F(x)-G(x))*(F(y)-G(y))*(F(min(x,y))-F(x)*F(y))
}

integral2(fi,0,1000,0,1000)

x=rlnorm(10,0,1)
Fn=ecdf(x)

fin<-function(x,y){
  4*(Fn(x)-G(x))*(Fn(y)-G(y))*(Fn(min(x,y))-Fn(x)*Fn(y))
}

est=minDistanceEstimator(x,distanceExponentialDistribution,1, c(0,10))
res=list()
res$estimator=est
standardDeviationExponential(parameter,res)
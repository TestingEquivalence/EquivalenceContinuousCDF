
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

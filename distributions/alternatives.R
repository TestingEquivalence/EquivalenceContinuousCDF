# The alternative distributions are from the article:
# Stephens, M.A., 1974. EDF statistics for goodness of fit and some comparisons. 
# Journal of the American Statistical Association 69, 730737.

# Distribution A_k

dA<-function(x, k){
  return(k*((1-x)^(k-1)))
}

pA<-function(x,k){
  return(1-(1-x)^k)
}

qA<-function(x,k){
  return(1-(1-x)^(1/k))
}

rA<-function(n,k){
  v=runif(n)
  r=sapply(v, qA,k)
  return(r)
}

# Distribution B_k

pB<-function(x,k){
  if (x<=0.5){
    r=2^(k-1)
    r=r*(x^k)
    return(r)
  }
  else{
    r=2^(k-1)
    r=r*((1-x)^k)
    return(1-r)
  }
}

dB<-function(x,k){
  if (x<=0.5){
    r=2^(k-1)
    r=r*k
    r=r*(x^(k-1))
    return(r)
  }
  else{
    r=2^(k-1)
    r=r*k
    r=r*((1-x)^(k-1))
    return(r)
  }
}

qB<-function(x,k){
  if (x<=0.5){
    r=2^((1-k)/k)
    r=r*(x^(1/k))
    return(r)
  }
  else{
    r=2^((1-k)/k)
    r=r*((1-x)^(1/k))
    return(1-r)
  }
}

rB<-function(n,k){
  v=runif(n)
  r=sapply(v, qB,k)
  return(r)
}

# Distribution C_k

pC<-function(x,k){
  if (x<=0.5){
    r=2^(k-1)
    r=r*((0.5-x)^k)
    return(0.5-r)
  }
  else{
    r=2^(k-1)
    r=r*((x-0.5)^k)
    return(0.5+r)
  }
}

dC<-function(x,k){
  if (x<=0.5){
    r=2^(k-1)
    r=r*((0.5-x)^(k-1))
    return(k*r)
  }
  else{
    r=2^(k-1)
    r=r*((x-0.5)^(k-1))
    return(k*r)
  }
}

qC<-function(x,k){
  if (x<=0.5){
    r=2^((1-k)/k)
    r=r*((0.5-x)^(1/k))
    return(0.5-r)
  }
  else{
    r=2^((1-k)/k)
    r=r*((x-0.5)^(1/k))
    return(0.5+r)
  }
}

rC<-function(n,k){
  v=runif(n)
  r=sapply(v, qC,k)
  return(r)
}

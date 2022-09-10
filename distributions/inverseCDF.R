pinv<-function(x,p,pt){
  return(pt(p(x)))
}
rinv<-function(n,q,qt){
  x=qt(n)
  return(q(x))
}
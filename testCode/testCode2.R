source("testStatistic.R")
source("asymptoticTest.R")

#test variance calculation
n=2
U=c(0.2,0.7)
upf(1,n,U[2])-upf(1,n,U[1])
pf(1,n,U)

set.seed(18032021)
n=5
res=runif(n)
asymptoticVariance(res)

#calculate  variance of test statistics
n=10000

F<-function(x){
  x
}


t<-function(i){
  res=rbeta(n, 1,2)
  res=uTransform(res,F)
  return(testStatisticAD(res))
}

set.seed(18032021)
vt=sapply(c(1:10000), t)
var(vt)*n

# calculate asymptotic variance
F<-function(x){
  x
}


n=1000
av<-function(i){
  res=rbeta(n, 1, 2)
  res=uTransform(res,F)
  print(i)
  asymptoticVariance(res)
}

set.seed(18032021)
vav=sapply(c(1:1000), av)
mean(vav)



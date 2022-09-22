library(parallel)

simulatePowerAtPoint<-function(p,mdr, nSimulation,eps){
  set.seed(01032020)
  psim=list()
  for (i in c(1:nSimulation)){
    psim[[i]]=resample.p(mdr$weights,p)
  }
  
  res=rep(NA,nSimulation)
  
  for (i in c(1:nSimulation)){
    nmdr=updateMinDistanceModel(p=psim[[i]],mdr)
    res[i]=nmdr$min.epsilon
  }
  
  return(sum(res<=eps)/nSimulation)
}


# Calculate the number of cores
getCluster<-function(){
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores,'SOCK')
  clusterExport(cl,c("min_dst_logit","resample.p","updateMinDistanceModel","simulatePowerAtPoint",
                     "logit", "logistic","asymptStDev","asymptoticTest","none",
                     "linearBoundaryPoint","nls.lm","asymptotic",
                     "asymptoticBootstrapVariance","empiricalBootstrap","bootstrapVolatility",
                     "asymptoticTestBootstrapVariance","empiricalBootstrapTest",
                     "tPercentileBootstrapTest","tPercentileBootstrap"))
  
  return(cl)
}
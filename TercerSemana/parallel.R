rm(list=ls())
myfunction=function(j)
{  datos=expand.grid(A=c("a1","a2"),B=c("b1","b2"),rep=c(1,2))
datos=cbind(datos,Y=rnorm(nrow(datos),0,1))
tryCatch(
  {
    model=gls(Y~A+B+A:B,data=datos)
  },error=function(e){model<<-NULL}
)
model
}

N=15000
system.time({
  library(nlme)
  results0=lapply(seq(N),myfunction)
})

library(parallel)
library(snow)
system.time({
  if (.Platform$OS == "unix")
    cl <- makeSOCKcluster(75)
  else
    cl <- makeSOCKcluster(detectCores(logical = TRUE)-1)
  clusterEvalQ(cl,library(nlme))
  clusterExport(cl, ls(all=T), envir = .GlobalEnv)
  results1=parLapply(cl,seq(N),myfunction)
  stopCluster(cl)
})

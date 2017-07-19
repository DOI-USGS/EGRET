## ----message=FALSE-------------------------------------------------------
library(parallel)
detectCores()

## ----eval=FALSE----------------------------------------------------------
#  library(EGRET)
#  library(parallel)
#  
#  eList <- Choptank_eList
#  nCores <- detectCores(logical = FALSE) - 1

## ----eval=FALSE----------------------------------------------------------
#  library(doParallel)
#  library(parallel)
#  
#  cl <- makeCluster(nCores)
#  registerDoParallel(cl)
#  eList <- modelEstimation(eList, verbose = FALSE, run.parallel = TRUE)
#  stopCluster(cl)
#  

## ----eval=FALSE----------------------------------------------------------
#  library(doSNOW)
#  library(parallel)
#  
#  cl <- makeCluster(nCores)
#  registerDoSNOW(cl)
#  eList <- modelEstimation(eList, verbose = FALSE, run.parallel = TRUE)
#  stopCluster(cl)
#  

## ----eval=FALSE----------------------------------------------------------
#  library(doMC)
#  library(parallel)
#  
#  cl <- makeCluster(nCores)
#  registerDoMC(cl)
#  eList <- modelEstimation(eList, verbose = FALSE, run.parallel = TRUE)
#  stopCluster(cl)
#  

## ----eval=FALSE----------------------------------------------------------
#  library(doParallel)
#  library(parallel)
#  library(EGRET)
#  
#  eList <- Choptank_eList
#  
#  nCores <- detectCores(logical = FALSE) - 1
#  
#  system.time({
#    cl <- makeCluster(nCores)
#    registerDoParallel(cl)
#    eList <- modelEstimation(eList, verbose = FALSE, run.parallel = TRUE)
#    stopCluster(cl)
#  })

## ----eval=FALSE----------------------------------------------------------
#  system.time({
#    eList <- modelEstimation(eList, verbose = FALSE, run.parallel = FALSE)
#  })
#  


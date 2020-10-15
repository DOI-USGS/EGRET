#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does three things. 
#' 1) a jack-knife cross-validation of a WRTDS model in which it augments the Sample data frame in the eList,
#' 2) fits the WRTDS model creating the surfaces matrix and places it in the eList
#' (the surfaces matrix expresses the estimated concentration as a function of discharge and time), and 
#' 3) estimates the daily values of concentration and flux, and flow normalized concentration and 
#' flux and places these in the Daily data frame in the eList. 
#' It returns a named list with the following dataframes: Daily, INFO, Sample, and the matrix: surfaces.
#' 
#' @param eList named list with at least the INFO, Daily, and Sample dataframes
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The edgeAdjust method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param verbose logical specifying whether or not to display progress message
#' @param run.parallel logical to run WRTDS in parallel or not
#' @keywords water-quality statistics
#' @export
#' @return eList named list with INFO, Daily, and Sample dataframes, along with the surfaces matrix.
#' @examples
#' eList <- Choptank_eList
#' \donttest{
#' eList <- modelEstimation(eList)
#' }
modelEstimation<-function(eList, 
                          windowY = 7, windowQ = 2, windowS = 0.5,
                          minNumObs = 100, minNumUncen = 50, 
                          edgeAdjust = TRUE, verbose = TRUE,
                          run.parallel = FALSE){

  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
  eList <- setUpEstimation(eList = eList, 
                           windowY = windowY, windowQ = windowQ, windowS = windowS,
                           minNumObs = minNumObs, minNumUncen = minNumUncen,
                           edgeAdjust = edgeAdjust, verbose = verbose)

  if(verbose) cat("\n first step running estCrossVal may take about 1 minute")
  
  Sample1 <- estCrossVal(eList$Daily$DecYear[1],
                       eList$Daily$DecYear[length(eList$Daily$DecYear)], 
                       eList$Sample, 
                       windowY=windowY, windowQ=windowQ, windowS=windowS,
                       minNumObs=minNumObs, minNumUncen=minNumUncen,edgeAdjust=edgeAdjust,
                       verbose=verbose)
  
  eList$Sample <- Sample1
  
  if(verbose) cat("\nNext step running  estSurfaces with survival regression:\n")
  
  surfaces1 <- estSurfaces(eList, 
                         windowY = windowY, windowQ=windowQ, windowS=windowS,
                         minNumObs = minNumObs, minNumUncen = minNumUncen, edgeAdjust = edgeAdjust,
                         verbose = verbose, run.parallel = run.parallel)

  eList$surfaces <- surfaces1
  
  Daily1 <- estDailyFromSurfaces(eList)
  
  eList$Daily <- Daily1
  
  checkSurfaceSpan(eList)
  
  return(eList)
  
}



#' setUpEstimation
#' 
#' Set up the INFO data frame for a modelEstimation
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param verbose logical specifying whether or not to display progress message
#' @param interactive logical deprecated. Use 'verbose' instead
#' @keywords water-quality statistics
#' @export
#' @return eList named list with Daily, Sample, and INFO dataframes.
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' 
setUpEstimation<-function(eList, 
                          windowY=7, windowQ=2, windowS=0.5,
                          minNumObs=100,minNumUncen=50, 
                          edgeAdjust=TRUE, verbose=TRUE, interactive = NULL){

  if(!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(!all(c("Q","LogQ") %in% names(localSample))){
    eList <- mergeReport(INFO=localINFO, Daily = localDaily, Sample = localSample, verbose=verbose)
  }
  
  if(any(localSample$ConcLow[!is.na(localSample$ConcLow)] == 0)){
    stop("modelEstimation cannot be run with 0 values in ConcLow. An estimate of the reporting limit needs to be included. See fixSampleFrame to adjust the Sample data frame")
  }
  
  numDays <- length(localDaily$DecYear)
  DecLow <- localDaily$DecYear[1]
  DecHigh <- localDaily$DecYear[numDays]
    
  surfaceIndexParameters<-surfaceIndex(localDaily)
  localINFO$bottomLogQ<-surfaceIndexParameters[['bottomLogQ']]
  localINFO$stepLogQ<-surfaceIndexParameters[['stepLogQ']]
  localINFO$nVectorLogQ<-surfaceIndexParameters[['nVectorLogQ']]
  localINFO$bottomYear<-surfaceIndexParameters[['bottomYear']]
  localINFO$stepYear<-surfaceIndexParameters[['stepYear']]
  localINFO$nVectorYear<-surfaceIndexParameters[['nVectorYear']]
  localINFO$windowY<-windowY
  localINFO$windowQ<-windowQ
  localINFO$windowS<-windowS
  localINFO$minNumObs<-minNumObs
  localINFO$minNumUncen<-minNumUncen
  localINFO$numDays <- numDays
  localINFO$DecLow <- DecLow
  localINFO$DecHigh <- DecHigh
  localINFO$edgeAdjust <- edgeAdjust
  
  eList$INFO <- localINFO
  
  return(eList)
  
}

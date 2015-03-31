#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does a jack-knife cross-validation of a WRTDS model, fits the surface
#' (concentration as a function of discharge and time), 
#' estimates daily values of concentration and flux, and flow normalized values. 
#' It returns a named list with the following dataframes: Daily, INFO, Sample, and the matrix: surfaces.
#'
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @keywords water-quality statistics
#' @export
#' @return eList named list with Daily, Sample, and INFO dataframes, along with the surfaces matrix.
#' Any of these values can be NA, not all EGRET functions will work with missing parts of the named list eList.
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' EGRETreturn <- modelEstimation(eList)
#' Daily <- EGRETreturn$Daily
#' Sample <- EGRETreturn$Sample
#' INFO <- EGRETreturn$INFO
#' surfaces <- EGRETreturn$surfaces
#'  
#' #Run an estimation adjusting windowQ from default:
#' eList <- modelEstimation(eList, windowQ=5)
#' }
modelEstimation<-function(eList, 
                          windowY=7, windowQ=2, windowS=0.5,
                          minNumObs=100,minNumUncen=50, 
                          edgeAdjust=TRUE){
  # this code is a wrapper for several different functions that test the model, fit a surface,
  #  estimate daily values and flow normalized daily values
  #  and organize these into monthly results
  #  it returns several data frames
  #  all of the data frames are given their "standard" names
  #
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  numDays <- length(localDaily$DecYear)
  DecLow <- localDaily$DecYear[1]
  DecHigh <- localDaily$DecYear[numDays]
  
  cat("\n first step running estCrossVal may take about 1 minute")
  Sample1<-estCrossVal(numDays,DecLow,DecHigh, localSample, 
                       windowY, windowQ, windowS, minNumObs, minNumUncen,
                       edgeAdjust)

  surfaceIndexParameters<-surfaceIndex(localDaily)
  localINFO$bottomLogQ<-surfaceIndexParameters[1]
  localINFO$stepLogQ<-surfaceIndexParameters[2]
  localINFO$nVectorLogQ<-surfaceIndexParameters[3]
  localINFO$bottomYear<-surfaceIndexParameters[4]
  localINFO$stepYear<-surfaceIndexParameters[5]
  localINFO$nVectorYear<-surfaceIndexParameters[6]
  localINFO$windowY<-windowY
  localINFO$windowQ<-windowQ
  localINFO$windowS<-windowS
  localINFO$minNumObs<-minNumObs
  localINFO$minNumUncen<-minNumUncen
  localINFO$numDays <- numDays
  localINFO$DecLow <- DecLow
  localINFO$DecHigh <- DecHigh
  localINFO$edgeAdjust <- edgeAdjust
  
  cat("\nNext step running  estSurfaces with survival regression:\n")
  surfaces1<-estSurfaces(eList, 
                         windowY, windowQ, windowS, minNumObs, minNumUncen, edgeAdjust)

  eList <- as.egret(Daily=localDaily, 
                    Sample=Sample1,
                    INFO=localINFO,
                    surfaces=surfaces1)
  
  Daily1<-estDailyFromSurfaces(eList)
  
  eList <- as.egret(Daily=Daily1, 
               Sample=Sample1,
               INFO=localINFO,
               surfaces=surfaces1)
  
  return(eList)
  
}

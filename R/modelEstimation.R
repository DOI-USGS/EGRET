#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does a jack-knife cross-validation of a WRTDS model, fits the surface
#' (concentration as a function of discharge and time), 
#' estimates daily values of concentration and flux, and flow normalized values. 
#' It returns several data frames or matrices (Daily, INFO, Sample, AnnualResults, and surfaces).
#' AnnualResults is calculated for water year. To use a period of analysis other than water year: AnnualResults<-setupYears(paLong,paStart).
#'
#' @param localDaily data frame containing the daily values, default is Daily
#' @param localSample data frame containing the sample values, default is Sample
#' @param localINFO data frame containing the metadata, default is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param localSurface string specifying the name of the returned surface array. If NA (default), array is returned named 'surface'
#' @param localAnnualResults string specifying the name of the returned AnnualResults calculation. If NA (default), data frame is returned named 'AnnualResults'. setupYears is performed with the water year, and results are saved in this data frame.
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @keywords water-quality statistics
#' @import survival
#' @export
#' @return named list of Daily, Sample, and INFO
#' @examples
#' Daily <- ChopDaily
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' \dontrun{EGRETreturn <- modelEstimation()
#' Daily <- EGRETreturn$Daily
#' Sample <- EGRETreturn$Sample
#' INFO <- EGRETreturn$INFO
#' surfaces <- EGRETreturn$surfaces
#' AnnualResults <- EGRETreturn$AnnualResults
#' }
modelEstimation<-function(localDaily = Daily,localSample = Sample, localINFO = INFO, 
                          windowY=7, windowQ=2, windowS=0.5,minNumObs=100,minNumUncen=50, 
                          localSurface=NA,localAnnualResults=NA,
                          edgeAdjust=TRUE){
  # this code is a wrapper for several different functions that test the model, fit a surface,
  #  estimate daily values and flow normalized daily values
  #  and organize these into monthly results
  #  it returns several data frames
  #  all of the data frames are given their "standard" names
  #
  
  numDays <- length(localDaily$DecYear)
  DecLow <- localDaily$DecYear[1]
  DecHigh <- localDaily$DecYear[numDays]
  
  cat("\n first step running estCrossVal may take about 1 minute")
  Sample1<-estCrossVal(numDays,DecLow,DecHigh, localSample, 
                       windowY, windowQ, windowS, minNumObs, minNumUncen,
                       edgeAdjust)

  surfaceIndexParameters<-surfaceIndex(localDaily = localDaily)
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
  surfaces1<-estSurfaces(localDaily = localDaily, localSample = localSample, 
                         windowY, windowQ, windowS, minNumObs, minNumUncen, edgeAdjust)

  Daily1<-estDailyFromSurfaces(localDaily = localDaily, localINFO = localINFO, localsurfaces = surfaces1)

  return(list(Daily=Daily1, 
              Sample=Sample1,
              INFO=localINFO,
              surfaces=surfaces1))
  
}

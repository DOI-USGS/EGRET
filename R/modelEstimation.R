#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does a jack-knife cross-validation of a WRTDS model, fits the surface
#' (concentration as a function of discharge and time), 
#' estimates daily values of concentration and flux, and flow normalized values. 
#' It returns several data frames or matrices (Daily, INFO, Sample, AnnualResults, and surfaces).
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localSample string specifying the name of the data frame containing the sample values, default is Sample
#' @param localINFO string specifying the name of the data frame containing the metadata, default is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param env environment to set variables in
#' @keywords water-quality statistics
#' @import survival
#' @export
#' @examples
#' Daily <- ChopDaily
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' \dontrun{modelEstimation()}
modelEstimation<-function(localDaily = Daily,localSample = Sample, localINFO = INFO, windowY=10, windowQ=2, windowS=0.5,minNumObs=100,minNumUncen=50, env=parent.frame()){
  # this code is a wrapper for several different functions that test the model, fit a surface,
  #  estimate daily values and flow normalized daily values
  #  and organize these into monthly results
  #  it returns several data frames
  #  all of the data frames are given their "standard" names
  #
  cat("\n first step running estCrossVal may take about 1 minute")
  Sample1<-estCrossVal(localSample = localSample, windowY, windowQ, windowS, minNumObs, minNumUncen)
#   cat("\n done with estCrossVal")
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
  cat("\nNext step running  estSurfaces with survival regression:\n")
  surfaces1<-estSurfaces(localDaily = localDaily, localSample = localSample, windowY, windowQ, windowS, minNumObs, minNumUncen)
#   cat("\nDone with estSurfaces and starting estDailyFromSurface")
#   cat("\nThis can take several minutes but you will see updates")
  Daily1<-estDailyFromSurfaces(localDaily = localDaily, localINFO = localINFO, localsurfaces = surfaces1)
  #   cat("\nDone with estDailyFromSurfaces moving on to calculateMonthlyResults")
  #   MonthlyResults1<-calculateMonthlyResults(localDaily = Daily1)
  #   cat("\nDone with calculateMonthlyResults")
  env$Daily<-Daily1
  env$INFO<-localINFO
  env$Sample<-Sample1
  env$surfaces<-surfaces1
  localpaLong <- ifelse("paLong" %in% colnames(localINFO),localINFO$paLong, 12)
  localpaStart <- ifelse("paStart" %in% colnames(localINFO),localINFO$paStart, 10)
  env$AnnualResults<-setupYears(paLong = localpaLong, paStart =localpaStart, localDaily = localDaily)
  #  env$MonthlyResults<-MonthlyResults1
  cat("\nDone with modelEstimation,\nnow do AnnualResults<-setupYears()\nor if using a period of analysis other than Water Year specify the arguments paStart and paLong in call to setupYears ")
}

#' Estimates all daily values of Concentration, and Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these two time series 
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augemented version of the Daily data frame, which is returned.
#'    This code is identical to estDailyFromSurfaces but it lacks the flow normalization process.
#'    The exclusion of the flow-normalization process saves a large amount of computer time.
#'
#' @param eList named list with at least Daily and INFO dataframes, and surface matrix.
#' @keywords water-quality statistics
#' @return localDaily character specifying the name of the data frame containing the daily values and these esimates
#' @export
#' @examples
#' eList <- Choptank_eList
#' Daily <- estDailyWithoutNormalization(eList)
estDailyWithoutNormalization<-function(eList) {
  # this function uses the surfaces that have been calulated based on the sample data
  # and fills in the individual estimates using bilinear interpolation off these surfaces
  # it produces estimates of ConcDay, and FluxDay,
  # it does not do Flow Normalization
  # Because it doesn't do Flow Normalization it is much faster than
  # estDailyFromSurfaces - The code is identical to the first part of estDailyFromSurfaces 
  # these are appended to the data frame Daily, which is returned
  #
  
  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  localsurfaces <- getSurfaces(eList)
  
  numDays<-length(localDaily$LogQ)
  # we need to add an extra column to Daily to handle leap year for flow normalization
  # the 366'th day of the year is treated as an extra day 365
  localDaily$Leap<-ifelse(localDaily$Day<365,localDaily$Day,365)
  # set up the grid values
  bottomLogQ<-localINFO$bottomLogQ
  stepLogQ<-localINFO$stepLogQ
  bottomYear<-localINFO$bottomYear
  stepYear<-localINFO$stepYear
  t0<-trunc((localDaily$DecYear-bottomYear)/stepYear)+1
  t1<-t0+1
  tat0<-((t0-1)*stepYear)+bottomYear
  tf<-(localDaily$DecYear - tat0)/stepYear
  q0<-trunc((localDaily$LogQ - bottomLogQ)/stepLogQ)+1
  q1<-q0+1
  qat0<-((q0-1)*stepLogQ)+bottomLogQ
  qf<-(localDaily$LogQ - qat0)/stepLogQ
  result<-array(0,dim=c(numDays,3))
  for(i in 1:numDays) {
    for(j in 1:3){
      f00<-localsurfaces[q0[i],t0[i],j]
      f01<-localsurfaces[q1[i],t0[i],j]
      f10<-localsurfaces[q0[i],t1[i],j]
      f11<-localsurfaces[q1[i],t1[i],j]
      b1<-f00
      b2<-f01-f00
      b3<-f10-f00
      b4<-f00-f10-f01+f11
      result[i,j]<-b1+b2*qf[i]+b3*tf[i]+b4*qf[i]*tf[i]
    }
  }
  localDaily$yHat<-result[,1]
  localDaily$SE<-result[,2]
  localDaily$ConcDay<-result[,3]
  localDaily$FluxDay<-result[,3]*localDaily$Q*86.40
  #     These two lines are here just in case there were values already in FNConc and FNFlux
  localDaily$FNConc<-rep(NA,numDays)
  localDaily$FNFlux<-rep(NA,numDays)
  return(localDaily)
}
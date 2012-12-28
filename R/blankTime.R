#'    Deletes the computed values during periods of time when there is no sample data
#'
#'  This function is used when the data analyst believes that a gap in the sample data record
#'  is so long that estimates during that period are not reliable. 
#'  This is only unsed for periods of several years in duration. 
#'  For this period, the values of Conc, Flux, FNConc and FNFlux are all converted to NA. 
#'
#' @param startBlank string specifying starting date of blank period, input in quotes in yyyy-mm-dd format
#' @param endBlank string specifying the ending date of blank period, input in quotes in yyyy-mm-dd format
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return localDaily Daily data frame returned with NA's in sample gap
#' @export
#' @examples
#' startBlank = "2004-10-01"
#' endBlank = "2006-09-30"
#' Daily <- exDailyEnd
#' Daily <- blankTime(startBlank, endBlank)
blankTime<-function(startBlank, endBlank, localDaily = Daily) {
  # this function is used after the model estimation is done
  # it can be used more than once, for multiple blank periods
  # the startBlank and endBlank variables must be in quotes and in yyyy-mm-dd format
  # it is a good idea for startBlank to be the first day of some month
  # it is a good idea for endBlank to be the last day of some month
  # it is also a good idea for these to cover entire water years
  #
  #  code needs to include error handling for inputs
  startBlank<-as.Date(startBlank)
  endBlank<-as.Date(endBlank)
  startJulian<-as.numeric(julian(startBlank,origin=as.Date("1850-01-01")))
  endJulian<-as.numeric(julian(endBlank,origin=as.Date("1850-01-01")))
  bad<-ifelse(localDaily$Julian>=startJulian&localDaily$Julian<=endJulian,TRUE,FALSE)
  localDaily$ConcDay<-ifelse(bad,NA,localDaily$ConcDay)
  localDaily$FluxDay<-ifelse(bad,NA,localDaily$FluxDay)
  localDaily$FNConc<-ifelse(bad,NA,localDaily$FNConc)
  localDaily$FNFlux<-ifelse(bad,NA,localDaily$FNFlux)
  return(localDaily)		
}
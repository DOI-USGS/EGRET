#'  Deletes the computed values during periods of time when there is no sample data
#'
#'  This function is used when the data analyst believes that a gap in the sample data record
#'  is so long that estimates during that period are not reliable. 
#'  This is only used for periods of several years in duration. 
#'  For this period, the values of Conc, Flux, FNConc and FNFlux are all converted to NA. 
#'
#' @param startBlank character specifying starting date of blank period, input in quotes in yyyy-mm-dd format
#' @param endBlank character specifying the ending date of blank period, input in quotes in yyyy-mm-dd format
#' @param eList named list with at least the Daily dataframe
#' @keywords water-quality statistics
#' @return eList named list with modified Daily data frame.
#' @export
#' @examples
#' startBlank = "2004-10-01"
#' endBlank = "2006-09-30"
#' eList <- Choptank_eList
#' eList <- blankTime(eList, startBlank, endBlank)
blankTime<-function(eList, startBlank, endBlank) {
  
  localDaily <- getDaily(eList)
  
  startBlank<-as.Date(startBlank)
  endBlank<-as.Date(endBlank)
  startJulian<-as.numeric(julian(startBlank,origin=as.Date("1850-01-01")))
  endJulian<-as.numeric(julian(endBlank,origin=as.Date("1850-01-01")))
  bad<-ifelse(localDaily$Julian>=startJulian&localDaily$Julian<=endJulian,TRUE,FALSE)
  localDaily$ConcDay<-ifelse(bad,NA,localDaily$ConcDay)
  localDaily$FluxDay<-ifelse(bad,NA,localDaily$FluxDay)
  localDaily$FNConc<-ifelse(bad,NA,localDaily$FNConc)
  localDaily$FNFlux<-ifelse(bad,NA,localDaily$FNFlux)
  
  eList$Daily <- localDaily
  
  return(eList)		
}
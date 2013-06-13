#' Creates a subset of the Daily data frame that only contains data for the specified period of analysis
#'
#'  Function is not called by user, but is called by plotSDLogQ
#'
#' @param paLong a numeric value for the length of the period of Analysis, must be an integer from 1 to 12
#' @param paStart a numeric value for the starting month of the period of analysis, must be an integer from 1 to 12
#' @param localDaily a character string specifying the name of the daily data frame to be used
#' @keywords statistics streamflow
#' @export
#' @return localDaily a data frame containing the daily data but only for the period of analysis (not all months)
#' @examples
#' Daily <- ChopDaily
#' DailySubset <- selectDays(1,3)
selectDays<-function(paLong,paStart,localDaily=Daily) {
  numDays<-length(localDaily$Q)
  goodMonth<-rep(FALSE,12)
  for (iMonth in 1:paLong) {
    monthInd <- paStart + iMonth -1
    monthInd <- if(monthInd > 12) monthInd-12 else monthInd
    goodMonth[monthInd] = TRUE
  }
  keep<-rep(TRUE,numDays)
  for(i in 1:numDays) {
    keep[i]<-ifelse(goodMonth[localDaily$Month[i]],TRUE,FALSE)
  }
  localDaily<-data.frame(localDaily,keep)
  localDaily<-subset(localDaily,keep==TRUE)
  return(localDaily)
}
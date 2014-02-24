#' A special version of setPA for use inside the plot15 function
#'
#' Part of the flowHistory system
#' Users should not need to call this function
#'
#' @param paStart A numeric value for the starting month of the Period of Analysis, default is 10
#' @param paLong A numeric value for the length of the Period of Analysis in months, default is 12
#' @param window A numeric value for the half-width of a smoothing window for annual streamflow values, default is 30
#' @param localINFO data frame that contains the metadata, default is INFO
#' @keywords statistics streamflow
#' @export
#' @return localInfo A data frame containing the metadata
#' @examples
#' INFO <- ChopINFO
#' INFO <- setPAx(paStart=12, paLong=3)
setPAx<-function (paStart = 10, paLong = 12, window = 30, localINFO = INFO) 
{
  localINFO$paStart <- paStart
  localINFO$paLong <- paLong
  localINFO$window <- window
  return(localINFO)
}
#' Axis generation for discharge
#'
#' Discharge axis tick generation
#'
#' @param x vector specifying discharge
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Daily <- exDaily
#' qUnit <- 1
#' dischargeAxis(Daily$Q,qUnit)
dischargeAxis <- function(x, qUnit, tinyPlot=FALSE){
  
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  
  xMin<-0.95*min(x)
  xMax<-1.05*max(x)
  xTicks<-if(tinyPlot) logPretty1(xMin,xMax) else logPretty3(xMin,xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  return(list(xMin=xMin,xMax=xMax,xTicks=xTicks,xLeft=xLeft,xLab=xLab))
}
#' Axis generation for log discharge
#'
#' Discharge axis tick generation
#'
#' @param x vector specifying discharge
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param padPercent number
#' @param tinyPlot logical
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Daily <- exDaily
#' qUnit <- 1
#' dischargeLogAxis(Daily$Q,qUnit)
dischargeLogAxis <- function(x,qUnit,padPercent=5,tinyPlot=FALSE){
  
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  xMin <- lowerMagnification*min(x)
  xMax <- upperMagnification*max(x)
  
  xTicks <- if(tinyPlot) {
    logPretty1(xMin,xMax) 
  } else {
    logPretty3(xMin,xMax)
  }
  
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  
  return(list(xTicks=xTicks,xLeft=xLeft,xRight=xRight,xLab=xLab))
}
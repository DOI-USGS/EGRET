#' Axis generation for log discharge
#'
#' Discharge axis tick generation
#'
#' @param qMax number
#' @param qActual vector
#' @param yPlotMin number
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param padPercent number
#' @param tinyPlot logical
#' @param runoff logical
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Daily <- exDaily
#' qUnit <- 1
#' qActual <- Daily$Q
#' qMax <- max(qActual)
#' dischargeYAxis(qMax,qActual,qUnit)
dischargeYAxis <- function(qMax,qActual,qUnit, runoff=FALSE, tinyPlot=FALSE,
                           yPlotMin=0,padPercent=5){
  
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  
  nTicks<-if(tinyPlot) 5 else 8
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  yTop<-if(is.na(qMax)) upperMagnification*max(qActual,na.rm=TRUE) else qMax
  yLow<-if(is.na(yPlotMin)) lowerMagnification*min(qActual,na.rm=TRUE) else yPlotMin
  
  ySpan<-c(yLow,yTop)
  yTicks<-pretty(ySpan,n=nTicks)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  yLab<-if(runoff) "mm/day" else qUnit@qUnitExpress
  
  return(list(yLab=yLab,yTop=yTop,yBottom=yBottom,yTicks=yTicks))
}
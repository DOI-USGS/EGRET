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
#' x <- Daily$Q
#' max <- max(x)
#' min <- 0
#' generalAxis(x, max, min)
#' min <- min(x)
#' generalAxis(x, max, min, log=TRUE)
generalAxis <- function(x,max,min,log=FALSE, tinyPlot=FALSE,padPercent=5){
  
  nTicks<-if(tinyPlot) 5 else 8
  
  if (length(max)>1) {
    max<-max(max) + 0.2
  }
  
  if (length(min)>1) {
    min<-min(min) - 0.2
  }
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  high <- if(is.na(max)) upperMagnification*max(x,na.rm=TRUE) else max
  low <- if(is.na(min)) lowerMagnification*min(x,na.rm=TRUE) else min
  span<-c(low,high)  
  ticks<-if (log){
    if(tinyPlot) {
      logPretty1(min,max) 
    } else {
      logPretty3(min,max)
    }
  } else {
    pretty(span,n=nTicks)
  }  
  
  numTicks<-length(ticks)
  
  bottom<-ticks[1]
  top<-ticks[numTicks]
  
  return(list(ticks=ticks, bottom=bottom, top=top))
}
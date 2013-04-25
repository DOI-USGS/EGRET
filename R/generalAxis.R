#' Axis generation for log discharge
#'
#' Discharge axis tick generation
#'
#' @param x vector to create scale about
#' @param max number maximum value on returned scale
#' @param min number minimum value on returned scale
#' @param log logical whether or not to return a log scale
#' @param tinyPlot logical
#' @param padPercent number used to pad the max and min if not specified
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
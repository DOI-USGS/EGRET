#' Axis generation for log discharge
#'
#' Discharge axis tick generation
#'
#' @param x vector to create scale about
#' @param maxVal number maximum value on returned scale
#' @param minVal number minimum value on returned scale
#' @param logScale logical whether or not to return a log scale
#' @param tinyPlot logical
#' @param padPercent number used to pad the max and min if not specified
#' @param max_offset maximum value offset
#' @param min_offset minimum value offset
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
generalAxis <- function(x, maxVal, minVal,logScale=FALSE, tinyPlot=FALSE,padPercent=5, max_offset=0, 
                        min_offset=0){
  
  nTicks<-if(tinyPlot) 5 else 8
  
#   if (length(maxVal)>1) {
#     maxVal<-max(maxVal) + 0.2
#   }
#   
#   if (length(maxVal)>1) {
#     maxVal<-min(maxVal) - 0.2
#   }
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  high <- if(is.na(maxVal)) {upperMagnification*max(x,na.rm=TRUE) + max_offset} else {maxVal}
  low <- if(is.na(minVal)) {lowerMagnification*min(x,na.rm=TRUE) - min_offset} else {minVal}       
  
#   if (year_search) {
#     high <- if(is.na(maxVal)) {(max(x,na.rm=TRUE) + max_offset)} else {maxVal}
#     low <- if(is.na(minVal)) {(min(x,na.rm=TRUE) - min_offset)} else {minVal}       
#   } else {
#     high <- if(is.na(maxVal)) {upperMagnification*(max(x,na.rm=TRUE) + max_offset)} else {maxVal}
#     low <- if(is.na(minVal)) {lowerMagnification*(min(x,na.rm=TRUE) - min_offset)} else {minVal}
#   }
  
  span<-c(low,high)
  
  ticks<-if (logScale){
    if(tinyPlot) {
      logPretty1(low,high) 
    } else {
      logPretty3(low,high)
    }
  } else {
    pretty(span,n=nTicks)
  }  
  
  numTicks<-length(ticks)
  bottom<-ticks[1]
  top<-ticks[numTicks]
  
  return(list(ticks=ticks, bottom=bottom, top=top))
}

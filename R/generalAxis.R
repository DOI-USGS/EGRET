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
#' @param concentration logical if concentration=TRUE, labels returned as concentration units, otherwise flux units.
#' @param units character concentration units. Typically found in INFO$param.units.
#' @param prettyDate logical use 'pretty' limits for date axis if TRUE, or force the yearStart/yearEnd as limits if FALSE
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' INFO <- getInfo(eList)
#' x <- Daily$Q
#' max <- max(x)
#' min <- 0
#' units <- INFO$param.units
#' generalAxis(x, max, min, units)
#' min <- min(x)
#' generalAxis(x, max, min, units, log=TRUE)
generalAxis <- function(x,
                        maxVal,
                        minVal,
                        units = NA,
                        logScale = FALSE,
                        tinyPlot = FALSE,
                        padPercent = 5,
                        concentration = TRUE,
                        prettyDate = TRUE) {
  
  
  nTicks<-if(tinyPlot) 5 else 8
  
  upperMagnification <- 1 + (padPercent / 100)
  lowerMagnification <- 1 - (padPercent / 100)
  
  if (max(x,na.rm=TRUE) > 0){
    high <- if(is.na(maxVal)) {upperMagnification*max(x,na.rm=TRUE)} else {maxVal}
  } else {
    high <- if(is.na(maxVal)) {lowerMagnification*max(x,na.rm=TRUE)} else {maxVal}
  }
  
  if (min(x,na.rm=TRUE) > 0){
    low <- if(is.na(minVal)) {lowerMagnification*min(x,na.rm=TRUE)} else {minVal}
  } else {
    low <- if(is.na(minVal)) {upperMagnification*min(x,na.rm=TRUE)} else {minVal}
  }
   
  if(concentration){
    if (tinyPlot){
      label <- paste("Conc. (",units,")",sep="")
    } else {
      label <- paste("Concentration in", units)
    }
  } else {
    label <- ""
  }
  
  span <- c(low, high)
  
  ticks <- if (logScale) {
    if (tinyPlot) {
      logPretty1(low, high)
    } else {
      logPretty3(low, high)
    }
  } else {
    pretty(span, n = nTicks)
  }
  
  numTicks <- length(ticks)
  bottom <- ticks[1]
  top <- ticks[numTicks]
  
  if(!prettyDate){
    bottom <- minVal
    top <- maxVal
    ticks[1] <- minVal
    ticks[length(ticks)] <- maxVal
  }
  
  return(list(ticks=ticks, bottom=bottom, top=top, label=label))
}

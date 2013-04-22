#' Axis generation for concentration
#'
#' Concentration axis tick generation
#'
#' @param ConcLow vector specifying ConcLow
#' @param ConcHigh vector
#' @param ConcAve vector
#' @param ConcMax number
#' @param ConcMin number
#' @param padPercent number
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' ConcMin <- min(Sample$ConcAve)
#' ConcMax <- max(Sample$ConcAve)
#' with(Sample, concentrationAxis(ConcLow, ConcHigh, ConcAve, ConcMax, ConcMin))
concentrationAxis <- function(ConcLow, ConcHigh, ConcAve, ConcMax, ConcMin,padPercent=5){
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  yLow <- ConcLow
  yHigh <- ConcHigh
  maxYHigh <- if(is.na(ConcMax)) upperMagnification*max(yHigh) else ConcMax
  minYLow <- if(is.na(ConcMin)) lowerMagnification*min(ConcAve) else ConcMin
  #yTicks <- logPretty3(minYLow,maxYHigh)
  #yBottom <- yTicks[1]
  #yTop <- yTicks[length(yTicks)]
  axis_info <- generalAxis(x=yHigh, min=minYLow, max=maxYHigh, log=TRUE)
  return(list(yTop=axis_info$top,yBottom=axis_info$bottom,yTicks=axis_info$ticks))

}
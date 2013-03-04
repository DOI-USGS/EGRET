#' Generic plotting function to create censored line segments
#'
#' Basic plotting framework for EGRET dot plots. Graphical parameters default to values that work well with most plots, but all can be re-assigned.
#' See ?par for complete definitions of most optional input variables.
#'
#' @param x vector specifying the x data (required)
censoredSegments <- function(yBottom,yLow){
  yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  numSamples<-length(x)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(x[uncensoredIndex],yLowVal[uncensoredIndex],x[uncensoredIndex],yHigh[uncensoredIndex])  
}
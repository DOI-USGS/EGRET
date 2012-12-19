#' Sets up tick marks for an axis with a log scale, where the graph is small
#'
#' Axis tick marks for a log scale for cases where the data cover many orders of magnitude
#' and the graph is small. 
#' These tick marks are designed to progress by factors of 10.
#' 
#' @param xMin A numeric value for the minimum value to be plotted, it must be >0
#' @param xMax A numeric value for the maximum value to be plotted, it must be >xMax
#' @keywords statistics graphics
#' @export
#' @return xTicks A vector representing the values for each of the tick marks
#' @examples
#' xMin<-0.7
#' xMax<-990000
#' logPretty1(xMin,xMax)
#' xMin<-3
#' xMax<-15
#' logPretty1(xMin,xMax)
logPretty1<-function(xMin,xMax) {
  #This function sets up tick marks for a log scale
  #All the tick marks are multiples of 10
  xFirst<-floor(log(xMin,10))
  xLast<-ceiling(log(xMax,10))
  xTicks<-seq(xFirst,xLast)
  xTicks<-10^xTicks
  return(xTicks)
}
#' Sets up tick marks for an axis for a graph with an arithmetic scale which starts at zero
#'
#' Axis tick marks that run from zero to some specified maximum, creates about 4 to 8 ticks marks.
#' 
#' @param yMax A numeric value for the maximum value to be plotted, it must be >0
#' @keywords statistics graphics
#' @export
#' @return yTicks A numeric vector representing the values for each of the tick marks
#' @examples
#' yTicks <- yPretty(7.8)
#' yTicks <- yPretty(125)
yPretty<-function(yMax) {
  #This function sets up the ticks on the y axis
  #To run from zero, to some reasonable maximum  
  yPair<-c(0,yMax)
  yTicks<-pretty(yPair,n=5)
  return(yTicks)
}
#' Sets up tick marks for an axis with a log scale, where the graph is small
#'
#' Axis tick marks for a log scale for cases where the data cover many orders of magnitude
#' and the graph is small. 
#' These tick marks are designed to progress by factors of 10.
#' 
#' @param xMin A numeric value for the minimum value to be plotted, it must be > 0
#' @param xMax A numeric value for the maximum value to be plotted, it must be > xMax
#' @keywords statistics graphics
#' @export
#' @return xTicks A vector representing the values for each of the tick marks
#' @examples
#' xMin <- 0.7
#' xMax <- 990000
#' logPretty1(xMin, xMax)
#' xMin <- 3
#' xMax <- 15
#' logPretty1(xMin, xMax)
logPretty1 <- function(xMin, xMax) {
  #This function sets up tick marks for a log scale
  #All the tick marks are multiples of 10
  xFirst <- floor(log(xMin, 10))
  xLast <- ceiling(log(xMax, 10))
  xTicks <- seq(xFirst, xLast)
  xTicks <- 10 ^ xTicks
  return(xTicks)
}

#' Sets up tick marks for an axis with a log scale
#'
#' Axis tick marks for a log scale. 
#' These tick marks are designed to progress with 3 tick marks for every factor of 10. 
#' For example: 2,5,10,20,50,100,200,500. 
#' 
#' @param xMin A numeric value for the minimum value to be plotted, it must be >0
#' @param xMax A numeric value for the maximum value to be plotted, it must be >xMax
#' @keywords statistics graphics
#' @export
#' @return xTicks A vector representing the values for each of the tick marks
#' @examples
#' logPretty3(0.7, 990000)
#' logPretty3(3, 15)
logPretty3<-function(xMin,xMax) {
  #This function sets up tick marks for a log scale
  #It does so with tick marks at 1, 2, 5, 10, 20,...
  #It is assumed that xMin and xMax already extend about 5%
  #  above and below the actual data range
  #    this next line is just to handle a small numerical problem
  xMin <- xMin * 1.00001
  xFirst <- floor(log(xMin, 10))
  xLast <- ceiling(log(xMax, 10))
  cycles <- xLast - xFirst + 1
  trio <- c(0, log(2, 10), log(5, 10))
  xTicks <- xFirst + trio
  top <- cycles - 2
  for(icycle in 1:top) {
    newTrio<-xFirst+icycle+trio
    xTicks<-c(xTicks,newTrio)
  }
  xTicks<-c(xTicks,xLast)
  numTicks<-length(xTicks)
  shortTicks<-numTicks-4
  xTicks<-if(cycles<=2) xTicks[1:shortTicks] else xTicks
  
  #  now cut it back on each end
  keepLow <- ifelse(log(xMin, 10) < xTicks, 1, 0)
  keepHigh <- ifelse(log(xMax, 10) > xTicks, 1, 0)
  top <- length(keepLow) - 1
  kLow <- keepLow
  kHigh <- keepHigh
  for(i in 1:top) {
    kLow[i] <- keepLow[i + 1]
  }
  for(i in 1:top) {
    kHigh[i + 1] <- keepHigh[i]
  }
  keep <- kLow * kHigh
  trim <- data.frame(xTicks, keep)
  trim <- subset(trim, keep > 0)
  xTicks <- 10 ^ trim$xTicks
  return(xTicks)
}
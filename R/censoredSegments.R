#' Generic plotting function to create censored line segments
#'
#' Basic plotting framework for EGRET dot plots. Graphical parameters default to values that work well with most plots, but all can be re-assigned.
#' See ?par for complete definitions of most optional input variables.
#'
#' @param yBottom number specifying minimum flux (required)
#' @param yLow vector specifying the x data (required), such as ConcLow
#' @param yHigh vector specifying the x data (required), such as ConcHigh
#' @param x vector x data (required)
#' @export
#' @examples
#' x <- c(1,2,3,4,5,6)
#' y <- c(1,3,4,3.3,4.4,2)
#' xlim <- c(min(x)*.75,max(x)*1.25)
#' ylim <- c(0,1.25*max(y))
#' xlab <- "Date"
#' ylab <- "Concentration"
#' xTicks <- pretty(xlim)
#' yTicks <- pretty(ylim)
#' genericEGRETDotPlot(x=x, y=y, 
#'                     xlim=xlim, ylim=ylim,
#'                     xlab=xlab, ylab=ylab,
#'                     xTicks=xTicks, yTicks=yTicks,
#'                     plotTitle="Test"
#' )
#' yBottom <- 0
#' yLow <- c(NA,3,4,3.3,4.4,2)
#' yHigh <- c(1,3,4,3.3,4.4,2)
#' Uncen <- c(0,1,1,1,1,1)
#' censoredSegments(yBottom=yBottom,yLow=yLow,yHigh=yHigh,x=x,Uncen=Uncen)
censoredSegments <- function(yBottom,yLow,yHigh,x,Uncen){
  yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  numSamples<-length(x)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(x[uncensoredIndex],yLowVal[uncensoredIndex],x[uncensoredIndex],yHigh[uncensoredIndex])  
}
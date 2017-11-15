#' Generic plotting function to create censored line segments
#'
#' Basic plotting framework for EGRET dot plots. Graphical parameters default to values that work well with most plots, but all can be re-assigned.
#' See ?par for complete definitions of most optional input variables.
#'
#' @param yBottom number specifying minimum flux (required)
#' @param yLow vector specifying the x data (required), such as ConcLow
#' @param yHigh vector specifying the x data (required), such as ConcHigh
#' @param x vector x data (required)
#' @param Uncen vector that defines whether the values are censored (0) or not (1)
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @seealso \code{\link[graphics]{segments}}
#' @export
#' @examples
#' x <- c(1,2,3,4,5,6)
#' y <- c(1,3,4,3.3,4.4,7)
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
#' yLow <- c(NA,3,4,3.3,4,7)
#' yHigh <- c(1,3,4,3.3,5,NA)
#' Uncen <- c(0,1,1,1,0,0)
#' censoredSegments(yBottom=yBottom,yLow=yLow,yHigh=yHigh,x=x,Uncen=Uncen)
censoredSegments <-
  function(yBottom,
           yLow,
           yHigh,
           x,
           Uncen,
           col = "black",
           lwd = 1) {
    
    yLowVal <- ifelse(is.na(yLow), yBottom, yLow) #yLow would be NA if "simple" censored....so giving it a value here

    yTop <- par()$usr[4]
    if(par()$ylog){
      yTop <- 10^yTop
    }
    
    yHighVal <- ifelse(is.na(yHigh), yTop, yHigh) 
    
    segments(x[Uncen == 0], yLowVal[Uncen == 0], x[Uncen == 0], 
             yHighVal[Uncen == 0], col = col, lwd = lwd)
  }
#' Generic EGRET plotting function
#'
#' Basic plotting framework for EGRET dot plots. Graphical parameters default to values that work well with most plots, but all can be re-assigned.
#' See ?par for complete definitions of most optional input variables.
#'
#' @param x vector specifying the x data (required)
#' @param y vector specifying the y data (required)
#' @param xlim vector specifying the x plotting range (required)
#' @param ylim vector specifying the y plotting range (required)
#' @param xTicks vector specifying x axis tick placement
#' @param yTicks vector specifying y axis tick placement
#' @param printTitle logical defaults to TRUE, plotting parameter to control whether to have title
#' @param xaxs string defaults to "i", defines the style of x-axis interval calculation.  Possible values are i, r, e, s, d.
#' @param xlab string defaults to "", defines the x label
#' @param yaxs string defaults to "i", defines the style of y-axis interval calculation.  Possible values are i, r, e, s, d.
#' @param ylab string defaults to "", defines the y label
#' @param plotTitle string defaults to "", defines the plot title
#' @param pch number defaults to 20, specifies plot symbol
#' @param cex number defaults to 0.7, specifies plotting text magnification
#' @param cex.main number defaults to 1.3, specifies title text magnification
#' @param font.main number defaults to 2, specifies which font to use for text
#' @param cex.lab number defaults to 1.2 specifies label text magnification
#' @param tcl number defaults to 0.5, specifies length of tick marks as fraction of height of a line of text.
#' @param oma vector defaults to c(0,0,0,0) specifies outer margin
#' @param mar vector defaults to c(5,4,1,1) specifies plot area
#' @param cex.axis number defaults to 1, specifies axis text magnification
#' @param tinyPlot logical defaults to FALSE, if TRUE, changes defaults to be appropriate for multi-plot
#' @param hLine logical defaults to FALSE, inserts horizontal line at zero
#' @param oneToOneLine logical defaults to FALSE, inserts 1:1 line
#' @param ... additional graphical parameters can be adjusted
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' x <- exDaily$Date
#' y <- exDaily$Q
#' xlim <- c(min(x),max(x))
#' ylim <- c(min(y),1.05*max(y))
#' xlab <- "Date"
#' ylab <- "Flow"
#' xTicks <- pretty(xlim)
#' yTicks <- pretty(ylim)
#' genericEGRETDotPlot(x=x, y=y, 
#'                     xlim=xlim, ylim=ylim,
#'                     xlab=xlab, ylab=ylab,
#'                     xTicks=xTicks, yTicks=yTicks,
#'                     plotTitle="Test"
#' )
genericEGRETDotPlot <- function(x,y, xlim, ylim,xTicks,yTicks,
                                printTitle=TRUE,
                                xaxs="i",xlab="",yaxs="i",ylab="",plotTitle="",
                                pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2,
                                tcl=0.5,oma=c(0,0,0,0),mar=c(5,4,1,1),cex.axis=1,
                                tinyPlot=FALSE,hLine=FALSE,oneToOneLine=FALSE, ...){
  
#   if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
  par(oma=oma) 
  par(mar=mar)
  plot(x,y,xlim=xlim,xaxs=xaxs,xlab=xlab,axes=FALSE,
       ylim=ylim,yaxs=yaxs,ylab=ylab,main=plotTitle,
       pch=pch,cex=cex,cex.main=cex.main,font.main=font.main,cex.lab=cex.lab,...)

  box()
  if (hLine) abline(h = 0)
  if (oneToOneLine) abline(a=0,b=1)
  axis(1,tcl=tcl,at=xTicks,labels=xTicks)
  axis(2,tcl=tcl,las=1,at=yTicks,labels=yTicks,cex.axis=cex.axis)
  axis(3,tcl=tcl,at=xTicks,labels=FALSE)
  axis(4,tcl=tcl,at=yTicks,labels=FALSE)
#   par(mar=c(5,4,4,2)+0.1) # Not sure if this is a good idea or not
#   par(oma=c(0,0,0,0)) # Not sure if this is a good idea or not
}


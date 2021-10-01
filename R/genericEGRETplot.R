#' Generic EGRET plotting function
#'
#' Basic plotting framework for EGRET dot plots. Graphical parameters default to values that work well with most plots, but all can be re-assigned.
#' See ?par for complete definitions of most optional input variables.
#'
#' @param x vector specifying the x data (required)
#' @param y vector specifying the y data (required)
#' @param xlim vector specifying the x plotting range (required)
#' @param ylim vector specifying the y plotting range (required)
#' @param xTicks vector specifying x axis tick placement (required)
#' @param yTicks vector specifying y axis tick placement (required)
#' @param printTitle logical defaults to TRUE, plotting parameter to control whether to have title
#' @param xaxs character defaults to "i", defines the style of x-axis interval calculation.  Possible values are i, r, e, s, d.
#' @param xlab character defaults to "", defines the x label
#' @param yaxs character defaults to "i", defines the style of y-axis interval calculation.  Possible values are i, r, e, s, d.
#' @param ylab character defaults to "", defines the y label
#' @param plotTitle character defaults to "", defines the plot title
#' @param pch number defaults to 20, specifies plot symbol
#' @param cex number defaults to 0.7, specifies plotting text magnification
#' @param cex.main number defaults to 1.3, specifies title text magnification
#' @param font.main number defaults to 2, specifies which font to use for text
#' @param cex.lab number defaults to 1.2 specifies label text magnification
#' @param tcl number defaults to 0.5, specifies length of tick marks as fraction of height of a line of text.
#' @param cex.axis number defaults to 1, specifies axis text magnification
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param tinyPlot logical defaults to FALSE, if TRUE, changes defaults to be appropriate for multi-plot
#' @param las number represents style of axis labels
#' @param hLine logical defaults to FALSE, inserts horizontal line at zero
#' @param oneToOneLine logical defaults to FALSE, inserts 1:1 line
#' @param rmSciX logical defaults to FALSE, changes x label from scientific to fixed
#' @param rmSciY logical defaults to FALSE, changes y label from scientific to fixed
#' @param xDate logical defaults to FALSE, changes x label to "year-month" format if set to TRUE and total years less than 4.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param showXLabels logical defaults to TRUE. If FALSE, the x axis label is not plotted
#' @param showYLabels logical defaults to TRUE. If FALSE, the y axis label is not plotted
#' @param showXAxis logical defaults to TRUE. If FALSE, the x axis is not plotted
#' @param showYAxis logical defaults to TRUE. If FALSE, the y axis is not plotted
#' @param removeFirstX logical defaults to FALSE. If TRUE, removes the first x axis label. This can be handy for plotting mutliple plots.
#' @param removeLastX logical defaults to FALSE. If TRUE, removes the last x axis label. This can be handy for plotting mutliple plots.
#' @param removeFirstY logical defaults to FALSE. If TRUE, removes the first y axis label. This can be handy for plotting mutliple plots.
#' @param removeLastY logical defaults to FALSE. If TRUE, removes the last y axis label. This can be handy for plotting mutliple plots.
#' @param \dots additional graphical parameters can be adjusted
#' @keywords graphics water-quality statistics
#' @importFrom graphics plot
#' @export
#' @examples
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' x <- Daily$Date
#' y <- Daily$Q
#' xlim <- c(min(x),max(x))
#' ylim <- c(min(y),1.05*max(y))
#' xlab <- "Date"
#' ylab <- "Flow"
#' genericEGRETDotPlot(x=x, y=y, 
#'                     xlim=xlim, ylim=ylim,
#'                     xlab=xlab, ylab=ylab,
#'                     plotTitle="Test"
#' )
genericEGRETDotPlot <- function(x,y, xlim, ylim,
                                xTicks=pretty(xlim),yTicks=pretty(ylim),
                                printTitle=TRUE,
                                xaxs="i",xlab="",yaxs="i",ylab="",plotTitle="",
                                pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2,
                                tcl=0.5,cex.axis=1,las=1,xDate=FALSE,
                                tinyPlot=FALSE,hLine=FALSE,oneToOneLine=FALSE, 
                                rmSciX=FALSE,rmSciY=FALSE,customPar=FALSE,col="black",lwd=1,
                                showXLabels=TRUE,showYLabels=TRUE,
                                showXAxis=TRUE,showYAxis=TRUE,
                                removeFirstX=FALSE,removeLastX=FALSE,
                                removeFirstY=FALSE,removeLastY=FALSE,
                                ...){
  
  if(!customPar){
    if (tinyPlot){
      par(mar=c(4,5,1,0.1),mgp=c(2.5,0.5,0))
    } else {
      par(mar=c(5,6,4,2) + 0.1,mgp=c(3,0.5,0))
    }
  }
  
  plot(x,y,xlim=xlim,xaxs=xaxs,xlab=if(showXLabels) xlab else "",axes=FALSE,
       ylim=ylim,yaxs=yaxs,ylab=if(showYLabels) ylab else "", main=plotTitle,col=col,lwd=lwd,
       pch=pch,cex=cex,cex.main=cex.main,font.main=font.main,cex.lab=cex.lab,...)
  
  box()
  if (hLine) abline(h = 0)
  if (oneToOneLine) abline(a=0,b=1)
  
  if(rmSciX){
    xTicksLab <- prettyNum(xTicks)
  } else {
    xTicksLab <- xTicks
  }
  
  if(rmSciY){
    yTicksLab <- prettyNum(yTicks)
  } else {
    yTicksLab <- yTicks
  }
  
  if(xDate){
    yearStart <- floor(min(xlim))
    yearEnd <- ceiling(max(xlim))
    
    if(yearEnd-yearStart >= 4){
      xSpan<-c(yearStart,yearEnd)
      xTicks<-pretty(xSpan,n=5)
      xTicksLab <- xTicks
    } else {
      xlabels <- c(as.Date(paste(yearStart,"-01-01",sep="")), as.Date(paste(yearEnd,"-01-01",sep="")))
      xlabels <- pretty(xlabels,n=5)
      xTicksDates <- as.POSIXlt(xlabels)
      years <- xTicksDates$year + 1900 
      day <- xTicksDates$yday
      xTicks <- years + day/365
      xTicksLab <- format(xlabels, "%Y-%b")
    }
  }
  
  if (removeFirstX){
    xTicks <- c("", xTicks[2:(length(xTicks))])  
  }
  
  if (removeLastX){
    xTicks <- c(xTicks[1:(length(xTicks)-1)] , "")
  }
  
  if (removeFirstY){
    yTicks <- c("", yTicks[2:(length(yTicks))])  
  }
  
  if (removeLastY){
    yTicks <- c(yTicks[1:(length(yTicks)-1)] , "")
  }
  
  if(showXAxis){
    axis(1,tcl=tcl,at=xTicks,cex.axis=cex.axis,labels=xTicksLab)  
  } else {
    axis(1,tcl=tcl,at=xTicks,labels=FALSE)
  }
  axis(3,tcl=tcl,at=xTicks,labels=FALSE)
  
  if(showYAxis){
    axis(2,tcl=tcl,las=las,at=yTicks,cex.axis=cex.axis,labels=yTicksLab)
  } else {
    axis(2,tcl=tcl,at=yTicks,labels=FALSE)    
  }
  axis(4,tcl=tcl,at=yTicks,labels=FALSE)    
}

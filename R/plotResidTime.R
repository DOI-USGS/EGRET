#' Plot of the residuals from WRTDS (in log concentration units) versus time 
#'
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus time.
#' It also provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotResidTime()
plotResidTime<-function(localSample = Sample, localINFO = INFO, stdResid = FALSE, printTitle = TRUE){
  # this function shows residual versus Time
  # Time on the x-axis , 
  # residual on y-axis 
  # these residuals are from a "leave-one-out" cross validation application of WRTDS
  # if stdResid=FALSE it just works with the regular residuals
  # if stdResid=TRUE it computes the standardized residual which is the residual/Sample$SE  
  x<-localSample$DecYear
  yLow<-log(localSample$ConcLow)-localSample$yHat
  yHigh<-log(localSample$ConcHigh)-localSample$yHat
  yLow<-if(stdResid) yLow/localSample$SE else yLow
  yHigh<-if(stdResid) yHigh/localSample$SE else yHigh
  Uncen<-localSample$Uncen
  xMin<-min(x) - 0.2
  xMax<-max(x) + 0.2
  #maxYHigh<-max(yHigh) + 0.1
  #minYLow<-min(yLow,na.rm=TRUE) - 0.5
  #xSpan<-c(xMin,xMax)
  #xTicks<-pretty(xSpan,n=9)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  #ySpan<-c(minYLow,maxYHigh)
  #yTicks<-pretty(ySpan,n=5)
  #numYTicks<-length(yTicks)
  #yBottom<-yTicks[1]
  #yTop<-yTicks[numYTicks]
  xLab<-paste("")
  yLab<-if(stdResid) "Standardized Residual in natural log units" else "Residual in natural log units" 
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Residual versus Time") else ""
  
  yInfo <- generalAxis(x=yHigh, max=yHigh, min=yLow)
  xInfo <- generalAxis(x=x, max=xMax, min=xMin)
  ##########################
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom, yInfo$top),
                      xlab=xLab, ylab=yLab, plotTitle=plotTitle, hLine=TRUE
  )
  
  #   plot(x,yHigh,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab=xLab,ylim=c(yBottom,yTop),yaxs="i",ylab=yLab,main=plotTitle,pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2)
  #   axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  #   axis(2,tcl=0.5,las=1,at=yTicks,labels=yTicks)
  #   axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  #   axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  #   box()
  censoredSegments(yBottom, yLow, yHigh, x, Uncen
  )
  #   yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  #   numSamples<-length(x)
  #   uncensoredIndex <- 1:numSamples
  #   uncensoredIndex <- uncensoredIndex[Uncen==0]
  #   segments(x[uncensoredIndex],yLowVal[uncensoredIndex],x[uncensoredIndex],yHigh[uncensoredIndex])
  #   abline(h=0)
}
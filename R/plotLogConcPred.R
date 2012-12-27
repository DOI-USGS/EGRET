#' Graph of bserved versus estimated concentration as a log-log graph
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSampleEnd
#' INFO <- exINFOEnd
#' plotLogConcPred()
plotLogConcPred<-function(localSample = Sample, localINFO = INFO, concMax = NA, tinyPlot = FALSE, printTitle = TRUE){
  # this function shows observed versus estimated concentration
  # estimated log concentration on the x-axis (these are prior to bias correction), 
  # observed log concentration on y-axis 
  # these estimates are from a "leave-one-out" cross validation application of WRTDS
  if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
  x<-exp(localSample$yHat)
  yLow<-localSample$ConcLow
  yHigh<-localSample$ConcHigh
  Uncen<-localSample$Uncen
  xMin<-0.95*min(x)
  xMax<-1.05*max(x)
  maxYHigh<-if(is.na(concMax)) 1.05*max(yHigh) else concMax
  minYLow<-0.9*min(localSample$ConcLow,na.rm=TRUE)
  xTicks<-logPretty3(xMin,xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTicks<-logPretty3(minYLow,maxYHigh)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  xLab<-"Estimated Concentration in mg/L"
  yLab<-"Observed Concentration in mg/L"
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed versus Estimated Concentration") else ""
  plot(log(x,10),log(yHigh,10),axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab=xLab,ylim=c(log(yBottom,10),log(yTop,10)),yaxs="i",ylab=yLab,main=plotTitle,pch=20,cex=0.7,cex.main=1.0,font.main=2,cex.lab=1.2)
  axis(1,tcl=0.5,at=log(xTicks,10),labels=xTicks)
  axis(2,tcl=0.5,las=1,at=log(yTicks,10),labels=yTicks)
  axis(3,tcl=0.5,at=log(xTicks,10),labels=FALSE)
  axis(4,tcl=0.5,at=log(yTicks,10),labels=FALSE)
  box()
  yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  numSamples<-length(x)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(log(x[uncensoredIndex],10),log(yLowVal[uncensoredIndex],10),log(x[uncensoredIndex],10),log(yHigh[uncensoredIndex],10))
  abline(a=0,b=1)
  par(mar=c(5,4,4,2)+0.1)
}
#' Graph of observed versus estimated flux
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{fluxConst}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotFluxPred(fluxUnit = 7)
#' plotFluxPred(fluxUnit = 'poundsDay')
plotFluxPred<-function(localSample = Sample, localINFO = INFO, fluxUnit = 3, fluxMax = NA, tinyPlot = FALSE, printTitle = TRUE){
  # this function shows observed versus estimated flux
  # estimated flux on the x-axis (these include the bias correction), 
  # observed flux on y-axis 
  # these estimates are from a jack-knife, "leave-one-out", cross validation application of WRTDS
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################
  
  if(tinyPlot) par(mar=c(5,5,1,1)) else par(mar=c(5,5,4,2)+0.1)
  fluxFactor <- fluxUnit@unitFactor*86.40
  x<-localSample$ConcHat*localSample$Q*fluxFactor
  yLow<-localSample$ConcLow*localSample$Q*fluxFactor
  yHigh<-localSample$ConcHigh*localSample$Q*fluxFactor
  Uncen<-localSample$Uncen
  xMax<-1.05*max(x)
  maxYHigh<-if(is.na(fluxMax)) 1.05*max(yHigh) else fluxMax
  xTicks<-yPretty(xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTicks<-yPretty(maxYHigh)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  xLab <- fluxUnit@unitEstimate
  yLab <- fluxUnit@unitExpress
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed vs Estimated Flux") else ""
  plot(x,yHigh,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab=xLab,ylim=c(yBottom,yTop),yaxs="i",ylab=yLab,main=plotTitle,pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2)
  axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  axis(2,tcl=0.5,las=1,at=yTicks,labels=yTicks)
  axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  box()
  yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  numSamples<-length(x)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(x[uncensoredIndex],yLowVal[uncensoredIndex],x[uncensoredIndex],yHigh[uncensoredIndex])
  abline(a=0,b=1)
  par(mar=c(5,4,4,2)+0.1)
}
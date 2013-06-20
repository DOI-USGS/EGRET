#' Graph of observed versus estimated flux
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{fluxConst}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param oneToOneLine inserts 1:1 line
#' @param tinyPlot logical variable if TRUE plot is designed to be small, if FALSE it is designed for page size, default is FALSE (not fully implemented yet)
#' @param logScale logical if TRUE x and y plotted in log axis
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' plotFluxPred()
#' plotFluxPred(fluxUnit = 'poundsDay')
plotFluxPred<-function(localSample = Sample, localINFO = INFO, fluxUnit = 3, fluxMax = NA, 
                       printTitle = TRUE, oneToOneLine=TRUE, customPar=FALSE,col="black", lwd=1,
                       cex=0.8, cex.axis=1.1,cex.main=1.1,tinyPlot=FALSE,logScale=FALSE,...){
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

  fluxFactor <- fluxUnit@unitFactor*86.40
  x<-localSample$ConcHat*localSample$Q*fluxFactor
  yLow<-localSample$ConcLow*localSample$Q*fluxFactor
  yHigh<-localSample$ConcHigh*localSample$Q*fluxFactor
  Uncen<-localSample$Uncen

  if (tinyPlot) {
    xLab <- fluxUnit@unitEstimateTiny
    yLab <- fluxUnit@unitExpressTiny
  } else {
    xLab <- fluxUnit@unitEstimate
    yLab <- fluxUnit@unitExpress
  }
  
  if(logScale){
    logText <- "xy"
    minX <- NA
    minY <- NA
  } else {
    logText <- ""
    minX <- 0
    minY <- 0
  }
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed vs Estimated Flux") else ""
  
  ###############################

  
  xInfo <- generalAxis(x=x, minVal=minX, maxVal=NA, logScale=logScale, tinyPlot=tinyPlot,padPercent=5)  
  yInfo <- generalAxis(x=yHigh, minVal=minY, maxVal=fluxMax, logScale=logScale, tinyPlot=tinyPlot,padPercent=5)
  
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,log=logText, customPar=customPar,
                      plotTitle=plotTitle,oneToOneLine=oneToOneLine, cex=cex,col=col,
                      tinyPlot=tinyPlot,cex.axis=cex.axis,cex.main=cex.main,...
  )
  
  censoredSegments(yBottom=yInfo$bottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen,col=col,lwd=lwd)

}
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
#' @param oneToOneLine inserts 1:1 line
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotFluxPred(fluxUnit = 7)
#' plotFluxPred(fluxUnit = 'poundsDay')
plotFluxPred<-function(localSample = Sample, localINFO = INFO, fluxUnit = 3, fluxMax = NA, 
                       tinyPlot = FALSE, printTitle = TRUE, oneToOneLine=FALSE, ...){
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
  
  #   if(tinyPlot) par(mar=c(5,5,1,1)) else par(mar=c(5,5,4,2)+0.1)
  fluxFactor <- fluxUnit@unitFactor*86.40
  x<-localSample$ConcHat*localSample$Q*fluxFactor
  yLow<-localSample$ConcLow*localSample$Q*fluxFactor
  yHigh<-localSample$ConcHigh*localSample$Q*fluxFactor
  Uncen<-localSample$Uncen
  #xMax<-1.05*max(x)
  #maxYHigh<-if(is.na(fluxMax)) 1.05*max(yHigh) else fluxMax
  #xTicks<-yPretty(xMax)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  #yTicks<-yPretty(maxYHigh)
  #numYTicks<-length(yTicks)
  #yBottom<-yTicks[1]
  #yTop<-yTicks[numYTicks]
  if (tinyPlot) {
    xLab <- fluxUnit@unitEstimateTiny
    yLab <- fluxUnit@unitExpressTiny
    par(mar=c(5,4,1,1.5))
  }
  else {
    xLab <- fluxUnit@unitEstimate
    yLab <- fluxUnit@unitExpress
    par(mar=c(5,4,4,2)+0.1)
  }
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed vs Estimated Flux") else ""
  
  ###############################
  #if(tinyPlot) par(mar=c(5,4,1,1.5)) else par(mar=c(5,4,4,2)+0.1)
  
  xInfo <- generalAxis(x=x, minVal=0, maxVal=NA, tinyPlot=tinyPlot)
  
  yInfo <- generalAxis(x=yHigh, minVal=0, maxVal=fluxMax, tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,
                      plotTitle=plotTitle,oneToOneLine=oneToOneLine, ...
  )
  
  censoredSegments(yBottom=yInfo$bottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen)
  
  par(mar=c(5,4,4,2)+0.1)
}
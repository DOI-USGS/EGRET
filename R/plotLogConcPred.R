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
#' @param ... arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogConcPred()
plotLogConcPred<-function(localSample = Sample, localINFO = INFO, concMax = NA, 
                          tinyPlot = FALSE, printTitle = TRUE, ...){
  # this function shows observed versus estimated concentration
  # estimated log concentration on the x-axis (these are prior to bias correction), 
  # observed log concentration on y-axis 
  # these estimates are from a "leave-one-out" cross validation application of WRTDS
#   if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
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
  
  #################################
  par(mar = c(5,6,5,2))
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xTicks, yTicks=yTicks,
                      xlim=c(xLeft,xRight), ylim=c(yBottom,yTop),
                      xlab=xLab,ylab=yLab,
                      plotTitle=plotTitle, cex.main=1.0,
                      log="xy", oneToOneLine=TRUE, ...
    )

  censoredSegments(yBottom=yBottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen)

  par(mar=c(5,4,4,2)+0.1)
}
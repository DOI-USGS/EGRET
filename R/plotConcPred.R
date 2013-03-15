#' Plot of Observed Concentration versus Estimated Concentration 
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param ... arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotConcPred()
plotConcPred<-function(localSample = Sample, localINFO = INFO, concMax = NA, 
                       printTitle = TRUE,...){
  # this function shows observed versus predicted concentration
  # predicted concentration on the x-axis (these include the bias correction), 
  # observed concentration on y-axis 
  # these predictions are from a "leave-one-out" cross validation application of WRTDS 
  x<-localSample$ConcHat
  yLow<-localSample$ConcLow
  yHigh<-localSample$ConcHigh
  Uncen<-localSample$Uncen
  xMax<-1.05*max(x)
  maxYHigh<-if(is.na(concMax)) 1.05*max(yHigh) else concMax
  xTicks<-yPretty(xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTicks<-yPretty(maxYHigh)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  xLab<-"Estimated Concentration in mg/L"
  yLab<-"Observed Concentration in mg/L"
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed versus Estimated Concentration") else ""
  
  ############################
  par(mar = c(5,6,5,2))
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xTicks, yTicks=yTicks,
                      xlim=c(xLeft,xRight), ylim=c(yBottom,yTop),
                      xlab=xLab, ylab=yLab,
                      plotTitle=plotTitle, oneToOneLine=TRUE,...
    )

  censoredSegments(yBottom=yBottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen)

}
#' Plot of the residuals from WRTDS versus the estimated values (all in log concentration units)
#'
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus the estimated values, also in natural log concentration units.  These estimates are
#' the log-space estimates prior to bias-correction.  
#' The function provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error. 
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex number
#' @param cex.axis number
#' @param cex.main number
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords water-quality statistics graphics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotResidPred()
plotResidPred<-function(localSample = Sample, localINFO = INFO, stdResid = FALSE, 
                        tinyPlot = FALSE, printTitle = TRUE, 
                        cex=0.8, cex.axis=1.1,cex.main=1.1,...){
  # this function shows residual versus estimated in log space
  # estimated log concentration on the x-axis (these are prior to bias correction), 
  # observed log concentration on y-axis 
  # these estimates are from a "leave-one-out" cross validation application of WRTDS
  # if stdResid=FALSE it just works with the regular residuals
  # if stdResid=TRUE it computes the standardized residual which is the residual/Sample$SE  
  #if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
  
  x<-exp(localSample$yHat)
  yLow<-log(localSample$ConcLow)-localSample$yHat
  yHigh<-log(localSample$ConcHigh)-localSample$yHat
  yLow<-if(stdResid) yLow/localSample$SE else yLow
  yHigh<-if(stdResid) yHigh/localSample$SE else yHigh
  Uncen<-localSample$Uncen
  #xMin<-0.95*min(x)
  #xMax<-1.05*max(x)
  #maxYHigh<-max(yHigh) + 0.1
  #minYLow<-min(yLow,na.rm=TRUE) - 0.5
  #xTicks<-logPretty3(xMin,xMax)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  #ySpan<-c(minYLow,maxYHigh)
  #yTicks<-pretty(ySpan,n=5)
  #numYTicks<-length(yTicks)
  #yBottom<-yTicks[1]
  #yTop<-yTicks[numYTicks]
  xLab<-"Estimated Concentration in mg/L"
  
  if (tinyPlot){
    xLab <- "Est. Conc. (mg/L)"
    yLab <- if(stdResid) expression(paste("log"["e"],"(Std. Residual) units")) else expression(paste("log"["e"],"(Residual) units"))
  }
  else {
    xLab<-"Estimated Concentration in mg/L"
    yLab<-if(stdResid) "Standardized Residual in natural log units" else "Residual in natural log units"
  }
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Residual versus Estimated Concentration") else ""
  
  ####################
  
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, logScale=TRUE, tinyPlot=tinyPlot, padPercent=5)
  
  yInfo <- generalAxis(x=yHigh, minVal=(min(yLow,na.rm=TRUE)-0.5), maxVal=(max(yHigh) + 0.1), tinyPlot=tinyPlot)

  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab, plotTitle=plotTitle,
                      log="x",hLine=TRUE,cex.axis=cex.axis,cex.main=cex.main, tinyPlot=tinyPlot,...
    )

  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen
    )

}

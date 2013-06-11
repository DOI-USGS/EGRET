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
#' @param hLine inserts horizontal line at zero
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotResidTime()
plotResidTime<-function(localSample = Sample, localINFO = INFO, stdResid = FALSE, 
                        printTitle = TRUE, hLine=TRUE, tinyPlot=FALSE,
                        cex=0.8, cex.axis=1.1,cex.main=1.1,...){
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

  xLab<-""
  if(tinyPlot){
    yLab<-if(stdResid) "Standardized Residual" else "Residual"     
  } else {
    yLab<-if(stdResid) "Standardized Residual in natural log units" else "Residual in natural log units"     
  }
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Residual versus Time") else ""
  
  yInfo <- generalAxis(x=yHigh, maxVal=max(yHigh) + 0.1, minVal=min(yLow,na.rm=TRUE) - 0.5,padPercent=0, tinyPlot=tinyPlot)
  xInfo <- generalAxis(x=x, maxVal=xMax, minVal=xMin,padPercent=0, tinyPlot=tinyPlot)
  
  ##########################
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom, yInfo$top),
                      xlab=xLab, ylab=yLab, plotTitle=plotTitle, 
                      cex.axis=cex.axis,cex.main=cex.main, hLine=hLine, tinyPlot=tinyPlot,...
  )

  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen
  )

}
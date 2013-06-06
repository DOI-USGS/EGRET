#' Plot of Observed Concentration versus Discharge 
#'
#' Data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO. 
#' Discharge is plotted on a log scale.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param logScale logical if TRUE x and y plotted in log axis
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param concMin numeric if you want to specify the minimum concentration value to display, you can do so with the argument concMin, otherwise it will be automatic
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param cex number
#' @param cex.axis number
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotConcQ()
plotConcQ<-function(localSample = Sample, localINFO = INFO, qUnit = 2, tinyPlot = FALSE, logScale=FALSE,
                    concMax = NA, concMin =NA, printTitle = TRUE, cex=0.8, cex.axis=1.1,cex.main=1.1,...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale, concentration on y-axis
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  qFactor<-qUnit@qUnitFactor
  x<-localSample$Q*qFactor
  
  yLow<-localSample$ConcLow
  yHigh<-localSample$ConcHigh
  Uncen<-localSample$Uncen

  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Concentration versus Discharge") else ""
  
  if (tinyPlot){
    xLab<-qUnit@qUnitTiny
    yLab <- "Conc. (mg/L)"
  } else {
    xLab<-qUnit@qUnitExpress
    yLab <- "Concentration in mg/L"
  }
  
  if(logScale){
    logScaleText <- "xy"
    yMin <- concMin
  } else {
    logScaleText <- "x"
    yMin <- 0
  }
  
  yInfo <- generalAxis(x=yHigh, maxVal=concMax, minVal=yMin, tinyPlot=tinyPlot,logScale=logScale)
  xInfo <- generalAxis(x=x, maxVal=NA, minVal=NA, logScale=TRUE, tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh, 
                      xlim=c(xInfo$bottom, xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      plotTitle=plotTitle, log=logScaleText,cex.axis=cex.axis,
                      cex.main=cex.main, tinyPlot=tinyPlot,...
  )
  
  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen  )

}

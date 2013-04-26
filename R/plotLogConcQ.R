#' Sample data plot: concentration vs. discharge (log/log)
#'
#' Concentration data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param concMax numeric if you want to specify the maximum concentration value to display, you can do so with the argument concMax, otherwise it will be automatic
#' @param concMin numeric if you want to specify the minimum concentration value to display, you can do so with the argument concMin, otherwise it will be automatic
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogConcQ(qUnit = 1)
#' plotLogConcQ(qUnit = 'thousandCfs')
plotLogConcQ<-function(localSample = Sample, localINFO = INFO, qUnit = 2, 
            tinyPlot = FALSE, concMax = NA, concMin = NA, printTitle = TRUE, ...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale, 
  # concentration on y-axis on a log scale
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  
  
  if(tinyPlot) par(mar=c(5,4,1,1.5)) else par(mar=c(5,4,4,2)+0.1)
  qFactor <- qUnit@qUnitFactor
  x<-localSample$Q*qFactor
  Uncen<-localSample$Uncen
#   xMin<-0.95*min(x)
#   xMax<-1.05*max(x)
#   xTicks<-if(tinyPlot) logPretty1(xMin,xMax) else logPretty3(xMin,xMax)
#   numXTicks<-length(xTicks)
#   xLeft<-xTicks[1]
#   xRight<-xTicks[numXTicks]
#   #   xLab<-qUnitExpress[qUnit]
#   xLab <- qUnit@qUnitExpress

  xInfo <- generalAxis(x, maxVal=NA, minVal=NA, logScale=TRUE, tinyPlot=tinyPlot, padPercent=5 )
  
#   yLow<-localSample$ConcLow
#   yHigh<-localSample$ConcHigh
#   maxYHigh<-if(is.na(concMax)) 1.05*max(yHigh) else concMax
#   minYLow<-if(is.na(concMin)) 0.9*min(localSample$ConcAve) else concMin
#   yTicks<-logPretty3(minYLow,maxYHigh)
#   yBottom<-yTicks[1]
#   yTop<-yTicks[length(yTicks)]
  
#   yInfo <- with(localSample, concentrationAxis(ConcLow, ConcHigh, ConcAve, concMax, concMin))
  
  yInfo <- generalAxis(localSample$ConcAve, max=concMax, min=concMin, tinyPlot=tinyPlot, padPercent=5, log=TRUE)
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Concentration versus Discharge") else ""
  
  if (tinyPlot) {
    yLabel <- "Conc. (mg/L)"
  }
  else {
    yLabel <- "Concentration in mg/L"
  }
  
  #####################
  genericEGRETDotPlot(x=x, y=localSample$ConcHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top),ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xInfo$xLab,ylab=yLabel, plotTitle=plotTitle,
                      log="xy",...
    )

  censoredSegments(yInfo$bottom, localSample$ConcLow, localSample$ConcHigh, x, Uncen )

  par(mar=c(5,4,4,2)+0.1)
}
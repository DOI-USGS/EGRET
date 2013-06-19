#' Plot of the time series of daily flux estimates and the sample values for the days that were sampled
#'
#' This plot is useful for visual examination of the ability of the WRTDS, or other model, to fit the 
#' data, as seen in a time-series perspective. 
#'
#' @param startYear numeric specifying the starting date (expressed as decimal years, for example 1989.0) for the plot
#' @param endYear numeric specifiying the ending date for the plot 
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param fluxUnit number representing in pre-defined fluxUnit class array. \code{\link{fluxConst}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par should be set by user, if FALSE, EGRET chooses best graphical parameters.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' plotFluxTimeDaily(2001,2009)
plotFluxTimeDaily<-function (startYear, endYear, localSample = Sample, localDaily = Daily, 
                             localINFO = INFO, tinyPlot = FALSE, fluxUnit = 3, fluxMax = NA, 
                             printTitle = TRUE, cex=0.8, cex.axis=1.1,cex.main=1.1, 
                             customPar=FALSE,col="black",lwd=1,...) {
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################    
  
  fluxFactor <- fluxUnit@unitFactor*86.40
  subSample <- subset(localSample, DecYear >= startYear)
  subSample <- subset(subSample, DecYear <= endYear)
  subDaily <- subset(localDaily, DecYear >= startYear)
  subDaily <- subset(subDaily, DecYear <= endYear)
  xSample <- subSample$DecYear
  xDaily <- subDaily$DecYear

  yLow <- subSample$ConcLow*subSample$Q*fluxFactor
  yHigh <- subSample$ConcHigh*subSample$Q*fluxFactor
  Uncen <- subSample$Uncen
  #yAll <- c(subDaily$ConcDay*subDaily$Q*fluxFactor, subSample$ConcHigh*subSample$Q*fluxFactor)
  #maxYHigh <- if (is.na(fluxMax)) 
  #  1.05 * max(yAll)
  #else fluxMax
  #yTicks <- yPretty(maxYHigh)
  #yTop <- yTicks[length(yTicks)]
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, "\n", localINFO$paramShortName, 
          "\n", "Observed and Estimated Flux versus Time")
  else ""
  
  ###################################
  
  yBottom <- 0 #Not specified within script, added under assumption that it's always zero based on ylim definition in this function
  
  xInfo <- generalAxis(x=xSample, minVal=startYear, maxVal=endYear, tinyPlot=tinyPlot,padPercent=0)
  yInfo <- generalAxis(x=yHigh, minVal=yBottom, maxVal=fluxMax, tinyPlot=tinyPlot,padPercent=5)
  
  if (tinyPlot) {
    yLab <- fluxUnit@unitExpressTiny
  }
  else {
    yLab <- fluxUnit@unitExpress
  }
  
  genericEGRETDotPlot(x=xSample, y=yHigh,
                      xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, yInfo$top),
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      ylab = yLab, customPar=customPar,cex=cex,
                      plotTitle=plotTitle, tinyPlot=tinyPlot,cex.axis=cex.axis,
                      cex.main=cex.main,col=col,lwd=lwd,...
    )

  lines(xDaily, subDaily$ConcDay*subDaily$Q*fluxFactor,col=col,lwd=lwd)
  censoredSegments(yBottom=yInfo$bottom,yLow=yLow,yHigh=yHigh,x=xSample,Uncen=Uncen,col=col,lwd=lwd)

}
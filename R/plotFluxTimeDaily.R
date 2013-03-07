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
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' Daily <- exDaily
#' INFO <- exINFO
#' plotFluxTimeDaily(2001,2009)
plotFluxTimeDaily<-function (startYear, endYear, localSample = Sample, localDaily = Daily, 
                             localINFO = INFO, tinyPlot = FALSE, fluxUnit = 3, fluxMax = NA, printTitle = TRUE) {
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################    
  
  if (tinyPlot) 
    par(mar = c(5, 4, 1, 1))
  else par(mar = c(5, 4, 4, 2) + 0.1)
  fluxFactor <- fluxUnit@unitFactor*86.40
  subSample <- subset(localSample, DecYear >= startYear)
  subSample <- subset(subSample, DecYear <= endYear)
  subDaily <- subset(localDaily, DecYear >= startYear)
  subDaily <- subset(subDaily, DecYear <= endYear)
  xSample <- subSample$DecYear
  xDaily <- subDaily$DecYear
  xLimits <- c(startYear, endYear)
  xTicks <- pretty(xLimits, n = 5)
  numXTicks <- length(xTicks)
  xLeft <- xTicks[1]
  xRight <- xTicks[numXTicks]
  yLow <- subSample$ConcLow*subSample$Q*fluxFactor
  yHigh <- subSample$ConcHigh*subSample$Q*fluxFactor
  Uncen <- subSample$Uncen
  yAll <- c(subDaily$ConcDay*subDaily$Q*fluxFactor, subSample$ConcHigh*subSample$Q*fluxFactor)
  maxYHigh <- if (is.na(fluxMax)) 
    1.05 * max(yAll)
  else fluxMax
  yTicks <- yPretty(maxYHigh)
  yTop <- yTicks[length(yTicks)]
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, "\n", localINFO$paramShortName, 
          "\n", "Observed and Estimated Flux versus Time")
  else ""
  
  ###################################
  
  yBottom <- 0 #Not specified within script, added under assumption that it's always zero based on ylim definition in this function
  
  genericEGRETDotPlot(x=xSample, y=yHigh,
                      xlim = c(xLeft, xRight), ylim = c(0, yTop),
                      xTicks=xTicks, yTicks=yTicks,
                      ylab = fluxUnit@unitExpress,
                      plotTitle=plotTitle, ...
    )
  
#   plot(xSample, yHigh, axes = FALSE, xlim = c(xLeft, xRight), 
#        xaxs = "i", xlab = "", ylim = c(0, yTop), yaxs = "i", 
#        ylab = fluxUnit@unitExpress, main = plotTitle, pch = 20, 
#        cex = 0.7, cex.main = 1.3, font.main = 2, cex.lab = 1.2)
#   axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
#   axis(2, tcl = 0.5, las = 1, at = yTicks)
#   axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
#   axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
  
  par(new = TRUE)
  genericEGRETDotPlot(x=xDaily, y=subDaily$ConcDay*subDaily$Q*fluxFactor,
                      xTicks=xTicks, yTicks=yTicks,
                      xlim = c(xLeft,xRight), ylim = c(0, yTop),
                      type="l", ...
    )
  
#   plot(xDaily, subDaily$ConcDay*subDaily$Q*fluxFactor, axes = FALSE, xlim = c(xLeft, 
#                                                                               xRight), xaxs = "i", xlab = "", ylim = c(0, yTop), yaxs = "i", 
#        ylab = "", main = "", type = "l", cex.main = 1.3, font.main = 2, 
#        cex.lab = 1.2)
#   box()
  censoredSegments(yBottom=yBottom,yLow=yLow,yHigh=yHigh,x=xSample,Uncen=Uncen
    )
  
#   yLowVal <- ifelse(is.na(yLow), 0, yLow)
#   numSamples <- length(xSample)
#   uncensoredIndex <- 1:numSamples
#   uncensoredIndex <- uncensoredIndex[Uncen == 0]
#   segments(xSample[uncensoredIndex], yLowVal[uncensoredIndex], 
#            xSample[uncensoredIndex], yHigh[uncensoredIndex])
  par(mar = c(5, 4, 4, 2) + 0.1)
}
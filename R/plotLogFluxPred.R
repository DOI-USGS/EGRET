#' Sample data plot:  observed log flux vs predicted log flux
#'
#' Concentration data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit object of qUnit class. \code{\link{qConst}}
#' @param fluxMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param oneToOneLine inserts 1:1 line
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogFluxPred(fluxUnit = 1)
#' plotLogFluxPred(fluxUnit = 'kgDay')
plotLogFluxPred<-function (localSample = Sample, localINFO = INFO, fluxUnit = 3, 
                           fluxMax = NA, tinyPlot=TRUE, printTitle = TRUE, oneToOneLine=TRUE, ...) 
{
  if (is.numeric(fluxUnit)) {
    fluxUnit <- fluxConst[shortCode = fluxUnit][[1]]
  }
  else if (is.character(fluxUnit)) {
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  if (tinyPlot) 
    par(mar = c(5, 5, 1, 1))
  else par(mar = c(5, 5, 4, 2) + 0.1)
  fluxFactor <- fluxUnit@unitFactor * 86.4
  #   x <- exp(localSample$yHat) * localSample$Q * fluxFactor
  x <- localSample$ConcHat * localSample$Q * fluxFactor
  yLow <- localSample$ConcLow * localSample$Q * fluxFactor
  yHigh <- localSample$ConcHigh * localSample$Q * fluxFactor
  Uncen <- localSample$Uncen
  #xMin <- 0.95 * min(x)
  #xMax <- 1.05 * max(x)
  #maxYHigh <- if (is.na(fluxMax)) 
  #  1.05 * max(yHigh)
  #else fluxMax
  #minYLow <- 0.5 * min(localSample$ConcLow * localSample$Q * 
  #                      fluxFactor, na.rm = TRUE)
  #xTicks <- logPretty3(xMin, xMax)
  #numXTicks <- length(xTicks)
  #xLeft <- xTicks[1]
  #xRight <- xTicks[numXTicks]
  #yTicks <- logPretty3(minYLow, maxYHigh)
  #numYTicks <- length(yTicks)
  #yBottom <- yTicks[1]
  #yTop <- yTicks[numYTicks]
  xLab <- fluxUnit@unitEstimate
  yLab <- fluxUnit@unitExpress
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, "\n", localINFO$paramShortName, 
          "\n", "Observed vs Predicted Flux")
  else ""
  
  ##################################
  
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, log=TRUE, tinyPlot=tinyPlot)
  
  yInfo <- generalAxis(x=yHigh, minVal=NA, maxVal=fluxMax, log=TRUE, tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=xInfo$ticks,
                      xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, yInfo$top),
                      xlab = xLab, ylab = yLab, plotTitle=plotTitle,
                      log="xy", oneToOneLine=oneToOneLine, ...
  )
  # Laura took out cex = 0.4,
  
  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen )
  
  par(mar = c(5, 4, 4, 2) + 0.1)
  
}
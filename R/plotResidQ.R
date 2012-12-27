#' Plot of the residuals from WRTDS (in log concentration units) versus the discharge 
#'
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus the discharge shown on a log scale. 
#' The function also provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSampleEnd
#' INFO <- exINFOEnd
#' plotResidQ(qUnit=1)
plotResidQ<-function (localSample = Sample, localINFO = INFO, qUnit = 2, tinyPlot = FALSE, 
                      stdResid = FALSE, printTitle = TRUE) 
{  if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
   if (is.numeric(qUnit)) {
     qUnit <- qConst[shortCode = qUnit][[1]]
   }
   else if (is.character(qUnit)) {
     qUnit <- qConst[qUnit][[1]]
   }
   qFactor <- qUnit@qUnitFactor
   x <- localSample$Q * qFactor
   yLow <- log(localSample$ConcLow) - localSample$yHat
   yHigh <- log(localSample$ConcHigh) - localSample$yHat
   yLow <- if (stdResid) 
     yLow/localSample$SE
   else yLow
   yHigh <- if (stdResid) 
     yHigh/localSample$SE
   else yHigh
   Uncen <- localSample$Uncen
   xMin <- 0.95 * min(x)
   xMax <- 1.05 * max(x)
   maxYHigh <- max(yHigh) + 0.1
   minYLow <- min(yLow, na.rm = TRUE) - 0.5
   xTicks <- logPretty3(xMin, xMax)
   numXTicks <- length(xTicks)
   xLeft <- xTicks[1]
   xRight <- xTicks[numXTicks]
   ySpan <- c(minYLow, maxYHigh)
   yTicks <- pretty(ySpan, n = 5)
   numYTicks <- length(yTicks)
   yBottom <- yTicks[1]
   yTop <- yTicks[numYTicks]
   xLab <- qUnit@qUnitExpress
   yLab <- if (stdResid) 
     "Standardized Residual in natural log units"
   else "Residual in natural log units"
   plotTitle <- if (printTitle) 
     paste(localINFO$shortName, "\n", localINFO$paramShortName, 
           "\n", "Residual versus Discharge")
   else ""
   plot(log(x, 10), yHigh, axes = FALSE, xlim = c(log(xLeft, 
                                                      10), log(xRight, 10)), xaxs = "i", xlab = xLab, ylim = c(yBottom, 
                                                                                                               yTop), yaxs = "i", ylab = yLab, main = plotTitle, pch = 20, 
        cex = 0.4, cex.main = 1.3, font.main = 2, cex.lab = 1.0)
   axis(1, tcl = 0.5, at = log(xTicks, 10), labels = xTicks)
   axis(2, tcl = 0.5, las = 1, at = yTicks, labels = yTicks)
   axis(3, tcl = 0.5, at = log(xTicks, 10), labels = FALSE)
   axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
   box()
   yLowVal <- ifelse(is.na(yLow), yBottom, yLow)
   numSamples <- length(x)
   uncensoredIndex <- 1:numSamples
   uncensoredIndex <- uncensoredIndex[Uncen == 0]
   segments(log(x[uncensoredIndex], 10), yLowVal[uncensoredIndex], 
            log(x[uncensoredIndex], 10), yHigh[uncensoredIndex])
   abline(h = 0)
   par(mar=c(5,4,4,2)+0.1)
}
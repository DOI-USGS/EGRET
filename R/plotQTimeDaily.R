#' Plot of the discharge time series
#'
#' Part of flowHistory component.
#' Allows discharge record to only show those discharges above a given threshold
#'
#' @param startYear numeric indicating the starting year for the graph
#' @param endYear numeric indicating the ending year for the graph (should be a time in decimal years that is after the last observations to be plotted)
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qLower numeric specifying the lower bound on discharges that are to be plotted, must be in the units specified by qUnit, default is NA (lower bound is zero)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.  Default is qUnit=1 (cubic feet per second)
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @keywords graphics streamflow
#' @export
#' @examples
#' Daily <- exDailyStart
#' INFO <- exINFOStart
#' plotQTimeDaily(1990,2000,qLower=10)
plotQTimeDaily<-function (startYear, endYear, localDaily = Daily, 
                          localINFO = INFO, qLower = NA, qUnit = 1, tinyPlot = FALSE, printTitle = TRUE)    
{
  #########################################################
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  }
  else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  #############################################################
  qFactor<-qUnit@qUnitFactor
  if (tinyPlot) 
    par(mar = c(5, 4, 1, 1))
  else par(mar = c(5, 4, 4, 2) + 0.1)
  subDaily <- subset(localDaily, DecYear >= startYear)
  subDaily <- subset(subDaily, DecYear <= endYear)
  xDaily <- subDaily$DecYear
  xLimits <- c(startYear, endYear)
  xTicks <- pretty(xLimits, n = 9)
  numXTicks <- length(xTicks)
  xLeft <- xTicks[1]
  xRight <- xTicks[numXTicks]
  yDaily <- qFactor * subDaily$Q
  yMin <- if(is.na(qLower)) 0 else qLower
  yMax <- 1.05*max(yDaily)
  ySpan <- c(yMin,yMax)
  yTicks <- pretty(ySpan,8)
  nYTicks <- length(yTicks)
  yTop <- yTicks[nYTicks]
  yBottom <- yTicks[1]
  line2 <- if(is.na(qLower)) "Daily Discharge" else paste("Daily discharge above a threshold of\n",qLower," ",qUnit@qUnitName,sep="")
  line1 <- localINFO$shortName
  plotTitle <- if (printTitle) 
    paste(line1, "\n", line2)
  else ""
  yLab <- qUnit@qUnitName
  qBottom <- if(is.na(qLower)) 0 else qLower
  plot(xDaily, yDaily, axes = FALSE, xlim = c(startYear, endYear), 
       xaxs = "i", xlab = "", ylim = c(qBottom, yTop), yaxs = "i", 
       ylab = yLab, main = plotTitle, type = "l", lwd = 3, col="red", 
       cex = 0.7, cex.main = 1.3, font.main = 2, cex.lab = 1.2)
  axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
  axis(2, tcl = 0.5, las = 1, at = yTicks)
  axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
  axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
  box()
  par(mar = c(5, 4, 4, 2) + 0.1)
}
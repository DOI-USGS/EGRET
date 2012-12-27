#' Plots the difference between two years from a contour plot created by plotContours
#'
#' These plots are normally used for plotting changes in the estimated concentration surface (whatSurface=3) but can be used to explore the 
#' changes in estimated surfaces for the log of concentration or for the standard error (in log space) which is what determines the bias correction.
#'
#' @param year0 numeric value for the calendar year that is the first year of the pair of years for the analysis, should be a whole number
#' @param year1 numeric value for the calendar year that is the second year of the pair of years for the analysis, should be a whole number
#' @param qBottom numeric value for the bottom edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qTop numeric value for the top edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param maxDiff numeric value which is the absolute value of the largest change in concentration that will be shown on the figure
#' @param whatSurface numeric value, can only accept 1, 2, or 3;  whatSurface=1 is yHat (log concentration), whatSurface=2 is SE (standard error of log concentration), and whatSurface=3 is ConcHat (unbiased estimate of concentration), default = 3
#' @param localsurfaces string specifying the name of the matrix that contains the estimated surfaces, default is surfaces
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param localDaily string specifying the name of the data frame that contains the daily data, default name is Daily
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param span numeric, it is the half-width (in days) of the smoothing window for computing the flow duration information, default = 60
#' @param pval numeric, the probability value for the lower flow frequency line on the graph
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param vert1 numeric, the location in time for a black vertical line on the figure, yearStart<vert1<yearEnd, default is NA (vertical line is not drawn) 
#' @param vert2 numeric, the location in time for a black vertical line on the figure, yearStart<vert2<yearEnd, default is NA (vertical line is not drawn)
#' @param horiz numeric, the location in discharge for a black horizontal line on the figure, qBottom<vert1<qTop, default is NA (no horizontal line is drawn)
#' @param flowDuration logical variable if TRUE plot the flow duration lines, if FALSE do not plot them, default = TRUE
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' year0<-2001
#' year1<-2009
#' qBottom<-0.2
#' qTop<-20
#' maxDiff<-0.5
#' surfaces <- exsurfaces
#' INFO <- exINFOEnd
#' Daily <- exDailyEnd
#' plotDiffContours(year0,year1,qBottom,qTop,maxDiff)
plotDiffContours<-function (year0, year1, qBottom, qTop, maxDiff, whatSurface = 3, 
                            localsurfaces = surfaces, localINFO = INFO, localDaily = Daily, 
                            qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, 
                            vert1 = NA, vert2 = NA, horiz = NA, flowDuration = TRUE) 
{
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  }
  else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  par(oma = c(6, 1, 6, 0))
  par(mar = c(5, 5, 4, 2) + 0.1)
  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
                   "Concentration")
  j <- 3
  j <- if (whatSurface == 1) 
    1
  else j
  j <- if (whatSurface == 2) 
    2
  else j
  surf <- localsurfaces
  
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  bottomYear <- localINFO$bottomYear
  stepYear <- localINFO$stepYear
  nVectorYear <- localINFO$nVectorYear
  start0<-((year0-bottomYear)*16)+1
  end0<-start0+16
  start1<-((year1-bottomYear)*16)+1
  end1<-start1+16
  diff<-surf[,start1:end1,j] - surf[,start0:end0,j]
  difft<-t(diff)
  surfaceSpan <- c(-maxDiff,maxDiff)
  contourLevels <- pretty(surfaceSpan, n = 15)
  x <- seq(0,1,stepYear)
  y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
  yLQ <- y
  qFactor <- qUnit@qUnitFactor
  y <- exp(y) * qFactor
  numX <- length(x)
  numY <- length(y)
  qBottom <- max(0.9 * y[1], qBottom)
  qTop <- min(1.1 * y[numY], qTop)
  xTicks <- c(0,0.0848,0.1642,0.249,0.331,0.416,0.498,0.583,0.668,0.750,0.835,0.917,1)
  xLabels <- c("Jan1","Feb1","Mar1","Apr1","May1","Jun1","Jul1","Aug1","Sep1","Oct1","Nov1","Dec1","Jan1")
  nxTicks<-length(xTicks)
  yTicks <- logPretty3(qBottom, qTop)
  nYTicks <- length(yTicks)
  numDays <- length(localDaily$Day)
  freq <- rep(0, nVectorLogQ)
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, "  ", localINFO$paramShortName, 
          "\nEstimated", surfaceName[j], "change from",year0,"to",year1)
  else ""
  
  if(flowDuration) {
    durSurf <- rep(0, 17 * nVectorLogQ)
    dim(durSurf) <- c(17, nVectorLogQ)
    centerDays <- seq(1, 388, 22.9)
    centerDays <- floor(centerDays)
    for (ix in 1:17) {
      startDay <- centerDays[ix] - span
      endDay <- centerDays[ix] + span
      goodDays <- seq(startDay, endDay, 1)
      goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
                           365)
      goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
                           365)
      numDays <- length(localDaily$Day)
      isGood <- rep(FALSE, numDays)
      for (i in 1:numDays) {
        count <- ifelse(localDaily$Day[i] == goodDays, 1, 
                        0)
        isGood[i] <- if (sum(count) > 0) 
          TRUE
        else FALSE
      }
      spanDaily <- data.frame(localDaily, isGood)
      spanDaily <- subset(spanDaily, isGood)
      n <- length(spanDaily$Day)
      LogQ <- spanDaily$LogQ
      for (jQ in 1:nVectorLogQ) {
        ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
        freq[jQ] <- sum(ind)/n
      }
      
      durSurf[ix, ] <- freq
    }
    plevels <- c(pval, 1 - pval)
    pct1 <- format(plevels[1] * 100, digits = 2)
    pct2 <- format(plevels[2] * 100, digits = 2)
    plotTitle <- if (printTitle) 
      paste(localINFO$shortName, "  ", localINFO$paramShortName, 
            "\nEstimated", surfaceName[j], "change from",year0,"to",year1,"\nBlack lines are", 
            pct1, "and", pct2, "flow percentiles")
    else ""
  }
  vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, 
                  log(yTicks[1], 10) - 1)
  v1 <- if (is.na(vert1)) 
    vectorNone
  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 
                                               10))
  v2 <- if (is.na(vert2)) 
    vectorNone
  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 
                                               10))
  h1 <- if (is.na(horiz)) 
    vectorNone
  else c(year0, log(horiz, 10), year1, log(horiz, 10))
  
  yLab <- qUnit@qUnitExpress
  filled.contour(x, log(y, 10), difft, levels = contourLevels, 
                 xlim = c(0,1), ylim = c(log(yTicks[1], 
                                             10), log(yTicks[nYTicks], 10)), main = plotTitle, 
                 xlab = "", ylab = yLab, xaxs = "i", yaxs = "i", cex.main = 0.95, 
                 plot.axes = {
                   axis(1, tcl = 0.5, at = xTicks, labels = xLabels, cex.axis=0.9)
                   axis(2, tcl = 0.5, las = 1, at = log(yTicks, 10), 
                        labels = yTicks, cex.axis=1.0)
                   axis(3, tcl = 0.5, at = xTicks, labels =FALSE)
                   axis(4, tcl = 0.5, at = log(yTicks, 10), labels=FALSE)
                   if(flowDuration) contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
                                            levels = plevels)
                   segments(v1[1], v1[2], v1[3], v1[4])
                   segments(v2[1], v2[2], v2[3], v2[4])
                   segments(h1[1], h1[2], h1[3], h1[4])
                 })
  par(oma = c(0, 0, 0, 0))
  par(mar = c(5, 4, 4, 2) + 0.1)
}
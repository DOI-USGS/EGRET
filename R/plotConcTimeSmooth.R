#' Plot up to three curves representing the concentration versus time relationship, each curve representing a different flow.
#'
#' These plots show how the concentration-time relationship is changing over flow. 
#'
#' @param q1 numeric This is the discharge value for the first curve to be shown on the plot. It is expressed in units specified by qUnit.
#' @param q2 numeric This is the discharge value for the second curve to be shown on the plot. It is expressed in units specified by qUnit. If you don't want a second curve then the argument must be q2=NA
#' @param q3 numeric This is the discharge value for the third curve to be shown on the plot. It is expressed in units specified by qUnit. If you don't want a third curve then the argument must be q3=NA
#' @param centerDate string This is the time of year to be used as the center date for the smoothing. It is expressed as a month and day and must be in the form "mm-dd"
#' @param yearStart numeric This is the starting year for the graph. The first value plotted for each curve will be at the first instance of centerDate in the year designated by yearStart.
#' @param yearEnd numeric This is the end of the sequence of values plotted on the graph.The last value will be the last instance of centerDate prior to the start of yearEnd. (Note, the number of values plotted on each curve will be yearEnd-yearStart.)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend, in the units shown on x-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param legendTop numeric which represents the top edge of the legend, in the units shown on y-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are also printed to the console (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param localSample string specifying the name of the data frame that contains the Sample data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param lwd line width, a positive number, defaulting to 1
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' q1 <- 10
#' q2 <- 25
#' q3 <- 75
#' centerDate <- "07-01"
#' yearStart <- 2000
#' yearEnd <- 2010
#' Sample <- exSample
#' INFO <- exINFO
#' plotConcTimeSmooth(q1, q2, q3, centerDate, yearStart, yearEnd)
plotConcTimeSmooth<-function (q1, q2, q3, centerDate, yearStart, yearEnd, qUnit = 2, legendLeft = 0, 
                              legendTop = 0, concMax = NA, bw = FALSE, printTitle = TRUE, 
                              printValues = FALSE, localSample = Sample, localINFO = INFO, 
                              windowY = 10, windowQ = 2, windowS = 0.5, cex.main = 1.1, lwd = 2, ...){
  
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  numQ <- 3
  
  numQ <- if (is.na(q2)){       
    1
  } else {
    3
  }
  
  numQ <- if (is.na(q3)) {
    2
  } else {
    3
  }
  qV <- rep(NA, 3)
  qVal <- rep(NA, 3)
  qFactor <- qUnit@qUnitFactor
  qV[1]<-q1
  qV[2]<-q2
  qV[3]<-q3
  qVal<- qV/qFactor
  centerDate <- paste("2001-", centerDate)
  centerDate <- as.Date(centerDate)
  centerDay <- as.POSIXlt(centerDate)$yday + 1
  centerMonth <- as.POSIXlt(centerDate)$mo + 1
  centerMDay <- as.POSIXlt(centerDate)$mday
  yDecYear <- (centerDay - 0.5)/366
  xStart <- trunc(yearStart) + yDecYear
  x <- seq(xStart,yearEnd)
  numX<-length(x)
  y <- rep(NA, 3 * numX)
  dim(y) <- c(3, numX)
  for (iCurve in 1:numQ) {
    LQ <- rep(log(qVal[iCurve]),numX)
    result <- runSurvReg(x, LQ, localSample, windowY = windowY, windowQ = windowQ, windowS = windowS)
    y[iCurve, ] <- result[, 3]
  }
  monthCenter<- as.POSIXlt(centerDate)$mon+1
  dayCenter<-as.POSIXlt(centerDate)$mday
  
  title <- if (printTitle) {
    paste(localINFO$shortName, "  ", localINFO$paramShortName, 
          "\nEstimated Concentration Versus Year\nCentered on",monthInfo[[monthCenter]]@monthFull,dayCenter,"of each year, at", 
          numQ, "specific discharges")
  } else{
    ""
  }
  qExpress = qUnit@qUnitExpress
  yLab = "Concentration in mg/L"
  #xTicks <- pretty(x,8)
  #numXTicks <- length(xTicks)
  #xLeft <- xTicks[1]
  #xRight <- xTicks[numXTicks]
  yMax <- max(y, na.rm = TRUE)
  yTop <- if (is.na(concMax)) {
    yMax
  } else {
    concMax
  }
  #yTicks <- yPretty(yTop)
  #numYTicks <- length(yTicks)
  #yTop <- yTicks[numYTicks]
  colorVal <- if (bw) {
    c("black", "black", "black")
  } else {
    c("black", "red", "green")
  } 
  lineVal <- if (bw) {
    c(1, 2, 3)
  } else {
    c(1, 1, 1)
  }
  #####################
  
  xInfo <- generalAxis(x=x, minVal=yearStart, maxVal=yearEnd)
  
  yInfo_x <- generalAxis(x=y[1,2,3,], minVal=0, maxVal=yTop)
  
  genericEGRETDotPlot(x=x, y=y[1, ],
                      xTicks=xInfo$ticks, yTicks=yInfo_x$ticks,
                      xlim = c(xInfo$bottom,xInfo$top),ylim = c(yInfo_x$bottom,yInfo_x$top),
                      ylab = yLab, plotTitle=title, 
                      type = "l", lwd = lwd, col = colorVal[1], lty = lineVal[1],
                      cex.main = cex.main, ...
  )
  #     plot(x, y[1, ], axes = FALSE, xlim = c(xLeft,xRight), xaxs = "i", xlab = "", ylim = c(0, 
  #         yTop), yaxs = "i", ylab = yLab, main = title, type = "l", 
  #         lwd = 2, col = colorVal[1], lty = lineVal[1], cex = 0.7, 
  #         cex.main = 1.1, font.main = 2, cex.lab = 1.2)
  #     axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
  #     axis(2, tcl = 0.5, las = 1, at = yTicks, labels = yTicks)
  #     axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
  #     axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
  #     box()
  par(new = TRUE)
  genericEGRETDotPlot(x=x, y=y[2, ],
                      xTicks=xInfo$ticks, yTicks=yInfo_x$ticks,
                      xlim = c(xInfo$bottom,xInfo$top),ylim = c(yInfo_x$bottom,yInfo_x$top),
                      type = "l", lwd = lwd, col = colorVal[2], lty = lineVal[2],
                      cex.main = cex.main, ...
  )
  #     plot(x, y[2, ], axes = FALSE, xlim = c(xLeft,xRight), xaxs = "i", xlab = "", ylim = c(0, 
  #         yTop), yaxs = "i", ylab = "", main = "", type = "l", 
  #         lwd = 2, col = colorVal[2], lty = lineVal[2], cex = 0.7, 
  #         cex.main = 1.1, font.main = 2, cex.lab = 1.2)
  par(new = TRUE)
  genericEGRETDotPlot(x=x, y=y[3, ],
                      xTicks=xInfo$ticks, yTicks=yInfo_x$ticks,
                      xlim = c(xInfo$bottom,xInfo$top),ylim = c(yInfo_x$bottom,yInfo_x$top),
                      type = "l", lwd = lwd, col = colorVal[3], lty = lineVal[3],
                      cex.main = cex.main, ...
  )
  #     plot(x, y[3, ], axes = FALSE, xlim = c(xLeft,xRight), xaxs = "i", xlab = "", ylim = c(0, 
  #         yTop), yaxs = "i", ylab = "", main = "", type = "l", 
  #         lwd = 2, col = colorVal[3], lty = lineVal[3], cex = 0.7, 
  #         cex.main = 1.1, font.main = 2, cex.lab = 1.2)
  legendLeft <- if (legendLeft == 0) {
    xInfo$bottom + 2
  } else {
    legendLeft
  }
  legendTop <- if (legendTop == 0) {
    0.3 * yTop
  } else {
    legendTop
  }
  words <- paste(qV[1:numQ],qUnit@qUnitName)
  ltys <- lineVal[1:numQ]
  cols <- colorVal[1:numQ]
  legend(legendLeft, legendTop, legend = words, lty = ltys, 
         col = cols, lwd = 2, cex = 1.3)
  printResults <- rep(NA, numX * 4)
  dim(printResults) <- c(numX, 4)
  for (j in 1:numX) {
    printResults[j, 1] <- format(x[j], width = 9)
    printResults[j, 2:4] <- format(y[1:3, j], width = 10)
  }
  qPrint<-format(qV,width=10)
  topLine <- c("  year  ", qPrint)
  if (printValues) {
    write(topLine, file = "", ncolumns = 4)
  }
  if (printValues) {
    write.table(printResults, file = "", quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

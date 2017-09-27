#' Plot up to three curves representing the concentration versus time relationship, each curve representing a different flow.
#'
#' @description
#' These plots show how the concentration-time relationship is changing over flow.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param q1 numeric This is the discharge value for the first curve to be shown on the plot. It is expressed in units specified by qUnit.
#' @param q2 numeric This is the discharge value for the second curve to be shown on the plot. It is expressed in units specified by qUnit. If you don't want a second curve then the argument must be q2=NA
#' @param q3 numeric This is the discharge value for the third curve to be shown on the plot. It is expressed in units specified by qUnit. If you don't want a third curve then the argument must be q3=NA
#' @param centerDate character This is the time of year to be used as the center date for the smoothing. It is expressed as a month and day and must be in the form "mm-dd"
#' @param yearStart numeric This is the starting year for the graph. The first value plotted for each curve will be at the first instance of centerDate in the year designated by yearStart.
#' @param yearEnd numeric This is the end of the sequence of values plotted on the graph.The last value will be the last instance of centerDate prior to the start of yearEnd. (Note, the number of values plotted on each curve will be yearEnd-yearStart.)
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend in the units of the plot.
#' @param legendTop numeric which represents the top edge of the legend in the units of the plot.
#' @param printLegend logicalif TRUE, legend is included
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param concMin numeric value for lower limit on concentration shown on the vertical log graph, default is NA 
#' (which causes the lower limit to be set automatically, based on the data). This value is ignored for linear scales, using 0 as the minimum value for the concentration axis.
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are printed to the console and returned in a dataframe (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param logScale logical whether or not to use a log scale in the y axis.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param lwd line width, a positive number, defaulting to 1
#' @param cex.legend number magnification  of legend
#' @param colors color vector of lines on plot, see ?par 'Color Specification'. Defaults to c("black","red","green")
#' @param lineVal vector of line types. Defaults to c(1,1,1) which is a solid line for each line. Options: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @keywords water-quality statistics graphics
#' @export
#' @seealso \code{\link{genericEGRETDotPlot}}, \code{\link{runSurvReg}}
#' @examples 
#' q1 <- 10
#' q2 <- 25
#' q3 <- 75
#' centerDate <- "07-01"
#' yearStart <- 2000
#' yearEnd <- 2010
#' eList <- Choptank_eList
#' plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd)
#' plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd,logScale=TRUE)
plotConcTimeSmooth<-function (eList, q1, q2, q3, centerDate, yearStart, yearEnd, qUnit = 2, legendLeft = 0, 
                              legendTop = 0, concMax = NA, concMin=NA,bw = FALSE, printTitle = TRUE, colors=c("black","red","green"), 
                              printValues = FALSE, tinyPlot=FALSE, minNumObs = 100, minNumUncen =  50, 
                              windowY = 10, windowQ = 2, windowS = 0.5, cex.main = 1.1, lwd = 2, printLegend = TRUE,
                              cex.legend = 1.2, cex=0.8, cex.axis=1.1, customPar=FALSE,lineVal=c(1,1,1),logScale=FALSE,
                              edgeAdjust=TRUE,...){
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(all(c("numDays","DecLow","DecHigh") %in% names(localINFO))){
    numDays <- localINFO$numDays
    DecLow <- localINFO$DecLow
    DecHigh <- localINFO$DecHigh
  } else {
    numDays <- localSample$Julian[nrow(localSample)] - localSample$Julian[1] + 1
    DecLow <- localSample$DecYear[1]
    DecHigh <- localSample$DecYear[nrow(localSample)]
  } 
  
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  
  numQ <- sum(!is.na(c(q1, q2, q3)))
  
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
  
  index <- which(!is.na(c(q1, q2, q3)))[1:numQ]
  
  for (iCurve in 1:numQ) {
    LQ <- rep(log(qVal[index[iCurve]]),numX)
    result <- runSurvReg(x, LQ,DecLow,DecHigh, 
                         localSample, windowY = windowY,
                         windowQ = windowQ, windowS = windowS,minNumObs=minNumObs, 
                         minNumUncen = minNumUncen, verbose=FALSE,
                         edgeAdjust = edgeAdjust, run.parallel = FALSE)
    y[index[iCurve], ] <- result[, 3]
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

  yMax <- max(y, na.rm = TRUE)
  yTop <- concMax
  
  if (is.na(concMax)) {
    yTop <- yMax
  } 

  
  if(logScale){
    logText <- "y"
  } else {
    logText <- ""
    concMin <- 0
  }
  
  colorVal <- colors
  
  if (bw) {
    colorVal <- c("black", "black", "black")
  } 
  
  if (bw){
    lineVal <- c(1, 2, 3)
  }
  
  #####################
  
  xInfo <- generalAxis(x=x, minVal=yearStart, maxVal=yearEnd, tinyPlot=tinyPlot)  
  combinedY <- c(y[1,], y[2,],y[3,])
  yInfo <- generalAxis(x=combinedY, minVal=concMin, maxVal=yTop, 
                       tinyPlot=tinyPlot,logScale=logScale, units=localINFO$param.units)
  
  genericEGRETDotPlot(x=x, y=y[1, ],
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim = c(xInfo$bottom,xInfo$top),ylim = c(yInfo$bottom,yInfo$top),
                      ylab = yInfo$label, plotTitle=title, customPar=customPar,log=logText,
                      type = "l", lwd = lwd, col = colorVal[1], lty = lineVal[1],xDate=TRUE,
                      cex.main = cex.main, tinyPlot=tinyPlot,cex=cex,cex.axis=cex.axis,...
  )


  lines(x=x, y=y[2, ], col=colorVal[2], lwd=lwd, lty=lineVal[2])
  lines(x=x, y=y[3, ], col=colorVal[3], lwd=lwd, lty=lineVal[3])

  words <- paste(qV[index],qUnit@qUnitName)
  ltys <- lineVal[index]
  cols <- colorVal[index]
  
  legendLeft <- if(legendLeft == 0) {
    grconvertX(0.05, from="npc", to="user")
  } else {
    legendLeft
  }
  
  if(legendTop == 0) {
    legendTop <- grconvertY(0.3, from="npc", to="user") 
  } 
  
  if (printLegend) legend(legendLeft,legendTop,legend=words,lty=ltys,col=cols,lwd=lwd,cex=cex.legend)
  
  printResults <- rep(NA, numX * 4)
  dim(printResults) <- c(numX, 4)
  for (j in 1:numX) {
    printResults[j, 1] <- format(x[j], width = 9)
    printResults[j, 2:4] <- format(y[1:3, j], width = 10)
  }
  qPrint<-format(qV,width=10)
  topLine <- c("\n  year  ", qPrint)
  if (printValues) {
#     write(topLine, file = "", ncolumns = 4)
#     write.table(printResults, file = "", quote = FALSE, row.names = FALSE, 
#                 col.names = FALSE)
    cat("\n")
    returnDF <- data.frame(year=x, q1=y[1,], q2=y[2,], q3=y[3,])
    colnames(returnDF) <- c("year",q1,q2,q3)
    return(returnDF)
  }
}
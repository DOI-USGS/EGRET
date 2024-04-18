#' Plots the difference between two years from a contour plot created by plotContours
#'
#' @description
#' These plots are normally used for plotting changes in the estimated concentration surface (whatSurface=3) but can be used to explore the 
#' changes in estimated surfaces for the log of concentration or for the standard error (in log space) which is what determines the bias correction.
#'
#' The difference can be shown either as an arithmetic difference or as a percentage difference.
#'
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and surfaces matrix
#' @param year0 numeric value for the calendar year that is the first year of the pair of years for the analysis, should be a whole number
#' @param year1 numeric value for the calendar year that is the second year of the pair of years for the analysis, should be a whole number
#' @param qBottom numeric value for the bottom edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit). NA will choose a "pretty" lower limit nearest to the 5\% of discharge. If yTicks are specified, then the first value of yTicks becomes the lowest discharge shown on the figure.
#' @param qTop numeric value for the top edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit). NA will choose a "pretty" upper limit nearest to the 95\% of discharge. If yTicks are specified, then the last value of yTicks becomes the highest discharge shown on the figure.
#' @param maxDiff numeric value which is the absolute value of the largest change in concentration that will be shown on the figure. If NA, the scale will be set from 5\% to 95\% of the concentration difference. If plotPercent = TRUE then maxDiff will be the maximum percentage difference.
#' @param whatSurface numeric value, can only accept 1, 2, or 3;  whatSurface = 1 is yHat (log concentration), whatSurface = 2 is SE (standard error of log concentration), and whatSurface = 3 is ConcHat (unbiased estimate of concentration), default = 3
#' @param plotPercent logical. If TRUE, plots percent difference, if FALSE, plots arithmetic differences. Defaults to FALSE.
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param span numeric, it is the half-width (in days) of the smoothing window for computing the flow duration information, default = 60
#' @param pval numeric, the probability value for the lower flow frequency line on the graph
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param vert1 numeric, the location in time for a black vertical line on the figure, yearStart < vert1 < yearEnd, default is NA (vertical line is not drawn) 
#' @param vert2 numeric, the location in time for a black vertical line on the figure, yearStart < vert2 < yearEnd, default is NA (vertical line is not drawn)
#' @param horiz numeric, the location in discharge for a black horizontal line on the figure, qBottom<vert1<qTop, default is NA (no horizontal line is drawn)
#' @param flowDuration logical variable if TRUE plot the flow duration lines (5 and 95 flow percentiles), if FALSE do not plot them, default = TRUE
#' @param yTicks vector of yTick labels and marks that will be plotted in log space. (for example yTicks = c(3, 5, 10, 20, 50, 100, 200, 400). The first and last values determine the range of the y axis. If NA, the tick marks will be automatically generated.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param lwd numeric, line width of flowDuration curve, default is 1
#' @param tcl numeric, length of tick marks in inches, default is 0.1
#' @param tick.lwd line width for axis ticks, default is 2
#' @param color.palette a function that creates a color palette for the contour plot. Default goes from blue to white to red 
#' using the function \code{grDevices::colorRampPalette(c("blue","white","red"))}. A few preset options are heat.colors, topo.colors, and terrain.colors.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins.
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels. 
#' @param concLab object of concUnit class, or numeric represented the short code, 
#' or character representing the descriptive name. By default, this argument sets
#' concentration labels to use either Concentration or Conc (for tiny plots). Units
#' are taken from the eList$INFO$param.units. To use any other words than
#' "Concentration" see \code{vignette(topic = "units", package = "EGRET")}.
#' @param monthLab object of monthLabel class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' year0 <- 1990
#' year1 <- 2009
#' qBottom <- 0.5
#' qTop <- 20
#' maxDiff<-0.5
#' eList <- Choptank_eList
#' plotDiffContours(eList, year0, year1, qBottom, qTop, maxDiff = 0.5)
#' plotDiffContours(eList, year0, year1, qBottom, qTop, maxDiff = 50, plotPercent = TRUE)
plotDiffContours<-function (eList, year0, year1, 
                            qBottom = NA, qTop = NA, maxDiff = NA, 
                            whatSurface = 3, tcl=0.03,
                            qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, plotPercent = FALSE,
                            vert1 = NA, vert2 = NA, horiz = NA, flowDuration = TRUE, yTicks=NA,tick.lwd=1,
                            lwd=2,cex.main = 0.95, cex.axis = 1,
                            customPar = FALSE, usgsStyle = FALSE,
                            color.palette = grDevices::colorRampPalette(c("blue","white","red")),
                            concLab = 1, monthLab = 1, ...) {

  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  
  if (is.numeric(concLab)){
    concPrefix <- concConst[shortCode=concLab][[1]]    
  } else if (is.character(concLab)){
    concPrefix <- concConst[concLab][[1]]
  } else {
    concPrefix <- concLab
  }
  
  if (is.numeric(monthLab)){
    monthInfo <- monthInfo[shortCode=monthLab][[1]]    
  } else if (is.character(monthLab)){
    monthInfo <- monthInfo[monthLab][[1]]
  } else {
    monthInfo <- monthLab
  }
  
  if(!customPar){
    par(mgp=c(2.5,0.5,0))
  }

  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
                   concPrefix@longPrefix)
  j <- 3
  j <- if (whatSurface == 1) 1 else j
  j <- if (whatSurface == 2) 2 else j
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
  if (plotPercent) {
    diff<-(surf[,start1:end1,j] - surf[,start0:end0,j])*100/surf[,start0:end0,j]
  } else {
    diff<-surf[,start1:end1,j] - surf[,start0:end0,j]
  }
  difft<-t(diff)
  if(!is.na(maxDiff)){
    if(length(maxDiff) == 1){
      surfaceSpan <- c(-maxDiff,maxDiff)
    } else {
      surfaceSpan <- range(maxDiff)
    }
  } else {
    surfaceSpan <- stats::quantile(difft, c(0.05,0.95))
  }
  
  contourLevels <- pretty(surfaceSpan, n = 15)
  x <- seq(0,1,stepYear)
  y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
  yLQ <- y
  qFactor <- qUnit@qUnitFactor
  y <- exp(y) * qFactor
  numX <- length(x)
  numY <- length(y)
  
  qBottomT <- ifelse(is.na(qBottom), stats::quantile(localDaily$Q, probs = 0.05)*qFactor, qBottom)
  
  qTopT <- ifelse(is.na(qTop), stats::quantile(localDaily$Q, probs = 0.95)*qFactor, qTop)
  
  if(any(is.na(yTicks))){
    
    if(is.na(qBottom)){
      qBottom <- max(0.9*y[1],qBottomT)
    }
    if(is.na(qTop)){
      qTop <- min(1.1*y[numY],qTopT)
    }
    yTicks <- logPretty3(qBottom,qTop)
  }

  xTicks <- c(0,0.0848,0.1642,0.249,0.331,0.416,0.498,0.583,0.668,0.750,0.835,0.917,1)
  xLabels <- c(monthInfo@monthAbbrev, monthInfo@monthAbbrev[1])
  xLabels <- paste0(xLabels, "1")
  nxTicks<-length(xTicks)
  
  nYTicks <- length(yTicks)
  numDays <- length(localDaily$Day)
  freq <- rep(0, nVectorLogQ)
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
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
      isGood <- localDaily$Day %in% goodDays 

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

    firstLine <- paste(localINFO$shortName,"  ",localINFO$paramShortName,sep="")
    secondLine <- if (plotPercent){
      paste("\nEstimated", surfaceName[j], "percent change from",year0,"to",year1)
    } else {
      paste("\nEstimated", surfaceName[j], "change from",year0,"to",year1)
    }
    thirdLine <- paste("\nBlack lines are", pct1, "and", pct2, "flow percentiles")
    plotTitle <- paste(firstLine,secondLine,thirdLine)

  }
  vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, 
                  log(yTicks[1], 10) - 1)
  v1 <- if (is.na(vert1)) 
    vectorNone
  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 10))
  v2 <- if (is.na(vert2)) 
    vectorNone
  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 10))
  h1 <- if (is.na(horiz)) 
    vectorNone
  else c(year0, log(horiz, 10), year1, log(horiz, 10))
  
  deltaY <- (log(yTicks[length(yTicks)],10)-log(yTicks[1],10))/25
  deltaX <- (1)/25
  
  yLab <- ifelse(usgsStyle,qUnit@unitUSGS,qUnit@qUnitExpress)
  graphics::filled.contour(x, log(y, 10), difft, levels = contourLevels, 
                 xlim = c(0,1), ylim = c(log(yTicks[1], 
                                             10), log(yTicks[nYTicks], 10)), #main = plotTitle, 
                 xaxs = "i", yaxs = "i",
                 plot.axes = {
                   axis(1, tcl = 0, at = xTicks, labels = xLabels, cex.axis=0.9*cex.axis)
                   axis(2, tcl = 0, las = 1, at = log(yTicks, 10), 
                        labels = yTicks, cex.axis=cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels =FALSE)
                   axis(4, tcl = 0, at = log(yTicks, 10), labels=FALSE)
                   if(flowDuration) graphics::contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
                                            levels = plevels,lwd=lwd)
                   segments(v1[1], v1[2], v1[3], v1[4])
                   segments(v2[1], v2[2], v2[3], v2[4])
                   segments(h1[1], h1[2], h1[3], h1[4])
                   
                   segments(xTicks, rep(log(yTicks[1],10),length(xTicks)), xTicks, rep(graphics::grconvertY(graphics::grconvertY(par("usr")[3],from="user",to="inches")+tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks],10),length(xTicks)), xTicks, rep(graphics::grconvertY(graphics::grconvertY(par("usr")[4],from="user",to="inches")-tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(rep(0,length(yTicks)), log(yTicks,10), rep(graphics::grconvertX(graphics::grconvertX(par("usr")[1],from="user",to="inches")+tcl,from="inches",to="user"),length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   segments(rep(graphics::grconvertX(graphics::grconvertX(par("usr")[2],from="user",to="inches")-tcl,from="inches",to="user"),length(yTicks)), log(yTicks,10), rep(1,length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   
                 },
                  plot.title = {
                    if(printTitle) {
                       title(main = plotTitle, ylab = yLab,cex.main = cex.main)
                     } else {
                       title(main = NULL, ylab = yLab)
                     }
                  }, 
                 color.palette=color.palette,...)

}
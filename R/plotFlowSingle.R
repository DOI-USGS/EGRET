#' Creates a plot of a time series of a particular flow statistic and a lowess smooth of that flow statistic
#'
#' @description
#' A part of the flowHistory system.
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param qMax A numeric value for the maximum value to be used for y-axis of graph, default is NA means that graph is self-scaling
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param printStaName logical variable, if TRUE station name is printed in title, if FALSE not printed, default is TRUE
#' @param printPA logical variable, if TRUE Period of Analysis information is printed in title, if FALSE not printed, default is TRUE
#' @param printIstat logical variable, if TRUE print the statistic name is printed in title, if FALSE not printed, default is TRUE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param lwd number line width
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{makeAnnualSeries}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotFlowSingle(eList, 1)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotFlowSingle(eList, 1)
plotFlowSingle<-function(eList, istat,yearStart=NA, yearEnd = NA,
                  qMax = NA, printTitle = TRUE, tinyPlot = FALSE, customPar=FALSE,
                  runoff = FALSE, qUnit = 1, printStaName = TRUE, printPA = TRUE, 
                  printIstat = TRUE,cex=0.8, cex.axis=1.1,cex.main=1.1, lwd=2, col="black",...) {
  
  localAnnualSeries <- makeAnnualSeries(eList)
  localINFO <- getInfo(eList)
  
  qActual<-localAnnualSeries[2,istat,]
  qSmooth<-localAnnualSeries[3,istat,]
  years<-localAnnualSeries[1,istat,]

  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  
  if (sum(c("paStart", "paLong", "window") %in% names(localINFO)) == 
        3) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
    window <- localINFO$window
  } else {
    paLong <- 12
    paStart <- 10
    window <- 20
  }
  
  qFactor<-qUnit@qUnitFactor
  qActual<-if(runoff) qActual*86.4/localINFO$drainSqKm else qActual*qFactor
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  localSeries<-data.frame(years,qActual,qSmooth)
  localSeries<-if(is.na(yearStart)) localSeries else subset(localSeries,years>=yearStart)
  localSeries<-if(is.na(yearEnd)) localSeries else subset(localSeries,years<=yearEnd)

  yInfo <- generalAxis(x=qActual, maxVal=qMax, minVal=0,tinyPlot=tinyPlot)
  xInfo <- generalAxis(x=localSeries$years, maxVal=yearEnd, minVal=yearStart, padPercent=0,tinyPlot=tinyPlot)
  
  line1<-if(printStaName) localINFO$shortName else ""	
  line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong)) else ""
  nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
  line3<-if(printIstat) paste("\n",nameIstat[istat]) else ""
  title<-if(printTitle) paste(line1,line2,line3) else ""
  
  ##############################################
  
  if(tinyPlot){
    yLab <- qUnit@qUnitTiny
    title<-if(printTitle) paste(nameIstat[istat]) else ""
  } else {
    yLab <- qUnit@qUnitExpress
  }
  
  yLab <- if(runoff) "Runoff in mm/day" else yLab

  genericEGRETDotPlot(x=localSeries$years, y=localSeries$qActual, 
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab="", ylab=yLab, customPar=customPar,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,cex=cex,
                      plotTitle=title, cex.axis=cex.axis,cex.main=cex.main, 
                      tinyPlot=tinyPlot,lwd=lwd,col=col,...
  )
  
  ##############################################
  
  lines(localSeries$years,localSeries$qSmooth,lwd=lwd,col=col)

}
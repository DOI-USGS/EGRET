#' Creates a plot of a time series of a particular flow statistic and a lowess smooth of that flow statistic
#'
#' A part of the flowHistory system.
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum
#'
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param localINFO A character string specifying the name of the metadata data frame
#' @param localAnnualSeries A character string specifying the name of a data frame containing the annual series
#' @param qMax A numeric value for the maximum value to be used for y-axis of graph, default is NA means that graph is self-scaling
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param printStaName logical variable, if TRUE station name is printed in title, if FALSE not printed, default is TRUE
#' @param printPA logical variable, if TRUE Period of Analysis information is printed in title, if FALSE not printed, default is TRUE
#' @param printIstat logical variable, if TRUE print the statistic name is printed in title, if FALSE not printed, default is TRUE
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' INFO <- exINFO
#' annualSeries <- exannualSeries
#' plotFlowSingle(8)
plotFlowSingle<-function(istat,yearStart=NA, yearEnd = NA, 
                  localINFO = INFO, localAnnualSeries = annualSeries, 
                  qMax = NA, printTitle = TRUE, tinyPlot = FALSE, 
                  runoff = FALSE, qUnit = 1, printStaName = TRUE, printPA = TRUE, 
                  printIstat = TRUE) {
  
  qActual<-localAnnualSeries[2,istat,]
  qSmooth<-localAnnualSeries[3,istat,]
  years<-localAnnualSeries[1,istat,]
#   par(mar =  c(3,2,5,1))
#   if(!tinyPlot) par(pty="s")

  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  qFactor<-qUnit@qUnitFactor
  qActual<-if(runoff) qActual*86.4/localINFO$drainSqKm else qActual*qFactor
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  localSeries<-data.frame(years,qActual,qSmooth)
  localSeries<-if(is.na(yearStart)) localSeries else subset(localSeries,years>=yearStart)
  localSeries<-if(is.na(yearEnd)) localSeries else subset(localSeries,years<=yearEnd)
  minYear<-min(localSeries$years,na.rm=TRUE)
  maxYear<-max(localSeries$years,na.rm=TRUE)
  xLeft<-if(is.na(yearStart)) minYear else yearStart
  numYears<-length(localSeries$years)
  xRight<-if(is.na(yearEnd)) maxYear else yearEnd
  nTicks<-if(tinyPlot) 5 else 8
  xSpan<-c(xLeft,xRight)
  xTicks<-pretty(xSpan,n=nTicks)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTop<-if(is.na(qMax)) 1.1*max(qActual,na.rm=TRUE) else qMax
  ySpan<-c(0,yTop)
  yTicks<-pretty(ySpan,n=nTicks)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  yLab<-if(runoff) "mm/day" else qUnit@qUnitExpress
  line1<-if(printStaName) localINFO$shortName else ""	
  line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)) else ""
  nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
  line3<-if(printIstat) paste("\n",nameIstat[istat]) else ""
  title<-if(printTitle) paste(line1,line2,line3) else ""
  
  ##############################################
  genericEGRETDotPlot(x=localSeries$years, y=localSeries$qActual, 
                      xlim=c(xLeft,xRight), ylim=c(yBottom,yTop),
                      xlab="", ylab=yLab,
                      xTicks=xTicks, yTicks=yTicks,cex=0.8,
                      plotTitle=title, mar=c(5,6,5,2), cex.axis=1.1,cex.main=1.1
  )
  
#   par(mar =  c(5,6,5,2))
#   plot(localSeries$years,localSeries$qActual,axes=FALSE,
#         xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(yBottom,yTop),yaxs="i",
#         ylab=yLab,main=title,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2,pch=20)
#   axis(1,tcl=0.5,at=xTicks,labels=xTicks)
#   axis(2,tcl=0.5,las=1,at=yTicks,labels=yTicks,cex.axis=1.1)
#   axis(3,tcl=0.5,at=xTicks,labels=FALSE)
#   axis(4,tcl=0.5,at=yTicks,labels=FALSE)
#   box()
  ##############################################
  
  lines(localSeries$years,localSeries$qSmooth,lwd=2)
  par(mar=c(5,4,4,2)+0.1)
}
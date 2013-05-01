#' Graph of the standard deviation of the log of daily discharge versus year
#'
#' Data come from data frame named Daily.
#' The metadata come from a data frame named INFO.
#' User must have already run the function, INFO<-setPA()
#' Can be used to analyze a specific period of analysis by
#' Running INFO<-setPA(paStart,paLong)
#'
#' @param yearStart numeric is the calendar year of the first value to be included in graph, default is NA, which plots from the start of the period of record
#' @param yearEnd numeric is the calendar year of the last value to be included in graph, default is NA, which plots to the end of the period of record
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @param localDaily string specifying the name of the data frame that contains the daily streamflow data, default name is Daily
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param sdMax numeric is the maximum value to be used on the vertical axis of the graph, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure), default is TRUE
#' @param tinyPlot logical variable if TRUE plot is designed to be small, if FALSE it is designed for page size, default is FALSE (not fully implemented yet)
#' @param printStaName logical variable, if TRUE print the station name, if FALSE do not, default is TRUE
#' @param printPA logical variable, if TRUE print the period of analysis information in the plot title, if FALSE leave it out, default is TRUE
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param lwd line width, a positive number, defaulting to 1
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' Daily <- exDaily
#' INFO <- exINFO
#' plotSDLogQ(window=3,printTitle=FALSE)  
plotSDLogQ<-function(yearStart=NA,yearEnd=NA,window=15,localDaily=Daily,
                     localINFO=INFO,sdMax=NA,printTitle = TRUE, tinyPlot = FALSE, 
                     printStaName = TRUE, printPA = TRUE, cex=0.8,
                     cex.main=1.1,cex.axis = 1.1,lwd=2, ...){
  par(mar = c(5,6,5,2))
#   par(mar =  c(3,2,5,1))
#   if(!tinyPlot) par(pty="s")
  numDays<-length(localDaily$LogQ)
  paLong <- localINFO$paLong
  paStart <- localINFO$paStart
  localDaily <- if(paLong == 12) localDaily else selectDays(paLong,paStart,localDaily)
  numDays<-length(localDaily$LogQ)
  startDec<-localDaily$DecYear[1]
  endDec<-localDaily$DecYear[numDays]
  startDays<-seq(startDec,endDec-window,0.1)
  numResults<-length(startDays)
  y<-rep(NA,numResults)
  xmid<-startDays+(window/2)
  for (i in 1:numResults){
    firstDay<-startDays[i]
    lastDay<-startDays[i]+window
    smallDaily<-subset(localDaily,DecYear>=firstDay&DecYear<=lastDay)
    y[i]<-sd(smallDaily$LogQ,na.rm=TRUE)
  }
  yTop<-if(is.na(sdMax)) 1.05*max(y)
  #yTicks<-yPretty(yTop)
  #numYTicks<-length(yTicks)
  #yTop<-yTicks[numYTicks]
  xMin<-if(is.na(yearStart)) startDec else yearStart
  xMax<-if(is.na(yearEnd)) endDec else yearEnd
  #nTicks <- if (tinyPlot) 5 else 8
  #yearSpan<-c(xMin,xMax)
  #xTicks<- pretty(yearSpan,n = nTicks)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  line1<-if(printStaName) localINFO$shortName else ""
  line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)) else ""
  line3<-"\nDischarge variability: Standard Deviation of Log(Q)" 
  title<-if(printTitle) paste(line1,line2,line3) else ""
  
  ##############################################
  
  xInfo <- generalAxis(x=xmid, minVal=yearStart, maxVal=yearEnd, tinyPlot=tinyPlot, year_search=TRUE)
  yInfo <- generalAxis(x=y, minVal=0, maxVal=yTop, tinyPlot=tinyPlot)

  genericEGRETDotPlot(x=xmid,y=y,
                      xlim=c(xInfo$bottom,xInfo$top),ylim=c(yInfo$bottom,yInfo$top),
                      xlab="",ylab="Dimensionless",
                      xTicks=xInfo$ticks,yTicks=yInfo$ticks,cex=cex,
                      plotTitle=title, cex.main=cex.main, cex.axis = cex.axis,
                      type="l", lwd=lwd, ...
  )
  
#   plot(xmid,y,type="l",ylim=c(0,yTop),yaxs="i",lwd=2,xlim=c(xLeft,xRight),xaxs="i",main=title,xlab="",ylab="Dimensionless",axes=FALSE,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2)
#   axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
#   axis(2, tcl = 0.5, las = 1, at = yTicks, cex.axis = 1.1)
#   axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
#   axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
#   box()
  ##############################################
  
  par(mar = c(5, 4, 4, 2) + 0.1)
}
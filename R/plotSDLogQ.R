#' Graph of the standard deviation of the log of daily discharge versus year
#'
#' Data come from data frame named Daily.
#' The metadata come from a data frame named INFO.
#' User must have already run the function, \code{INFO<-setPA()}
#' Can be used to analyze a specific period of analysis by
#' Running \code{INFO<-setPA(paStart,paLong)}
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#'  contains an INFO and Daily dataframes, then the following R code will produce a plot:
#'  \code{plotSDLogQ()}
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param yearStart numeric is the calendar year of the first value to be included in graph, default is NA, which plots from the start of the period of record
#' @param yearEnd numeric is the calendar year of the last value to be included in graph, default is NA, which plots to the end of the period of record
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @param sdMax numeric is the maximum value to be used on the vertical axis of the graph, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure), default is TRUE
#' @param tinyPlot logical variable if TRUE plot is designed to be small, if FALSE it is designed for page size, default is FALSE (not fully implemented yet)
#' @param printStaName logical variable, if TRUE print the station name, if FALSE do not, default is TRUE
#' @param printPA logical variable, if TRUE print the period of analysis information in the plot title, if FALSE leave it out, default is TRUE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param lwd line width, a positive number, defaulting to 1
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotSDLogQ(eList) 
#' plotSDLogQ(eList, 1998,2000) 
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotSDLogQ(eList) 
plotSDLogQ<-function(eList, yearStart=NA,yearEnd=NA,window=15,sdMax=NA,
                     printTitle = TRUE, tinyPlot = FALSE, 
                     printStaName = TRUE, printPA = TRUE, cex=0.8,
                     cex.main=1.1,cex.axis = 1.1,lwd=2, customPar=FALSE, ...){

  localINFO <- info(eList)
  localDaily <- daily(eList)
  
  numDays<-length(localDaily$LogQ)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
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
    smallDaily<-localDaily[localDaily$DecYear >= firstDay & localDaily$DecYear <= lastDay,]
    y[i]<-sd(smallDaily$LogQ,na.rm=TRUE)
  }


  xMin<-if(is.na(yearStart)) startDec else yearStart
  xMax<-if(is.na(yearEnd)) endDec else yearEnd

  line1<-if(printStaName) localINFO$shortName else ""
  line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)) else ""
  line3<-"\nDischarge variability: Standard Deviation of Log(Q)" 
  title<-if(printTitle) paste(line1,line2,line3) else ""
  
  if(tinyPlot){
    title<-if(printTitle) "standard deviation of log(Q)"
  }

  ##############################################
  
  xInfo <- generalAxis(x=xmid, minVal=yearStart, maxVal=yearEnd, tinyPlot=tinyPlot,padPercent=0)
  yInfo <- generalAxis(x=y, minVal=0, maxVal=sdMax, tinyPlot=tinyPlot,padPercent=5)

  genericEGRETDotPlot(x=xmid,#localDaily$DecYear,
                      y=y,
                      xlim=c(xInfo$bottom,xInfo$top),ylim=c(yInfo$bottom,yInfo$top),
                      xlab="",ylab="Dimensionless",xDate=TRUE,
                      xTicks=xInfo$ticks,yTicks=yInfo$ticks,cex=cex,tinyPlot=tinyPlot,
                      plotTitle=title, cex.main=cex.main, cex.axis = cex.axis,
                      type="l", lwd=lwd, customPar=customPar, ...
  )

}
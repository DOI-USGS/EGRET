#' Plot of the time series of daily concentration estimates and the sample values for the days that were sampled
#'
#' @description
#' This plot is useful for visual examination of the ability of the WRTDS, or other model, to fit the 
#' data, seen in a time-series perspective. 
#' The graph is most useful when it covers a period of just a few years and not the complete record
#' but a complete record can be done by repeated use over a series of segments.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#' contains an INFO, Daily, and Sample dataframes, then the following R code will produce a plot:
#' \code{plotConcTimeDaily()}
#'
#' @param startYear numeric specifying the starting date (expressed as decimal years, for example 1989.0) for the plot
#' @param endYear numeric specifiying the ending date for the plot 
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @param localDaily data frame that contains the flow data, default name is Daily 
#' @param localINFO data frame that contains the metadata, default name is INFO
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' # Water year:
#' INFO <- setPA()
#' plotConcTimeDaily()
#' plotConcTimeDaily(startYear=1998,endYear=2001)
#' # Graphs consisting of Jun-Aug
#' INFO <- setPA(paStart=6,paLong=3)
#' plotConcTimeDaily()
plotConcTimeDaily<-function(startYear=NA, endYear=NA, localSample = Sample, 
                            localDaily = Daily, localINFO = INFO, tinyPlot = FALSE, 
                            concMax = NA, printTitle = TRUE,cex=0.8, cex.axis=1.1,
                            cex.main=1.1, customPar=FALSE,col="black",lwd=1,...){

  startYear <- if (is.na(startYear)) as.integer(min(localSample$DecYear,na.rm=TRUE)) else startYear
  endYear <- if (is.na(endYear)) as.integer(max(localSample$DecYear,na.rm=TRUE)) else endYear
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(paLong,paStart,localDaily=localSample)
  localDaily <- if(paLong == 12) localDaily else selectDays(paLong,paStart,localDaily=localDaily)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  subSample<-subset(localSample,DecYear>=startYear)
  subSample<-subset(subSample,DecYear<=endYear)
  subDaily<-subset(localDaily,DecYear>=startYear)
  subDaily<-subset(subDaily,DecYear<=endYear)
  xSample<-subSample$DecYear
  xDaily<-subDaily$DecYear

  yLow<-subSample$ConcLow
  yHigh<-subSample$ConcHigh
  Uncen<-subSample$Uncen

  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed and Estimated Concentration versus Time") else ""
  
  ###################################
  
  if (tinyPlot) {
    yLab = "Conc. (mg/L)"
  } else {
    yLab = "Concentration in mg/L"
  }
  yBottom <- 0 #Not specified within script, added under assumption that it's always zero based on ylim definition in this function
  
  xInfo <- generalAxis(x=xSample, minVal=startYear, maxVal=endYear, tinyPlot=tinyPlot,padPercent=0)  
  
  yCombined <- c(yHigh,subDaily$ConcDay)
  yInfo <- generalAxis(x = yCombined, minVal = yBottom, maxVal = concMax, 
                       tinyPlot = tinyPlot, padPercent = 5)
  
  genericEGRETDotPlot(x=xSample, y=yHigh, xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      ylab=yLab,plotTitle=plotTitle,cex.axis=cex.axis,col=col,lwd=lwd,cex=cex,
                      cex.main=cex.main, tinyPlot=tinyPlot,customPar=customPar, xDate=TRUE,...
  )

  lines(x=xDaily, y=subDaily$ConcDay, type="l",col=col,lwd=lwd)

  censoredSegments(yInfo$bottom,yLow=yLow,yHigh=yHigh,x=xSample,Uncen=Uncen,col=col,lwd=lwd)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}
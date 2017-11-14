#' Plot of the time series of daily concentration estimates and the sample values for the days that were sampled
#'
#' @description
#' This plot is useful for visual examination of the ability of the WRTDS, or other model, to fit the 
#' data, seen in a time-series perspective. 
#' The graph is most useful when it covers a period of just a few years and not the complete record
#' but a complete record can be done by repeated use over a series of segments.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param yearStart numeric specifying the starting date (expressed as decimal years, for example 1989.0) for the plot
#' @param yearEnd numeric specifiying the ending date for the plot 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
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
#' @param randomCensored logical. Show censored values as randomized.
#' @param prettyDate logical use 'pretty' limits for date axis if TRUE, or force the yearStart/yearEnd as limits if FALSE
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotConcTimeDaily(eList)
#' plotConcTimeDaily(eList, yearStart=1998,yearEnd=2001)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcTimeDaily(eList)
plotConcTimeDaily<-function(eList, yearStart=NA, yearEnd=NA, tinyPlot = FALSE, 
                            concMax = NA, printTitle = TRUE,cex=0.8, cex.axis=1.1,randomCensored=FALSE,
                            cex.main=1.1, customPar=FALSE,col="black",lwd=1,prettyDate=TRUE,...){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  yearStart <- if (is.na(yearStart)) as.integer(min(localSample$DecYear,na.rm=TRUE)) else yearStart
  yearEnd <- if (is.na(yearEnd)) as.integer(max(localSample$DecYear,na.rm=TRUE)) else yearEnd
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample, paLong,paStart)
  localDaily <- if(paLong == 12) localDaily else selectDays(localDaily,paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  subSample<-localSample[localSample$DecYear>=yearStart & localSample$DecYear<= yearEnd,]
  subDaily<-localDaily[localDaily$DecYear>=yearStart & localDaily$DecYear <= yearEnd,]
  
  xSample<-subSample$DecYear
  xDaily<-subDaily$DecYear
  
  Uncen<-subSample$Uncen
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed and Estimated Concentration versus Time") else ""
  
  ###################################
  
  yBottom <- 0 #Not specified within script, added under assumption that it's always zero based on ylim definition in this function
  
  xInfo <- generalAxis(x=xSample, minVal=yearStart, maxVal=yearEnd, tinyPlot=tinyPlot,padPercent=0,prettyDate=prettyDate)  
  
  if(!randomCensored){
    
    yLow<-subSample$ConcLow
    yHigh<-subSample$ConcHigh
    
    yCombined <- c(yHigh,subDaily$ConcDay)
    yInfo <- generalAxis(x = yCombined, minVal = yBottom, maxVal = concMax, 
                         tinyPlot = tinyPlot, padPercent = 5,units=localINFO$param.units)
    
    genericEGRETDotPlot(x=xSample, y=yHigh, xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        ylab=yInfo$label,plotTitle=plotTitle,cex.axis=cex.axis,col=col,lwd=lwd,cex=cex,
                        cex.main=cex.main, tinyPlot=tinyPlot,customPar=customPar, xDate=TRUE,...
    )
    
    censoredSegments(yInfo$bottom,yLow=yLow,yHigh=yHigh,x=xSample,Uncen=Uncen,col=col,lwd=lwd)
  } else {
    if(!("rObserved" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
      subSample<-localSample[localSample$DecYear>=yearStart & localSample$DecYear<= yearEnd,]
    }
    
    yHigh <- subSample$rObserved
    
    yCombined <- c(yHigh,subDaily$ConcDay)
    yInfo <- generalAxis(x = yCombined, minVal = yBottom, maxVal = concMax, 
                         tinyPlot = tinyPlot, padPercent = 5,units=localINFO$param.units)
    genericEGRETDotPlot(x=xSample[subSample$Uncen == 1], y=yHigh[subSample$Uncen == 1], xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        ylab=yInfo$label,plotTitle=plotTitle,cex.axis=cex.axis,col=col,lwd=lwd,cex=cex,
                        cex.main=cex.main, tinyPlot=tinyPlot,customPar=customPar, xDate=TRUE,...
    )
    points(x=xSample[Uncen == 0], y=yHigh[Uncen == 0], pch=1,cex=cex,col=col)
    
  }
  
  lines(x=xDaily, y=subDaily$ConcDay, type="l",col=col,lwd=lwd)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  
}
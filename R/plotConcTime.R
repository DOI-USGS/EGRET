#' Plot of Observed Concentration versus Time 
#'
#' @description
#' This function allows the user to plot all of the data, but also to limit it in two ways. 
#'   The data can be limited to only those observed concentrations collected in a specified discharge range. 
#'   The data can also be limited to only those observed in certain months of the year. 
#'     These two selection criteria can be combined. For example, 
#'     we may only want to plot data for discharges between 100 and 500 cubic feet per second in the months of March, April and May.
#'     
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata.  
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param qLower numeric the lower bound on values of discharge used to select the data points to be plotted, units are those specified by qUnit, default = NA which is equivalent to a lower bound of zero but if the desired lower bound is zero use qLower = NA
#' @param qUpper numeric the upper bound on values of discharge for selection of data points to be plotted, units are those specified by qUnit, default = NA which is equivalent to an upper bound of infinity
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param concMax numeric value for the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param concMin numeric value for lower limit on concentration shown on the vertical log graph, default is NA 
#' (which causes the lower limit to be set automatically, based on the data). This value is ignored for linear scales, using 0 as the minimum value for the concentration axis.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure).
#' @param logScale logical, default FALSE, FALSE creates a linear scale y-axis, TRUE creates a y-axis is in log scale.
#' @param cex numerical value giving the amount by which plotting symbols should be magnified.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex.
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function.
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width.
#' @param randomCensored logical. Show censored values as randomized.
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters.
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotConcTime(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcTime(eList, qUnit = 1, qLower = 100, qUpper = 10000)
#' plotConcTime(eList, logScale=TRUE)
#' plotConcTime(eList, qUnit = 1, qLower = 100, qUpper = 10000, randomCensored = TRUE)
plotConcTime<-function(eList, qUnit = 2, yearStart = NA, yearEnd = NA,
                       qLower = NA, qUpper = NA, randomCensored=FALSE,
                       tinyPlot = FALSE, concMax = NA, concMin = NA, printTitle = TRUE,logScale=FALSE, 
                       cex=0.8, cex.axis=1.1,cex.main=1.1, customPar=FALSE,col="black",lwd=1,...){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################  
  
  qFactor<-qUnit@qUnitFactor
  subSample<-localSample
  subSample$Q<-subSample$Q*qFactor
  qMin<-min(subSample$Q, na.rm = TRUE)
  qMax<-max(subSample$Q, na.rm = TRUE)
  if (logScale=="y"){
    qScale <- 0.9
  } else {
    qScale <- 0.95
  }
  
  qLowerBound<-if(is.na(qLower)) qScale*qMin else qLower
  qUpperBound<-if(is.na(qUpper)) 1.05*qMax else qUpper
  # the next section of code just sets up the approach to creating the plot title
  codeLower<-if(is.na(qLower)) 0 else 1
  codeUpper<-if(is.na(qUpper)) 0 else 2
  codeSum<-codeLower+codeUpper+1
  qText<-rep("",4)
  qText[1]<-"Concentration versus Time"
  qText[2]<-paste("For Discharge >",qLower,qUnit@qUnitName)
  qText[3]<-paste("For Discharge <",qUpper,qUnit@qUnitName)
  qText[4]<-paste("For Discharge between",qLower,"and",qUpper,qUnit@qUnitName)
  title3<-qText[codeSum]

  # the next section of code sets up the seasonal part of the plot title
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  #########################################################
  if (logScale){
    minYLow <- concMin
    logVariable <- "y"
  } else {
    minYLow <- 0
    logVariable <- ""
  }  
  #########################################################
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n",title3,sep="") else ""
  
  if(!randomCensored){
    subSample<-subSample[subSample$Q>qLowerBound & subSample$Q<qUpperBound,]
    subSample <- if(paLong == 12) subSample else selectDays(subSample, paLong,paStart)
    Uncen<-subSample$Uncen
    x<-subSample$DecYear
    
    xInfo <- generalAxis(x=x, 
                         minVal = ifelse(is.na(yearStart),min(x, na.rm = TRUE),yearStart),
                         maxVal=ifelse(is.na(yearEnd),max(x, na.rm = TRUE),yearEnd), 
                         tinyPlot=tinyPlot)  
    
    yLow<-subSample$ConcLow
    yHigh<-subSample$ConcHigh
    
    yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, logScale=logScale, 
                         tinyPlot=tinyPlot,units=attr(eList, "param.units"))
    
    genericEGRETDotPlot(x=x, y=yHigh, 
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab="", ylab=yInfo$label,
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,cex=cex,
                        plotTitle=plotTitle, log=logVariable,
                        cex.axis=cex.axis,cex.main=cex.main,tinyPlot=tinyPlot,col=col,customPar=customPar, ...
    )
    censoredSegments(yBottom=yInfo$ticks[1],yLow=yLow,yHigh=yHigh,x=x,Uncen=Uncen,col=col,lwd=lwd)
  } else {
    if(!("rObserved" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
    }
    
    subSample<-localSample
    subSample$Q<-subSample$Q*qFactor
    
    subSample<-subSample[subSample$Q>qLowerBound & subSample$Q<qUpperBound,]
    subSample <- if(paLong == 12) subSample else selectDays(subSample, paLong,paStart)
    
    Uncen<-subSample$Uncen
    x<-subSample$DecYear

    xInfo <- generalAxis(x=x, 
                         minVal = ifelse(is.na(yearStart),min(x),yearStart),
                         maxVal=ifelse(is.na(yearEnd),max(x),yearEnd), 
                         tinyPlot=tinyPlot)
    
    yHigh <- subSample$rObserved
    yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, logScale=logScale, 
                         tinyPlot=tinyPlot,units=attr(eList, "param.units"))
    
    genericEGRETDotPlot(x=x[Uncen == 1], y=yHigh[Uncen == 1], 
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab="", ylab=yInfo$label,
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,cex=cex,
                        plotTitle=plotTitle, log=logVariable,
                        cex.axis=cex.axis,cex.main=cex.main,tinyPlot=tinyPlot,col=col,customPar=customPar, ...
    )
    points(x=x[Uncen == 0], y=yHigh[Uncen == 0], pch=1,cex=cex,col=col)
    
    
  }
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}

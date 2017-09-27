#' Plot up to three curves representing the concentration versus discharge relationship. Each curve is a different point in time.  
#'
#' @description
#' These plots are like a vertical slice of the estimated concentration surface that is seen in the plotContours function.  
#' These plots show how the concentration-discharge relationship is changing over time. 
#' Typically the time points selected would be in three years at the same time of year spaced out over the period of record.  But that is not necessary.  
#' Another possibility is to use this to explore seasonal differences.  In this case the three
#' dates would be in the same year but different times during the year.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param date1 character specifying the date for the first curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes) 
#' @param date2 character specifying the date for the second curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If only one curve is wanted this should be NA
#' @param date3 character specifying the date for the third curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If a third curve is not wanted this should be NA
#' @param qLow numeric value for the lowest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qHigh numeric value for the highest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend in the units of the plot.
#' @param legendTop numeric which represents the top edge of the legend in the units of the plot.
#' @param printLegend logicalif TRUE, legend is included
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param concMin numeric value for lower limit on concentration shown on the vertical log graph, default is NA 
#' (which causes the lower limit to be set automatically, based on the data). This value is ignored for linear scales, using 0 as the minimum value for the concentration axis.
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are also printed to the console and returned in a dataframe (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param lwd number line width
#' @param cex.legend magnification to be used for legend annotation relative to the current setting of cex
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param logScale logical whether or not to use a log scale in the y axis.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param colors color vector of lines on plot, see ?par 'Color Specification'. Defaults to c("black","red","green")
#' @param lineVal vector of line types. Defaults to c(1,1,1) which is a solid line for each line. Options: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords water-quality statistics graphics
#' @export
#' @seealso \code{\link{genericEGRETDotPlot}}, \code{\link{runSurvReg}}
#' @examples 
#' date1<-"2001-06-01"
#' date2<-"2005-06-01"
#' date3<-"2010-06-01"
#' qLow<-1
#' qHigh<-100
#' eList <- Choptank_eList
#' plotConcQSmooth(eList, date1,date2,date3,qLow,qHigh)
#' plotConcQSmooth(eList, date1,date2,date3,qLow,qHigh,logScale=TRUE)
plotConcQSmooth<-function(eList, date1,date2,date3,qLow,qHigh,qUnit = 2, legendLeft = 0,legendTop = 0, 
                          concMax = NA, concMin=NA, bw = FALSE, printTitle = TRUE, printValues = FALSE, 
                          minNumObs = 100, minNumUncen =  50,
                          colors=c("black","red","green"),printLegend=TRUE,
                          windowY = 7, windowQ = 2, windowS = 0.5,tinyPlot=FALSE, customPar=FALSE,
                          lwd=2,cex=0.8, cex.axis=1.1,cex.main=1.1, cex.legend=1.2,lineVal=c(1,1,1),logScale=FALSE,
                          edgeAdjust=TRUE,...) {
  
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
  
  #########################################################
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  #############################################################

  numDates <- sum(!is.na(c(date1, date2, date3)))
  
  dates<-rep(as.POSIXlt(date1),3)
  dates[1]<-as.POSIXlt(date1)
  dates[2]<-as.POSIXlt(date2)
  dates[3]<-as.POSIXlt(date3)
  
  LogQLow<-log(qLow)
  LogQHigh<-log(qHigh)
  step<-(LogQHigh-LogQLow)/47
  x<-exp(seq(LogQLow,LogQHigh,step))
  qFactor<-qUnit@qUnitFactor
  LQ<-log(x/qFactor)
  # note the vector x is the set of 48 discharge values used to construct the curve, expressed in the selected units (such as cfs or 1000 cfs)
  # and the vector LQ is the same set of 48 discharge values but expressed in units of natural log of cubic meters per second
  y<-rep(NA,3*48)
  dim(y)<-c(3,48)
  
  day<-dates$yday + 1
  year<-dates$year + 1900
  decYear<-year+((day-0.5)/366)
  
  index <- which(!is.na(c(date1, date2, date3)))[1:numDates]
  
  for(iCurve in 1:numDates) {
    yrs<-rep(decYear[index[iCurve]],48)
    result<-runSurvReg(yrs,LQ,DecLow,DecHigh,localSample,
                       windowY = windowY, windowQ = windowQ, 
                       windowS = windowS, minNumObs=minNumObs, 
                       minNumUncen = minNumUncen,verbose=FALSE,
                       edgeAdjust=edgeAdjust,run.parallel = FALSE)
    y[index[iCurve],]<-result[,3]
  }
  
  title<-if(printTitle) paste (localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated Concentration Versus Discharge Relationship\nat",numDates,"specific dates") else ""
  
  colorVal<-if(bw) c("black","black","black") else colors
  lineVal<-if(bw) c(1,2,3) else lineVal

  #####################
  
  if(tinyPlot){
    xLab=qUnit@qUnitTiny  
  } else {
    xLab=qUnit@qUnitExpress  
  }
  
  if(logScale){
    logText <- "xy"
  } else {
    logText <- "x"
    concMin <- 0
  }
  
  xInfo <- generalAxis(x, maxVal=qHigh, minVal=qLow, logScale=TRUE, tinyPlot=tinyPlot)
  combinedY <- c(y[1,], y[2,],y[3,])
  yInfo <- generalAxis(combinedY, maxVal=concMax, minVal=concMin, logScale=logScale, 
                       tinyPlot=tinyPlot, units=localINFO$param.units)
  
  genericEGRETDotPlot(x=x, y=y[1,],
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$xLeft,xInfo$xRight),ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yInfo$label, plotTitle=title,
                      type="l",lwd=lwd,col=colorVal[1],lty=lineVal[1], 
                      cex=cex,cex.axis=cex.axis,cex.main=cex.main, 
                      log=logText,tinyPlot=tinyPlot,customPar=customPar,...
    )
  
  lines(x=x, y=y[2,],col=colorVal[2],lty=lineVal[2],lwd=lwd)
  lines(x=x, y=y[3,],col=colorVal[3],lty=lineVal[3],lwd=lwd)

#   legendLeft<-if(legendLeft==0) qLow*2 else legendLeft
#   legendTop<-if(legendTop==0) 0.3*yTop else legendTop 

  words<-as.character(dates[index])
  ltys<-lineVal[index]
  cols<-colorVal[index]
  
  legendLeft <- if(legendLeft == 0) {
    grconvertX(0.05, from="npc", to="user")
  } else {
    legendLeft
  }
  
  legendTop <- if(legendTop == 0) {
    grconvertY(0.3, from="npc", to="user") 
  } else {
    legendTop
  }
  
  if (printLegend) legend(legendLeft,legendTop ,legend=words,lty=ltys,col=cols,lwd=lwd,cex=cex.legend)
  
  printResults<-rep(NA,48*4)
  dim(printResults)<-c(48,4)
  for(j in 1:48) {printResults[j,1]<-format(x[j],width=9)
                  printResults[j,2:4]<-format(y[1:3,j],width=10)}
  topLine<-c("discharge",as.character(dates[1:numDates]))
  
  if(printValues) {    
#     write(topLine,file="",ncolumns=4)
#     write.table(printResults,file="",quote=FALSE,row.names=FALSE,col.names=FALSE)  
    cat("\n")
    returnDF <- data.frame(discharge=x, date1=y[1,], date2=y[2,], date3=y[3,])
    colnames(returnDF) <- c("discharge",date1,date2,date3)
    return(returnDF)
  }
}
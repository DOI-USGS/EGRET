#' Sample data plot: log of concentration vs. time
#'
#' Concentration data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO. 
#' This function allows the user to plot all of the data, but also to limit it in two ways. 
#'   The data can be limited to only those observed concentrations collected in a specified discharge range. 
#'   The data can also be limited to only those observed in certain months of the year. 
#'     These two selection criteria can be combined, for example, 
#'     we may only want to plot data for discharges between 100 and 500 cubic feet per second in the months of March, April and May. 
#' There is also a version of this using an arithmetic scale for concentration, plotConcTime. 
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric representing the short code, or character representing the descriptive name.
#' @param qLower numeric the lower bound on values of discharge used to select the data points to be plotted, units are those specified by qUnit, default = NA which is equivalent to a lower bound of zero but if the desired lower bound is zero use qLower = NA
#' @param qUpper numeric the upper bound on values of discharge for selection of data points to be plotted, units are those specified by qUnit, default = NA which is equivalent to an upper bound of infinity
#' @param paLong numeric, this is the length of the portion of the year from which data should be included in the plot, paLong must be an integer between 1 and 12.  The default is 12, which prints data from all months.
#' @param paStart numeric, this is the starting month of the portion of the year from which data should be included in the plot, paStart must be an integer between 1 and 12.  The default is 10, which corresponds to the water year, which starts in October.  If paLong = 12 then the choice of paStart is of no consequence.  All months will be included.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param concMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param concMin numeric specifying the minimum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{plotConcTime}}
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogConcTime(qUnit = 1)
#' plotLogConcTime(qUnit = 'thousandCfs')
plotLogConcTime<-function(localSample = Sample, localINFO = INFO, qUnit = 2,qLower = NA,qUpper = NA, paLong = 12, paStart = 10, tinyPlot = FALSE, concMax = NA, concMin = NA, printTitle = TRUE){
  # this function shows the sample data,
  # time on x-axis, concentration on y-axis 
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  
  qFactor<-qUnit@qUnitFactor
  if(tinyPlot)
    par(mar = c(5,4,1,1.5))
  else par(mar = c(5,4,4,2) + 0.1)
  subSample<-localSample
  subSample$Q<-subSample$Q*qFactor
  qMin<-min(subSample$Q)
  qMax<-max(subSample$Q)
  qLowerBound<-if(is.na(qLower)) 0.90*qMin else qLower
  qUpperBound<-if(is.na(qUpper)) 1.05*qMax else qUpper
  # the next section of code just sets up the approach to creating the plot title
  codeLower<-if(is.na(qLower)) 0 else 1
  codeUpper<-if(is.na(qUpper)) 0 else 2
  codeSum<-codeLower+codeUpper+1
  qText<-rep("",4)
  qText[1]<-"\n"
  qText[2]<-paste("\nFor Discharge >",qLower,qUnit@qUnitName)
  qText[3]<-paste("\nFor Discharge <",qUpper,qUnit@qUnitName)
  qText[4]<-paste("\nFor Discharge between",qLower,"and",qUpper,qUnit@qUnitName)
  title3<-qText[codeSum]
  subSample<-subset(subSample,Q>qLowerBound)
  subSample<-subset(subSample,Q<qUpperBound)
  # the next section subsets the data for the selected season
  goodMonth<-seq(paStart,paStart+paLong-1,1)
  goodMonth<-ifelse(goodMonth>12,goodMonth-12,goodMonth)
  numDays<-length(subSample$Month)
  isGood<-rep(FALSE,numDays)
  for(i in 1:numDays){
    count<-ifelse(subSample$Month[i]==goodMonth,1,0)
    isGood[i]<-if(sum(count)>0) TRUE else FALSE
  }
  subSample<-data.frame(subSample,isGood)
  subSample<-subset(subSample,isGood)
  # the next section of code sets up the seasonal part of the plot title
  title2<-if(paLong==12) "\n" else paste("\n",setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong))  
  yLow<-subSample$ConcLow
  yHigh<-subSample$ConcHigh
  Uncen<-subSample$Uncen
  numSamples<-length(subSample$Q)
  x<-subSample$DecYear
  xMin<-x[1]
  xMax<-x[numSamples]
  yearSpan<-c(xMin,xMax)
  nXTicks<-if(tinyPlot) 5 else 8 
  xTicks<-pretty(yearSpan,n=nXTicks)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  maxYHigh<-if(is.na(concMax)) 1.05*max(yHigh) else concMax
  minYLow<-if(is.na(concMin)) 0.95*min(subSample$ConcAve) else concMin
  yTicks<-logPretty3(minYLow,maxYHigh)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  plotTitle<-if(printTitle) paste(localINFO$shortName,",",localINFO$paramShortName,title2,title3) else ""
  plot(x,log(yHigh,10),axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(log(yBottom,10),log(yTop,10)),yaxs="i",ylab="Concentration in mg/L",main=plotTitle,pch=20,cex=0.5,cex.main=1.0,font.main=2,cex.lab=1.2)
  axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  axis(2,tcl=0.5,las=1,at=log(yTicks,10),labels=yTicks)
  axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  axis(4,tcl=0.5,at=log(yTicks,10),labels=FALSE)
  box()
  yLowVal<-ifelse(is.na(yLow),yBottom,yLow)
  numSamples<-length(x)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(x[uncensoredIndex],log(yLowVal[uncensoredIndex],10),x[uncensoredIndex],log(yHigh[uncensoredIndex],10))
  par(mar = c(5, 4, 4, 2) + 0.1)
}
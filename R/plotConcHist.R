#' Graph of annual concentration and flow normalized concentration versus year
#'
#' Data come from a data frame named AnnualResults. 
#' The metadata come from a data frame named INFO.
#' The annual concentrations are "time-weighted" mean concentrations (as opposed to "flow-weighted"). 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month.  
#' User specifies this period of analysis in the call to setupYears.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param localAnnualResults string specifying the name of the data frame that contains the annual results, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE flow normalized line is plotted, if FALSE not plotted 
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' INFO <- exINFO
#' AnnualResults <- exAnnualResults
#' plotConcHist(yearStart, yearEnd)
plotConcHist<-function(yearStart = NA, yearEnd = NA, localAnnualResults = AnnualResults, localINFO = INFO, concMax = NA, printTitle = TRUE, plotFlowNorm = TRUE){
  # produces a graph of annual mean concentration and flow normalized concentration versus year
  # AnnualResults contains the set of results
  # typically yearStart and yearEnd should be integers, 
  # yearStart is the start of the calendar year of the first estimated annual value
  # yearEnd is the start of the calendar year after the last estimated annual value
  # if you want to specify the maximum value, you can do so with the argument concMax, otherwise it will be automatic
  par(oma=c(3,0,3,0))
  par(mar=c(5,6,5,2))
  numYears <- length(localAnnualResults$DecYear)
  yearStart <- if(is.na(yearStart)) trunc(localAnnualResults$DecYear[1]) else yearStart
  yearEnd <- if(is.na(yearEnd)) trunc(localAnnualResults$DecYear[numYears])+1 else yearEnd 
  subAnnualResults<-subset(localAnnualResults,DecYear>=yearStart)
  subAnnualResults<-subset(subAnnualResults,DecYear<=yearEnd)
  annConc<-subAnnualResults$Conc
  fnConc<-subAnnualResults$FNConc
  concMax<-if(is.na(concMax)) 1.05*max(annConc,na.rm=TRUE) else concMax
  xVals<-subAnnualResults$DecYear
  xMin<-yearStart
  xMax<-yearEnd
  yearSpan<-c(xMin,xMax)
  xTicks<-pretty(yearSpan,n=9)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTicks<-yPretty(concMax)
  yTop<-yTicks[length(yTicks)]
  periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
  title3<-if(plotFlowNorm) "\nMean Concentration (dots) & Flow Normalized Concentration (line)" else "\nAnnual Mean Concentration"
  title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""
  plot(subAnnualResults$DecYear,annConc,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="Concentration in mg/L",main=title,pch=20,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2)
  if(plotFlowNorm) par(new=TRUE)
  if(plotFlowNorm) plot(subAnnualResults$DecYear,fnConc,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="",main="",type="l",col="green",lwd=3,cex=0.8,cex.main=1.0,cex.lab=1.2,font=2)
  axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  axis(2,tcl=0.5,las=1,at=yTicks,cex.axis=1.1)
  axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  box()
  par(oma=c(0,0,0,0))
  par(mar=c(5,4,4,2)+0.1)	
}
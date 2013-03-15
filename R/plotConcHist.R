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
#' @param cex number
#' @param cex.axis number
#' @param cex.main number
#' @param lwd number
#' @param ... arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' INFO <- exINFO
#' AnnualResults <- exAnnualResults
#' plotConcHist(yearStart, yearEnd)
plotConcHist<-function(yearStart = NA, yearEnd = NA, localAnnualResults = AnnualResults, 
        localINFO = INFO, concMax = NA, printTitle = TRUE, plotFlowNorm = TRUE,
        cex=0.8, cex.axis=1.1,cex.main=1.1, lwd=2, ...){
  # produces a graph of annual mean concentration and flow normalized concentration versus year
  # AnnualResults contains the set of results
  # typically yearStart and yearEnd should be integers, 
  # yearStart is the start of the calendar year of the first estimated annual value
  # yearEnd is the start of the calendar year after the last estimated annual value
  # if you want to specify the maximum value, you can do so with the argument concMax, otherwise it will be automatic
#   par(oma=c(3,0,3,0))
#   par(mar=c(5,6,5,2))
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
  
  ##################
  par(mar = c(5,6,5,2))
  genericEGRETDotPlot(x=subAnnualResults$DecYear, y=annConc,
                      xTicks=xTicks, yTicks=yTicks,
                      xlim=c(xLeft,xRight), ylim=c(0,yTop),
                      ylab="Concentration in mg/L", 
                      plotTitle=title, cex.axis=cex.axis,cex.main=cex.main,...
    )
  
  if(plotFlowNorm) lines(subAnnualResults$DecYear, fnConc, col="green", lwd=lwd)

#   par(oma=c(0,0,0,0))
  par(mar=c(5,4,4,2)+0.1)	
}
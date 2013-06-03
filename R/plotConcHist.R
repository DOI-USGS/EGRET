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
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
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
        cex=0.8, cex.axis=1.1,cex.main=1.1, lwd=2,...){
  # produces a graph of annual mean concentration and flow normalized concentration versus year
  # AnnualResults contains the set of results
  # typically yearStart and yearEnd should be integers, 
  # yearStart is the start of the calendar year of the first estimated annual value
  # yearEnd is the start of the calendar year after the last estimated annual value
  # if you want to specify the maximum value, you can do so with the argument concMax, otherwise it will be automatic
#   par(oma=c(3,0,3,0))
#   par(mar=c(5,6,5,2))
  
  originalPar <-  par(no.readonly = TRUE)
  periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
  title3<-if(plotFlowNorm) "\nMean Concentration (dots) & Flow Normalized Concentration (line)" else "\nAnnual Mean Concentration"
  title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""
  
  ##################
  par(mar = c(5,6,5,2))
  
  xInfo <- generalAxis(x=localAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd, padPercent=0)
  
  yInfo <- generalAxis(x=localAnnualResults$Conc, minVal=0, maxVal=concMax, padPercent=5)
  
  genericEGRETDotPlot(x=localAnnualResults$DecYear, y=localAnnualResults$Conc,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      ylab="Concentration in mg/L", 
                      plotTitle=title, cex.axis=cex.axis,cex.main=cex.main,...
    )
  
  if(plotFlowNorm) with(localAnnualResults, 
                        lines(DecYear[DecYear>xInfo$bottom & DecYear<xInfo$top], 
                              FNConc[DecYear>xInfo$bottom & DecYear<xInfo$top], 
                              col="green", lwd=lwd))

#   par(oma=c(0,0,0,0))
  par(originalPar)	
}

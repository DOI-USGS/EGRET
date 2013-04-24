#' Graph of annual flux and flow normalized flux versus year
#'
#' Data come from a data frame named AnnualResults.  
#' The metadata come from a data frame named INFO. 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month. 
#' The user specifies this period of analysis in the call to setupYears.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param localAnnualResults string specifying the name of the data frame that contains the annual results, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{fluxConst}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE the flow normalized line is plotted, if FALSE not plotted 
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
#' AnnualResults <- exAnnualResults
#' INFO <- exINFO
#' plotFluxHist(yearStart, yearEnd, fluxUnit = 1)
#' plotFluxHist(yearStart, yearEnd, fluxUnit = 'kgDay')
plotFluxHist<-function(yearStart = NA, yearEnd = NA, fluxUnit = 9, 
    localAnnualResults = AnnualResults, localINFO = INFO, fluxMax = NA, 
    printTitle = TRUE, plotFlowNorm = TRUE,
    cex=0.8, cex.axis=1.1,cex.main=1.1, lwd=2, ...){
  # produces a graph of annual flux and flow normalized flux versus year
  # AnnualResults contains the set of results
  # typically yearStart and yearEnd should be integers, 
  # yearStart is the start of the calendar year of the first estimated annual value
  # yearEnd is the start of the calendar year after the last estimated annual value
  # if you want to specify the maximum value, you can do so with the argument fluxMax, otherwise it will be automatic
  # fluxUnit is the units you want the results displayed in, see manual for list of all possible units  

  #   par(oma=c(3,0,3,0))
#   par(mar=c(5,6,5,2))
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################
  
  unitFactorReturn <- fluxUnit@unitFactor
  ylabel <- fluxUnit@unitName
  numYears <- length(localAnnualResults$DecYear)
  yearStart <- if(is.na(yearStart)) trunc(localAnnualResults$DecYear[1]) else yearStart
  yearEnd <- if(is.na(yearEnd)) trunc(localAnnualResults$DecYear[numYears])+1 else yearEnd
  subAnnualResults<-subset(localAnnualResults,DecYear>=yearStart)
  subAnnualResults<-subset(subAnnualResults,DecYear<=yearEnd)
  annFlux<-unitFactorReturn*subAnnualResults$Flux
  fnFlux<-unitFactorReturn*subAnnualResults$FNFlux
  fluxMax<-if(is.na(fluxMax)) 1.05*max(annFlux,na.rm=TRUE) else fluxMax
  #xVals<-subAnnualResults$DecYear
  xMin<-yearStart
  xMax<-yearEnd
  #yearSpan<-c(xMin,xMax)
  #xTicks<-pretty(yearSpan,n=9)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  #yTicks<-yPretty(fluxMax)
  #yTop<-yTicks[length(yTicks)]
  periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
  title3<-if(plotFlowNorm) "\nFlux Estimates (dots) & Flow Normalized Flux (line)" else "\nAnnual Flux Estimates"
  title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""
  
  xInfo <- generalAxis(x=subAnnualResults$DecYear, min=xMin, max=xMax)
  
  yInfo <- generalAxis(x=annFlux, min=0, max=fluxMax)
  
  ###############################################
  par(mar = c(5,6,5,2))
  genericEGRETDotPlot(x=subAnnualResults$DecYear, y = annFlux,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(0,yInfo$top),
                      ylab=ylabel, plotTitle=title,
                      cex.axis=cex.axis,cex.main=cex.main, ...
                      
    )
  # Laura took out cex=0.8,cex.main=1.1, cex.axis=1.1

  if(plotFlowNorm) lines(subAnnualResults$DecYear, fnFlux, col="green", lwd=lwd)
    
  par(oma=c(0,0,0,0))
  par(mar=c(5,4,4,2)+0.1)   
}
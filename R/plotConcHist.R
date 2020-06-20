#' Graph of annual concentration and flow normalized concentration versus year
#'
#' @description
#' Data come from named list, which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#' 
#' The annual concentrations are "time-weighted" mean concentrations (as opposed to "flow-weighted"). 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month.  
#' User specifies this period of analysis in the call to \code{setupYears}.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param eList named list with at least the Daily and INFO dataframes
#' @param DailyK data frame returned from \code{makeDailyK}
#' @param concMax numeric. Maximum value of concentration to be plotted.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE flow normalized line is plotted, if FALSE not plotted 
#' @param plotAnnual logical variable if TRUE annual concentration points are plotted, if FALSE not plotted 
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param lwd number magnification of line width.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param col.pred color of flow normalized line on plot, see ?par 'Color Specification'
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS complience. It will only change automatically
#' generated labels
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' eList <- Choptank_eList
#' 
#' # Water year:
#' plotConcHist(eList, yearStart, yearEnd, tinyPlot = TRUE)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcHist(eList, tinyPlot = TRUE)
#' 
plotConcHist<-function(eList, yearStart = NA, yearEnd = NA, 
                       DailyK = NA,
                       concMax = NA, printTitle = TRUE, 
                       tinyPlot = FALSE,usgsStyle = FALSE,
                       plotFlowNorm = TRUE, plotAnnual = TRUE,
                        cex=0.8, cex.axis=1.1,cex.main=1.1, 
                       lwd=2, col="black", col.pred="green", customPar=FALSE,...){

  localINFO <- getInfo(eList)

  if(all(is.na(DailyK))){
    localDaily <- getDaily(eList)
  } else {
    localDaily <- DailyK
    localDaily$ConcDay <- DailyK$GenConc
  }
  
  if(all(c("paStart","paLong") %in% names(localINFO))){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  if(!(c("FNConc") %in% names(eList$Daily))){
    stop("This function requires running modelEstimation on eList")
  }

  localAnnualResults <- setupYears(paStart=paStart,paLong=paLong, localDaily = localDaily)
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  
  periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
  
  if(hasFlex){
    periodName <- paste(periodName,"*")
  }
  
  if(plotAnnual & plotFlowNorm){
    title3 <- "\nMean Concentration (dots) & Flow Normalized Concentration (line)" 
  } else if(plotAnnual & !plotFlowNorm){
    title3 <- "\nAnnual Mean Concentration"
  } else if(!plotAnnual & plotFlowNorm){
    title3 <- "\nFlow Normalized Concentration"
  } else {
    title3 <- "\n"
  }
  
  title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""

  ##################

  if(is.na(yearStart)){
    yearStart <- min(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], na.rm = TRUE)
  }

  if(is.na(yearEnd)){
    yearEnd <- max(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], na.rm = TRUE)
  }
  
  xInfo <- generalAxis(x=localAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd, padPercent=0, tinyPlot=tinyPlot)
 
  combinedY <- c(localAnnualResults$Conc,localAnnualResults$FNConc[localAnnualResults$DecYear>xInfo$bottom & localAnnualResults$DecYear<xInfo$top])
 
  yInfo <- generalAxis(x=combinedY, minVal=0, maxVal=concMax, padPercent=5, 
                       tinyPlot=tinyPlot,units=localINFO$param.units, usgsStyle = usgsStyle)
  
  genericEGRETDotPlot(x=NA, y=NA,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,xDate=TRUE,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      ylab=yInfo$label, col=col,cex=cex,
                      plotTitle=title, cex.axis=cex.axis,cex.main=cex.main,
                      tinyPlot=tinyPlot,customPar=customPar,...
    )
  
  if(plotAnnual){
    with(localAnnualResults, 
         points(DecYear[DecYear>xInfo$bottom & DecYear<xInfo$top], 
                Conc[DecYear>xInfo$bottom & DecYear<xInfo$top], 
               col=col, cex=cex, pch=20))
  }

  if(plotFlowNorm){

    with(localAnnualResults, 
                        lines(DecYear[DecYear>xInfo$bottom & DecYear<xInfo$top], 
                              FNConc[DecYear>xInfo$bottom & DecYear<xInfo$top], 
                              col=col.pred, lwd=lwd))
    
  }
	
}

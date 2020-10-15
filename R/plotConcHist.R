#' Graph of annual mean concentration and flow normalized concentration versus year
#'
#' @description
#' Data come from named list, which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#' 
#' The annual mean concentrations are "time-weighted" mean concentrations (as opposed to "flow-weighted"). 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month.  
#' User specifies this period of analysis in the call to \code{setupYears}.
#'
#' Three versions of annual mean concentration can be plotted
#'  "Annual" version is the mean concentration computed directly from the WRTDS model
#'  "GenConc" version uses the WRTDS_K calculation, that uses an auto-regressive formulation to improve the accuracy of the mean concentration.  It has been shown to be more accurate than the "Annual" version.
#'  "FlowNormalized" version eliminates the interannual variability by integrating the WRTDS model results over the full probability distribution of discharge
#'  See introduction to the EGRET vignette for more details on these three options.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param eList named list with at least the Daily and INFO dataframes
#' @param concMax numeric. Maximum value of concentration to be plotted.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE flow normalized line is plotted, if FALSE not plotted 
#' @param plotAnnual logical variable if \code{TRUE}, annual concentration points from WRTDS output are plotted, if \code{FALSE} not plotted 
#' @param plotGenConc logical variable. If \code{TRUE}, annual concentration points from WRTDS_K output are plotted, if \code{FALSE} not plotted 
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param lwd number magnification of line width, default = 2.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification', default = "black"
#' @param col.pred color of flow normalized line on plot, see ?par 'Color Specification', default = "green"
#' @param col.gen color of points for WRTDS_K output on plot, see ?par 'Color Specification', default = "red"
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @details
#'  running modelEstimation is required before running this function.
#'  if genConc is plotted then running WRTDSKalman is also required.
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
                       concMax = NA, 
                       printTitle = TRUE, 
                       tinyPlot = FALSE, usgsStyle = FALSE,
                       plotFlowNorm = TRUE, plotAnnual = TRUE, plotGenConc = FALSE,
                       cex = 0.8, cex.axis = 1.1, cex.main = 1.1, lwd = 2, 
                       col = "black", col.pred = "green", col.gen = "red", customPar = FALSE, ...){

  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  
  if(!(c("FNConc") %in% names(eList$Daily))){
    stop("This function requires running modelEstimation on eList")
  }
  
  if(plotGenConc){
    if(!all((c("GenFlux","GenConc") %in% names(eList$Daily)))){
      stop("This option requires running WRTDSKalman on eList")
    }

  } 
  
  if(all(c("paStart","paLong") %in% names(localINFO))){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localAnnualResults <- setupYears(paStart = paStart,
                                   paLong = paLong,
                                   localDaily = localDaily)
  
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  
  periodName <- setSeasonLabel(localAnnualResults=localAnnualResults)
  
  if(hasFlex){
    periodName <- paste(periodName,"*")
  }
  
  if(plotGenConc){
    conc_words <- "Mean Concentration (K)"
  } else {
    conc_words <- "Mean Concentration"
  }
  
  if((plotAnnual | plotGenConc) & plotFlowNorm){
    # Need to think what we want to say if there are both
    title3 <- paste("\n", conc_words, "(dots) & Flow Normalized Concentration (line)" )
  } else if((plotAnnual | plotGenConc) & !plotFlowNorm){
    title3 <- paste("\nAnnual", conc_words)
  } else if(!(plotAnnual | plotGenConc) & plotFlowNorm){
    title3 <- "\nFlow Normalized Concentration"
  } else {
    title3 <- "\n"
  }
  
  title <- if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",
                                periodName,
                                title3) else ""

  ##################

  if(is.na(yearStart)){
    yearStart <- min(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], na.rm = TRUE)
  }

  if(is.na(yearEnd)){
    yearEnd <- max(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], na.rm = TRUE)
  }
  
  xInfo <- generalAxis(x=localAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd, padPercent=0, tinyPlot=tinyPlot)
 
  combinedY <- c(localAnnualResults$FNConc[localAnnualResults$DecYear > xInfo$bottom &
                                             localAnnualResults$DecYear < xInfo$top])
 
  if(plotAnnual){
    combinedY <- c(combinedY, localAnnualResults$Conc)
  }
  
  if(plotGenConc){
    combinedY <- c(combinedY, localAnnualResults$GenConc)
  }
  
  yInfo <- generalAxis(x = combinedY, 
                       minVal = 0, maxVal = concMax, 
                       tinyPlot=tinyPlot,
                       units=localINFO$param.units,
                       usgsStyle = usgsStyle)
  
  genericEGRETDotPlot(x = NA, y = NA,
                      xTicks = xInfo$ticks, yTicks = yInfo$ticks, xDate = TRUE,
                      xlim = c(xInfo$bottom,xInfo$top), ylim = c(yInfo$bottom,yInfo$top),
                      ylab = yInfo$label, col = col, cex = cex,
                      plotTitle = title, cex.axis = cex.axis, cex.main = cex.main,
                      tinyPlot = tinyPlot, customPar = customPar, ...
    )
  
  if(plotAnnual){
    with(localAnnualResults, 
         points(DecYear[DecYear > xInfo$bottom & DecYear < xInfo$top], 
                Conc[DecYear > xInfo$bottom & DecYear < xInfo$top], 
               col = col, cex = cex, pch = 20))
  }

  if(plotGenConc){
    with(localAnnualResults, 
         points(DecYear[DecYear > xInfo$bottom & DecYear < xInfo$top], 
                GenConc[DecYear > xInfo$bottom & DecYear < xInfo$top], 
                col = col.gen, cex = cex, pch = 20))
  }
  
  if(plotFlowNorm){

    with(localAnnualResults, 
                        lines(DecYear[DecYear > xInfo$bottom & DecYear < xInfo$top], 
                              FNConc[DecYear > xInfo$bottom & DecYear < xInfo$top], 
                              col = col.pred, lwd = lwd))
    
  }
	
}

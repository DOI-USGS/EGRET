#' Graph of annual concentration and flow normalized concentration versus year
#'
#' @description
#' Data come from named list (eList), which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#' 
#' The annual concentrations are "time-weighted" mean concentrations (as opposed to "flow-weighted"). 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month.  
#' User specifies this period of analysis in the call to \code{setupYears}.
#' 
#' User can specify plotting of three possible series.  All are in units of mg/L.
#'   Annual mean concentration 
#'   WRTDS_K version of annual mean concentration (requires that WRTDSKalman has been run)
#'   Flow normalized mean concentration
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param eList named list with at least the Daily and INFO dataframes
#' @param concMax numeric. Maximum value of concentration to be plotted.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is
#' not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE flow normalized line is plotted, if FALSE not plotted 
#' @param plotAnnual logical variable if \code{TRUE}, annual concentration points
#' from WRTDS output are plotted, if \code{FALSE} not plotted 
#' @param plotGenConc logical variable. If \code{TRUE}, annual concentration points
#' from \code{WRTDSKalman} output are plotted, if \code{FALSE} not plotted 
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param lwd number magnification of line width.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param col.pred color of flow normalized line on plot, see ?par 'Color Specification'
#' @param col.gen color of points for WRTDS_K output on plot, see ?par 'Color Specification'
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels
#' @param concLab object of concUnit class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' eList <- Choptank_eList
#' 
#' plotConcHist(eList, yearStart, yearEnd)
plotConcHist <- function(eList,
                         yearStart = NA, 
                         yearEnd = NA, 
                         concMax = NA, 
                         printTitle = TRUE, 
                         tinyPlot = FALSE,
                         usgsStyle = FALSE,
                         plotFlowNorm = TRUE,
                         plotAnnual = TRUE,
                         plotGenConc = FALSE,
                         cex = 0.8, cex.axis = 1.1, 
                         cex.main = 1.1, lwd = 2, 
                         col = "black", col.pred = "green",
                         concLab = 1,
                         col.gen = "red", customPar = FALSE, ...){
  
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
  
  waterYear <- paLong == 12 & paStart == 10
  
  localAnnualResults <- setupYears(paStart = paStart,
                                   paLong = paLong,
                                   localDaily = localDaily)
  
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  
  periodName <- setSeasonLabel(localAnnualResults=localAnnualResults)
  
  if(hasFlex){
    periodName <- paste(periodName,"*")
  }

  if (is.numeric(concLab)){
    concPrefix <- concConst[shortCode=concLab][[1]]    
  } else if (is.character(concLab)){
    concPrefix <- concConst[concLab][[1]]
  } else {
    concPrefix <- concLab
  }
  
  if(plotAnnual & plotGenConc & plotFlowNorm){  #all 3
    title3 <- "\nMean (red = Kalman, black = WRTDS) & Flow-Normalized (line)" 
  } else if (plotAnnual & plotGenConc & !plotFlowNorm){ # no flow-normalized
    title3 <- "\nMean (red = Kalman, black = WRTDS)"
  } else if (!plotAnnual & !plotGenConc & plotFlowNorm){ # only flow-normalized
    title3 <- "\nFlow Normalized Concentration"
  } else if (plotFlowNorm & (plotGenConc | plotAnnual)){ # flow normalized with 1
    title3 <- "\nMean (dots) & Flow-Normalized (line)"
  } else if (plotAnnual & !plotGenConc) {
    title3 <- "\nMean"
  } else if (!plotAnnual & plotGenConc) {
    title3 <- "\nMean Kalman"
  } else {
    title3 <- "\n"
  }
  
  if(title3 != "\n"){
    title3 <- paste(title3, concPrefix@longPrefix)
  }
  
  if(printTitle) {
    title <-  paste(localINFO$shortName," ",localINFO$paramShortName,"\n",
                                periodName,
                                title3)
  } else {
    title <- ""
  }
  
  ##################
  dataStart <- min(eList$Sample$DecYear, na.rm = TRUE)
  dataStartPad <- dataStart - 0.5
  
  if(is.na(yearStart)){
    yearStart <- dataStartPad
  } else {
    yearStart <- max(yearStart, dataStartPad)
    
  }
  
  dataEnd <- max(eList$Sample$DecYear, na.rm = TRUE)
  dataEndPad <- dataEnd + 0.5
  
  if(is.na(yearEnd)){
    yearEnd <- dataEndPad
  } else {
    yearEnd <- min(yearEnd, dataEndPad)
  }
  
  localAnnualResults <- localAnnualResults[localAnnualResults$DecYear >= yearStart &
                                             localAnnualResults$DecYear <= yearEnd, ]
  
  xInfo <- generalAxis(x = localAnnualResults$DecYear,
                       minVal = yearStart,
                       maxVal = yearEnd, 
                       padPercent = 0.05,
                       tinyPlot = tinyPlot, 
                       concentration = FALSE)
  
  combinedY <- c(localAnnualResults$FNConc[localAnnualResults$DecYear > xInfo$bottom &
                                             localAnnualResults$DecYear < xInfo$top])
  
  if(plotAnnual){
    combinedY <- c(combinedY, localAnnualResults$Conc)
  }
  
  if(plotGenConc){
    combinedY <- c(combinedY, localAnnualResults$GenConc)
  }
  
  yInfo <- generalAxis(x = combinedY, 
                       minVal = 0, 
                       maxVal = concMax, 
                       tinyPlot = tinyPlot,
                       units = localINFO$param.units,
                       usgsStyle = usgsStyle, 
                       concLab = concLab)
  
  genericEGRETDotPlot(x = NA, y = NA,
                      xTicks = xInfo$ticks, yTicks = yInfo$ticks, xDate = TRUE,
                      xlim = c(xInfo$bottom,xInfo$top), ylim = c(yInfo$bottom,yInfo$top),
                      ylab = yInfo$label, col = col, cex = cex,
                      plotTitle = title, cex.axis = cex.axis, cex.main = cex.main,
                      tinyPlot = tinyPlot, customPar = customPar, ...
  )
  
  if(plotAnnual){
    with(localAnnualResults, 
         points(DecYear[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                Conc[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                col = col, cex = cex, pch = 20))
  }
  
  if(plotGenConc){
    with(localAnnualResults, 
         points(DecYear[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                GenConc[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                col = col.gen, cex = cex, pch = 20))
  }
  
  if(plotFlowNorm){
    
    with(localAnnualResults, 
         lines(DecYear[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
               FNConc[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
               col = col.pred, lwd = lwd))
    
  }
  
}

#' Graph of annual flux and flow normalized flux versus year
#'
#' @description
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month. 
#' The user specifies this period of analysis in the call to setupYears.
#' Values plotted express a flux rate, such as thousand kg per year
#' For a period of analysis that is less than a year, this does not equal the mass transported over the period of analysis
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' Data come from named list (eList), which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{printFluxUnitCheatSheet}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE the flow normalized line is plotted, if FALSE not plotted 
#' @param plotAnnual logical variable if TRUE annual flux points are plotted, if FALSE not plotted 
#' @param plotGenFlux logical variable. If \code{TRUE}, annual flux points from WRTDS_K output are plotted, if \code{FALSE} not plotted 
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col.pred color of flow normalized line on plot, see ?par 'Color Specification'
#' @param col.gen color of points for WRTDS_K output on plot, see ?par 'Color Specification'
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels. 
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' eList <- Choptank_eList
#' # Water year:
#' \donttest{
#' plotFluxHist(eList)
#' plotFluxHist(eList, yearStart, yearEnd, fluxUnit = 1)
#' plotFluxHist(eList, yearStart, yearEnd, fluxUnit = 'kgDay')
#' }
plotFluxHist<-function(eList, yearStart = NA, yearEnd = NA, 
                       fluxUnit = 9, fluxMax = NA, 
                       printTitle = TRUE, usgsStyle = FALSE,
                       plotFlowNorm = TRUE, plotAnnual = TRUE, plotGenFlux = FALSE,
                       tinyPlot=FALSE, 
                       col="black", col.pred="green", col.gen = "red",
                       cex=0.8, cex.axis=1.1, cex.main=1.1, 
                       lwd=2, customPar=FALSE, ...){
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  
  if(plotGenFlux){
    if(!all((c("GenFlux","GenConc") %in% names(eList$Daily)))){
      stop("This option requires running WRTDSKalman on eList")
    }
  } 
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  waterYear <- paLong == 12 & paStart == 10
  
  if(!(c("FNFlux") %in% names(eList$Daily))){
    stop("This function requires running modelEstimation on eList")
  }
  
  possibleGoodUnits <- c("mg/l","mg/l as N", "mg/l as NO2", 
                         "mg/l as NO3","mg/l as P","mg/l as PO3","mg/l as PO4","mg/l as CaCO3",
                         "mg/l as Na","mg/l as H","mg/l as S","mg/l NH4" )
  
  allCaps <- toupper(possibleGoodUnits)
  localUnits <- toupper(localINFO$param.units)
  
  if(!(localUnits %in% allCaps)){
    warning("Expected concentration units are mg/l, \nThe INFO dataframe indicates:",localINFO$param.units,
            "\nFlux calculations will be wrong if units are not consistent")
  }
  
  localAnnualResults <- setupYears(paStart = paStart,
                                   paLong = paLong,
                                   localDaily = localDaily)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################
  
  if (tinyPlot) {
    ylabel <- fluxUnit@unitExpressTiny
  } else {
    ylabel <- ifelse(usgsStyle,fluxUnit@unitUSGS,fluxUnit@unitExpress)
  }
  
  unitFactorReturn <- fluxUnit@unitFactor
  
  numYears <- length(localAnnualResults$DecYear)
  
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
  
  subAnnualResults <- localAnnualResults[localAnnualResults$DecYear>=yearStart & localAnnualResults$DecYear <= yearEnd,]
  
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  
  periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
  
  if(hasFlex){
    periodName <- paste(periodName,"*")
  }
  
  if(plotAnnual & plotGenFlux & plotFlowNorm){  #all 3
    title3 <- "\nMean (red = Kalman, black = WRTDS) & Flow-Normalized (line) Flux Estimates" 
  } else if (plotAnnual & plotGenFlux & !plotFlowNorm){ # no flow-normalized
    title3 <- "\nMean (red = Kalman, black = WRTDS) Flux Estimates"
  } else if (!plotAnnual & !plotGenFlux & plotFlowNorm){ # only flow-normalized
    title3 <- "\nFlow Normalized Flux Estimates"
  } else if (plotFlowNorm & (plotGenFlux | plotAnnual)){ # flow normalized with 1
    title3 <- "\nMean (dots) & Flow-Normalized (line) Flux Estimates"
  } else if (plotAnnual & !plotGenFlux) {
    title3 <- "\nMean Flux Estimates"
  } else if (!plotAnnual & plotGenFlux) {
    title3 <- "\nMean Kalman Flux Estimates"
  } else {
    title3 <- "\n"
  }
  
  title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""
  
  xInfo <- generalAxis(x=subAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd,padPercent=0, tinyPlot=tinyPlot)  
  
  annFlux <- unitFactorReturn*subAnnualResults$Flux
  
  fnFlux <- unitFactorReturn*subAnnualResults$FNFlux
  
  combinedY <- fnFlux
  
  if(plotAnnual){
    combinedY <- c(combinedY, annFlux)
  }
  
  if(plotGenFlux){
    combinedY <- c(combinedY, unitFactorReturn*subAnnualResults$GenFlux)
  }
  
  yInfo <- generalAxis(x = combinedY, 
                       minVal = 0, maxVal = fluxMax, 
                       tinyPlot = tinyPlot)
  
  ###############################################
  
  genericEGRETDotPlot(x=NA, y = NA,
                      xTicks = xInfo$ticks, yTicks = yInfo$ticks, xDate = TRUE,
                      xlim = c(xInfo$bottom, xInfo$top), ylim = c(0, yInfo$top), col = col,
                      ylab = ylabel, plotTitle = title, customPar = customPar, cex = cex,
                      cex.axis = cex.axis, cex.main = cex.main, tinyPlot = tinyPlot,...
                      
  )
  
  if(plotAnnual){
    with(subAnnualResults, 
         points(subAnnualResults$DecYear[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                annFlux[DecYear >= xInfo$bottom & DecYear <= xInfo$top], 
                col = col, cex = cex, pch = 20))
  }
  
  if(plotGenFlux){
    points(subAnnualResults$DecYear[subAnnualResults$DecYear >= xInfo$bottom & subAnnualResults$DecYear <= xInfo$top], 
           unitFactorReturn*subAnnualResults$GenFlux[subAnnualResults$DecYear >= xInfo$bottom & subAnnualResults$DecYear <= xInfo$top], 
           col = col.gen, cex = cex, pch = 20)
  }
  
  if(plotFlowNorm) {
    lines(subAnnualResults$DecYear[subAnnualResults$DecYear >= xInfo$bottom & subAnnualResults$DecYear <= xInfo$top], 
          fnFlux[subAnnualResults$DecYear >= xInfo$bottom & subAnnualResults$DecYear <= xInfo$top], col=col.pred, lwd=lwd)
  }
  
}
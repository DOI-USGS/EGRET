#' Sample data plot: observed log flux vs log discharge
#'
#' @description
#' Concentration and discharge data used to compute flux come from a data frame named Sample which contains the sample data.
#' The metadata come from a data frame named INFO.
#'
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param fluxUnit object of fluxUnit class. \code{\link{printFluxUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable if TRUE plot is designed to fit into a multi-plot array, default is FALSE
#' @param fluxMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param fluxMin numeric specifying the minimum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param logScale logical, default TRUE, TRUE creates a log-log scale, FALSE creates an arithmatic scale.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotFluxQ(eList, qUnit = 1, fluxUnit = 1)
#' plotFluxQ(eList, fluxUnit = 'kgDay')
#' plotFluxQ(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotFluxQ(eList)
plotFluxQ<-function(eList, qUnit = 2,logScale=TRUE,
                       fluxUnit = 3, tinyPlot = FALSE, fluxMax = NA, fluxMin = NA, col="black",lwd=1,
                       printTitle = TRUE,cex=0.8, cex.axis=1.1,cex.main=1.1, customPar=FALSE,...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale,
  # flux on y-axis on a log scale
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
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
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample,paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################
  
  
  qFactor<-qUnit@qUnitFactor
  fluxFactor<-fluxUnit@unitFactor*86.40
  x<-localSample$Q*qFactor
  yLow<-localSample$ConcLow*localSample$Q*fluxFactor
  yHigh<-localSample$ConcHigh*localSample$Q*fluxFactor
  Uncen<-localSample$Uncen
  
  if (tinyPlot) {
    xLab <- qUnit@qUnitTiny
    yLab <- fluxUnit@unitExpressTiny
  } else {
    xLab<-qUnit@qUnitExpress
    yLab<-fluxUnit@unitExpress
  }
  
  if(logScale){
    logText <- "xy"
    yMin <- fluxMin
  } else {
    logText <- ""
    yMin <- 0
  }
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Flux versus Discharge") else ""
  
  ##############################################
  
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, logScale=logScale, tinyPlot=tinyPlot, padPercent=5)
  yInfo <- generalAxis(x=yHigh, minVal=fluxMin, maxVal=fluxMax, logScale=logScale, tinyPlot=tinyPlot, padPercent=5)
  
  genericEGRETDotPlot(x=x, y=yHigh,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,cex=cex,col=col,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks, tinyPlot=tinyPlot, customPar=customPar,
                      plotTitle=plotTitle, log=logText,cex.axis=cex.axis,cex.main=cex.main, ...
  )
  
  censoredSegments(yInfo$bottom,yLow,yHigh,x,Uncen,col=col,lwd=lwd)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  
}
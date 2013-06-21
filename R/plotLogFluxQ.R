#' Sample data plot: observed log flux vs log discharge
#'
#' @description
#' Concentration and discharge data used to compute flux come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#'  contains an INFO and Sample dataframes, then the following R code will produce a plot:
#'  \code{plotLogFluxQ()}
#'
#' @param localSample string specifying the name of the data frame that contains the concentration and discharge data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable if TRUE plot is designed to fit into a multi-plot array, default is FALSE 
#' @param fluxMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param fluxMin numeric specifying the minimum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' plotLogFluxQ(qUnit = 1, fluxUnit = 1)
#' plotLogFluxQ(fluxUnit = 'kgDay')
#' plotLogFluxQ()
plotLogFluxQ<-function(localSample = Sample,localINFO = INFO, qUnit = 2, 
              fluxUnit = 3, tinyPlot = FALSE, fluxMax = NA, fluxMin = NA, col="black",lwd=1,
                       printTitle = TRUE,cex=0.8, cex.axis=1.1,cex.main=1.1, customPar=FALSE,...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale, 
  # flux on y-axis on a log scale 
  
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

  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Flux versus Discharge") else ""
  
  ##############################################  
  
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, logScale=TRUE, tinyPlot=tinyPlot, padPercent=5)
  yInfo <- generalAxis(x=yHigh, minVal=fluxMin, maxVal=fluxMax, logScale=TRUE, tinyPlot=tinyPlot, padPercent=5)
  
  genericEGRETDotPlot(x=x, y=yHigh, 
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,cex=cex,col=col,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks, tinyPlot=tinyPlot, customPar=customPar,
                      plotTitle=plotTitle, log="xy",cex.axis=cex.axis,cex.main=cex.main, ...
  )

  censoredSegments(yInfo$bottom,yLow,yHigh,x,Uncen,col=col,lwd=lwd)
  
}

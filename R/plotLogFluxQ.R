#' Sample data plot: observed log flux vs log discharge
#'
#' Concentration and discharge data used to compute flux come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration and discharge data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable if TRUE plot is designed to fit into a multi-plot array, default is FALSE 
#' @param fluxMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param fluxMin numeric specifying the minimum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogFluxQ(qUnit = 1, fluxUnit = 1)
#' plotLogFluxQ(fluxUnit = 'kgDay')
#' plotLogFluxQ()
plotLogFluxQ<-function(localSample = Sample,localINFO = INFO, qUnit = 2, 
              fluxUnit = 3, tinyPlot = FALSE, fluxMax = NA, fluxMin = NA, 
                       printTitle = TRUE,...){
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
  #xMin<-0.95*min(x)
  #xMax<-1.05*max(x)
  #xTicks<-if(tinyPlot) logPretty1(xMin,xMax) else logPretty3(xMin,xMax)
  #numXTicks<-length(xTicks)
  #xLeft<-xTicks[1]
  #xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  yLab<-fluxUnit@unitExpress
  #maxYHigh<-if(is.na(fluxMax)) 1.05*max(yHigh) else fluxMax
  #minYLow<-if(is.na(fluxMin)) 0.95*min(yLow,na.rm=TRUE) else fluxMin
  #yTicks<-if(tinyPlot) logPretty1(minYLow,maxYHigh) else logPretty3(minYLow,maxYHigh)
  #numYTicks<-length(yTicks)
  #yBottom<-yTicks[1]
  #yTop<-yTicks[numYTicks]
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Flux versus Discharge") else ""
  
  ##############################################  
  mar<-c(5,5,4,2)+0.1
  
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, log=TRUE, tinyPlot=tinyPlot)
  yInfo <- generalAxis(x=yHigh, minVal=fluxMin, maxVal=fluxMax, log=TRUE, tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh, 
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      plotTitle=plotTitle, mar=mar,log="xy", ...
  )

  censoredSegments(yInfo$bottom,yLow,yHigh,x,Uncen)
  
  par(mar=c(5,4,4,2)+0.1)
}
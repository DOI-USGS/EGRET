#' Plot of Observed Concentration versus Discharge 
#'
#' Data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO. 
#' Discharge is plotted on a log scale.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotConcQ()
plotConcQ<-function(localSample = Sample, localINFO = INFO, qUnit = 2, tinyPlot = FALSE, 
                    concMax = NA, printTitle = TRUE, ...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale, concentration on y-axis
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  qFactor<-qUnit@qUnitFactor
  x<-localSample$Q*qFactor
  yLow<-localSample$ConcLow
  yHigh<-localSample$ConcHigh
  Uncen<-localSample$Uncen
  xMin<-0.95*min(x)
  xMax<-1.05*max(x)
  xTicks<-if(tinyPlot) logPretty1(xMin,xMax) else logPretty3(xMin,xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  maxYHigh<-if(is.na(concMax)) 1.05*max(yHigh) else concMax
  #yTicks<-yPretty(maxYHigh)
  #yTop<-yTicks[length(yTicks)]
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Concentration versus Discharge") else ""
  yBottom <- 0 #No value given, so assuming zero due to the ylim value
  
  yLab <- "Concentration in mg/L"
  mar<-c(5,4,4,2)+0.1
  
  xInfo <- generalAxis(x=x, max=xMax, min=xMin, log=TRUE, tinyPlot=tinyPlot)

  yInfo <- generalAxis(x=yHigh, max=maxYHigh, min=min(yHigh), tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh, 
                      xlim=c(xLeft, xRight), ylim=c(0,yInfo$top),
                      xlab=xLab, ylab=yLab,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      plotTitle=plotTitle, mar=mar,log="x", ...
  )
  
  #   plot(log(x,10),yHigh,axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab=xLab,ylim=c(0,yTop),yaxs="i",ylab="Concentration in mg/L",main=plotTitle,pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2)
  #   axis(1,tcl=0.5,at=log(xTicks,10),labels=xTicks)
  #   axis(2,tcl=0.5,las=1,at=yTicks)
  #   axis(3,tcl=0.5,at=log(xTicks,10),labels=FALSE)
  #   axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  #   box()
  censoredSegments(yBottom, yLow, yHigh, x, Uncen
  )
  #   yLowVal<-ifelse(is.na(yLow),0,yLow)
  #   numSamples<-length(x)
  #   uncensoredIndex <- 1:numSamples
  #   uncensoredIndex <- uncensoredIndex[Uncen==0]
  #   segments(x[uncensoredIndex],yLowVal[uncensoredIndex],x[uncensoredIndex],yHigh[uncensoredIndex])
  #   segments(log(x[uncensoredIndex],10),yLowVal[uncensoredIndex],log(x[uncensoredIndex],10),yHigh[uncensoredIndex])
}
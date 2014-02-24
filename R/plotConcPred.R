#' Plot of Observed Concentration versus Estimated Concentration 
#'
#' @description
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#' contains an INFO and Sample dataframes, then the following R code will produce a plot:
#' \code{plotConcPred()}
#'
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @param localINFO data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param logScale logical, default TRUE, TRUE indicates y axis is in log scale, "xy" indicates both x and y in log scale, "x" is only x
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
#' # Water year:
#' INFO <- setPA()
#' plotConcPred()
#' plotConcPred(logScale=TRUE)
#' # Graphs consisting of Jun-Aug
#' INFO <- setPA(paStart=6,paLong=3)
#' plotConcPred()
plotConcPred<-function(localSample = Sample, localINFO = INFO, concMax = NA, logScale=FALSE,
                       printTitle = TRUE,tinyPlot=FALSE,cex=0.8, cex.axis=1.1,
                       cex.main=1.1, customPar=FALSE,col="black",lwd=1,...){
  # this function shows observed versus predicted concentration
  # predicted concentration on the x-axis (these include the bias correction), 
  # observed concentration on y-axis 
  # these predictions are from a "leave-one-out" cross validation application of WRTDS 
  
  paLong <- localINFO$paLong
  paStart <- localINFO$paStart  
  localSample <- if(paLong == 12) localSample else selectDays(paLong,paStart,localDaily=localSample)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  x<-localSample$ConcHat
  yLow<-localSample$ConcLow
  yHigh<-localSample$ConcHigh
  Uncen<-localSample$Uncen

  if(tinyPlot){
    xLab<-"Est. Conc."
    yLab<-"Obs. Conc."
  } else {
    xLab<-"Estimated Concentration in mg/L"
    yLab<-"Observed Concentration in mg/L"
  }
  
  if (logScale){
    minYLow <- NA
    minXLow <- NA
    logVariable <- "xy"
  } else {
    minYLow <- 0
    minXLow <- 0
    logVariable <- ""
  } 
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed versus Estimated Concentration") else ""

  xInfo <- generalAxis(x=x, minVal=minXLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)  
  yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
  
  ############################

  genericEGRETDotPlot(x=x, y=yHigh,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yLab,log=logVariable,
                      plotTitle=plotTitle, oneToOneLine=TRUE,
                      cex.axis=cex.axis,cex.main=cex.main,cex=cex,
                      tinyPlot=tinyPlot,customPar=customPar,col=col,lwd=lwd,...
    )

  censoredSegments(yBottom=yInfo$bottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen,col=col,lwd=lwd)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}

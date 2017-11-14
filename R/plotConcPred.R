#' Plot of Observed Concentration versus Estimated Concentration 
#'
#' @description
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param eList named list with at least the Sample and INFO dataframes
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
#' @param randomCensored logical. Show censored values as randomized.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotConcPred(eList)
#' plotConcPred(eList, logScale=TRUE)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcPred(eList)
plotConcPred<-function(eList, concMax = NA, logScale=FALSE,
                       printTitle = TRUE,tinyPlot=FALSE,cex=0.8, cex.axis=1.1,
                       cex.main=1.1, customPar=FALSE,col="black",lwd=1, randomCensored = FALSE,...){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList) 
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  if(!all((c("SE","yHat") %in% names(eList$Sample)))){
    stop("This function requires running modelEstimation on eList")
  }
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample, paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  x<-localSample$ConcHat

  Uncen<-localSample$Uncen

  if(tinyPlot){
    xLab<-"Est. Conc."
    yLab<-"Obs. Conc."
  } else {
    xLab<-paste("Estimated Concentration in",localINFO$param.units)
    yLab<-paste("Observed Concentration in",localINFO$param.units)
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

  if(randomCensored){
    if(!("rObserved" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
    }
    yHigh <- localSample$rObserved
    yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
    
    genericEGRETDotPlot(x=x[Uncen == 1], y=yHigh[Uncen == 1],
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab=xLab, ylab=yLab,log=logVariable,
                        plotTitle=plotTitle, oneToOneLine=TRUE,
                        cex.axis=cex.axis,cex.main=cex.main,cex=cex,
                        tinyPlot=tinyPlot,customPar=customPar,col=col,lwd=lwd,...
    )
    points(x=x[Uncen == 0], y=yHigh[Uncen == 0], pch=1,cex=cex,col=col)
    
  } else {
    yLow<-localSample$ConcLow
    yHigh<-localSample$ConcHigh
    
    yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
  
    genericEGRETDotPlot(x=x, y=yHigh,
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab=xLab, ylab=yLab,log=logVariable,
                        plotTitle=plotTitle, oneToOneLine=TRUE,
                        cex.axis=cex.axis,cex.main=cex.main,cex=cex,
                        tinyPlot=tinyPlot,customPar=customPar,col=col,lwd=lwd,...
      )
  
    censoredSegments(yBottom=yInfo$bottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen,col=col,lwd=lwd)
  }

  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  invisible(eList)
}

#' Plot of the residuals from WRTDS (in log concentration units) versus time 
#'
#' @description
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus time.
#' It also provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error.
#'
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'  
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param hLine inserts horizontal line at zero
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param randomCensored logical. Show censored residuals as randomized.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotResidTime(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotResidTime(eList)
plotResidTime<-function(eList, stdResid = FALSE, 
                        printTitle = TRUE, hLine=TRUE, tinyPlot=FALSE,col="black",lwd=1,
                        cex=0.8, cex.axis=1.1,cex.main=1.1, customPar=FALSE,randomCensored=FALSE,...){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(!all((c("SE","yHat") %in% names(eList$Sample)))){
    stop("This function requires running modelEstimation on eList")
  }
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample,paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  x<-localSample$DecYear

  Uncen<-localSample$Uncen
  xMin<-min(x) - 0.2
  xMax<-max(x) + 0.2

  xLab<-""
  if(tinyPlot){
    yLab<-if(stdResid) "Standardized Residual" else "Residual"     
  } else {
    yLab<-if(stdResid) "Standardized Residual in natural log units" else "Residual in natural log units"     
  }
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Residual versus Time") else ""
  
  xInfo <- generalAxis(x=x, maxVal=xMax, minVal=xMin,padPercent=0, tinyPlot=tinyPlot)
  
  if(!randomCensored){
    yLow<-log(localSample$ConcLow)-localSample$yHat
    yHigh<-log(localSample$ConcHigh)-localSample$yHat
    
    if(stdResid){
      yLow <- yLow/localSample$SE 
      yHigh <- yHigh/localSample$SE
    }
    
    yInfo <- generalAxis(x=yHigh, maxVal=NA, minVal=NA,padPercent=5, tinyPlot=tinyPlot)
    
    genericEGRETDotPlot(x=x, y=yHigh,
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,col=col,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom, yInfo$top),
                        xlab=xLab, ylab=yLab, plotTitle=plotTitle, customPar=customPar, cex=cex,
                        cex.axis=cex.axis,cex.main=cex.main, hLine=hLine, tinyPlot=tinyPlot,...
    )
    censoredSegments(yBottom = yInfo$bottom, yLow = yLow, yHigh = yHigh, x = x, Uncen = Uncen,col=col,lwd=lwd)
    
  } else {
    if(!("rResid" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
    }
    yHigh <- localSample$rResid

    if(stdResid){
      yHigh <- yHigh/localSample$SE
    }

    yInfo <- generalAxis(x=yHigh, maxVal=NA, minVal=NA,padPercent=5, tinyPlot=tinyPlot)

    genericEGRETDotPlot(x=x[Uncen == 1], y=yHigh[Uncen == 1],
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,col=col,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom, yInfo$top),
                        xlab=xLab, ylab=yLab, plotTitle=plotTitle, customPar=customPar, cex=cex,
                        cex.axis=cex.axis,cex.main=cex.main, hLine=hLine, tinyPlot=tinyPlot,...
    )
    points(x=x[Uncen == 0], y=yHigh[Uncen == 0],cex=cex,col=col)

  }
  
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

  invisible(eList)
}
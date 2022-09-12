#' Plot of the residuals from WRTDS versus the estimated values (all in log concentration units)
#'
#' @description
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus the estimated values, also in natural log concentration units.  These estimates are
#' the log-space estimates prior to bias-correction.  
#' The function provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error. 
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default.
#'  
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for x and y labels relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param concLab object of concUnit class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @param randomCensored logical. Show censored residuals as randomized.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords water-quality statistics graphics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotResidPred(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotResidPred(eList)
plotResidPred <- function(eList, stdResid = FALSE, 
                          tinyPlot = FALSE, 
                          printTitle = TRUE,
                          col = "black", lwd = 1,
                          cex = 0.8, cex.axis = 1.1,
                          cex.main = 1.1, customPar = FALSE,
                          randomCensored = FALSE,
                          concLab = 1, ...){
  # this function shows residual versus estimated in log space
  # estimated log concentration on the x-axis (these are prior to bias correction), 
  # observed log concentration on y-axis 
  # these estimates are from a "leave-one-out" cross validation application of WRTDS
  # if stdResid=FALSE it just works with the regular residuals
  # if stdResid=TRUE it computes the standardized residual which is the residual/Sample$SE  
  
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
  
  if (is.numeric(concLab)){
    concPrefix <- concConst[shortCode=concLab][[1]]    
  } else if (is.character(concLab)){
    concPrefix <- concConst[concLab][[1]]
  } else {
    concPrefix <- concLab
  }
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample,paLong,paStart)
  
  if(paLong==12){
    title2 <- ""
  }  else {
    title2 <- setSeasonLabelByUser(paStartInput = paStart,
                                   paLongInput = paLong)
  }
  
  x <- exp(localSample$yHat)
  xInfo <- generalAxis(x=log(x), minVal=NA, maxVal=NA, tinyPlot=tinyPlot)
  
  Uncen<-localSample$Uncen
  
  if (tinyPlot){
    xLab <- paste("Est.", concPrefix@shortPrefix, "in natural log units")
    yLab <- if(stdResid) "Std. residual" else "Residual"
  }  else {
    xLab <- paste("Estimated", tolower(concPrefix@longPrefix),
                  "in natural log units")
    yLab <- if(stdResid) "Standardized residual in natural log units" else "Residual in natural log units"
  }
  
  if(printTitle) {
    plotTitle <- paste(localINFO$shortName,"\n",
                       localINFO$paramShortName,"\n",
                       "Residual versus Estimated", concPrefix@longPrefix)
  } else {
    plotTitle <- ""
  }
  ####################
  
  if(!randomCensored){
    
    yLow<-log(localSample$ConcLow)-localSample$yHat
    yHigh<-log(localSample$ConcHigh)-localSample$yHat
    yLow<-if(stdResid) yLow/localSample$SE else yLow
    yHigh<-if(stdResid) yHigh/localSample$SE else yHigh
    
    yInfo <- generalAxis(x=yHigh, minVal=NA, maxVal=NA, tinyPlot=tinyPlot)
    
    genericEGRETDotPlot(x=log(x), y=yHigh,
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,col=col,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab=xLab, ylab=yLab, plotTitle=plotTitle, customPar=customPar,cex=cex,
                        hLine=TRUE,cex.axis=cex.axis,cex.main=cex.main, tinyPlot=tinyPlot,...
      )
  
    censoredSegments(yInfo$bottom, yLow, yHigh, log(x), Uncen, col=col, lwd=lwd)
    
  } else {
    if(!("rResid" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
    }

    yHigh <- localSample$rResid
    Uncen <- localSample$Uncen

    if(stdResid){
      yHigh <- yHigh/localSample$SE
    }

    yInfo <- generalAxis(x=yHigh, minVal=NA, maxVal=NA, tinyPlot=tinyPlot)

    genericEGRETDotPlot(x=log(x[Uncen == 1]), y=yHigh[Uncen == 1],
                        xTicks=xInfo$ticks, yTicks=yInfo$ticks,col=col,
                        xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                        xlab=xLab, ylab=yLab, plotTitle=plotTitle, customPar=customPar,cex=cex,
                        hLine=TRUE,cex.axis=cex.axis,cex.main=cex.main, tinyPlot=tinyPlot,...
    )
    points(x=log(x[Uncen == 0]), y=yHigh[Uncen == 0], pch=1,cex=cex,col=col)

  }
  
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  
  invisible(eList)

}

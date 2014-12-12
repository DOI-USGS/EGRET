#' Plot of the residuals from WRTDS (in log concentration units) versus the discharge 
#'
#' @description
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus the discharge shown on a log scale. 
#' The function also provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default.
#'  
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param rmSciX logical defaults to FALSE, changes x label from scientific to fixed
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
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotResidQ(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotResidQ(eList)
plotResidQ<-function (eList, qUnit = 2, 
                      tinyPlot = FALSE, stdResid = FALSE, printTitle = TRUE,col="black",lwd=1,
                      cex=0.8, cex.axis=1.1,cex.main=1.1,rmSciX=FALSE, customPar=FALSE,...){  
   
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }  
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample,paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  if (is.numeric(qUnit)) {
     qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
     qUnit <- qConst[qUnit][[1]]
  }
   
  qFactor <- qUnit@qUnitFactor
  x <- localSample$Q * qFactor
   
  yLow <- log(localSample$ConcLow) - localSample$yHat
  yHigh <- log(localSample$ConcHigh) - localSample$yHat
   
  yLow <- if(stdResid){
     yLow/localSample$SE
  } else {
       yLow
  }
   
  yHigh <- if(stdResid){
     yHigh/localSample$SE
  } else {
       yHigh
  }
   
  Uncen <- localSample$Uncen
  
  if (tinyPlot){
     xLab <- qUnit@qUnitTiny
     yLab <- ifelse(stdResid, "Standardized Residual", "Residual")
  } else {
     xLab <- qUnit@qUnitExpress
     yLab <- ifelse(stdResid, "Standardized Residual in natural log units", "Residual in natural log units")
  }
  
  plotTitle <- ifelse (printTitle,  paste(localINFO$shortName, "\n", localINFO$paramShortName, 
           "\n", "Residual versus Discharge"), "")
   
   #######################
   
  xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, logScale=TRUE, tinyPlot=tinyPlot,padPercent=5)   
  #    yInfo <- generalAxis(x=yHigh, minVal=(min(yLow, na.rm = TRUE) - 0.5), maxVal=(max(yHigh) + 0.1), tinyPlot=tinyPlot)
  yInfo <- generalAxis(x=yHigh, minVal=NA, maxVal=NA, tinyPlot=tinyPlot,padPercent=5)
   
  genericEGRETDotPlot(x=x, y=yHigh,
                       xTicks=xInfo$ticks, yTicks=yInfo$ticks,hLine=TRUE,
                       xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, yInfo$top),
                       xlab = xLab, ylab = yLab, plotTitle=plotTitle,cex=cex,
                       log = "x", cex.axis=cex.axis,cex.main=cex.main, col=col,
                       tinyPlot=tinyPlot,rmSciX=rmSciX, customPar=customPar,...
  )
  
  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen,col=col, lwd=lwd )
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
   
}

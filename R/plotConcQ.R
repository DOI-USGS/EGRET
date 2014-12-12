#' Plot of Observed Concentration versus Discharge 
#'
#' @description
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#' Discharge is plotted on a log scale.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param logScale logical if TRUE x and y plotted in log axis
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param concMin numeric if you want to specify the minimum concentration value to display, you can do so with the argument concMin, otherwise it will be automatic
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param rmSciX logical defaults to FALSE, changes x label from scientific to fixed
#' @param rmSciY logical defaults to FALSE, changes y label from scientific to fixed
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
#' plotConcQ(eList)
#' plotConcQ(eList, logScale=TRUE)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcQ(eList)
plotConcQ<-function(eList, qUnit = 2, tinyPlot = FALSE, logScale=FALSE,
                    concMax = NA, concMin =NA, printTitle = TRUE, cex=0.8, cex.axis=1.1,cex.main=1.1,
                    rmSciX=FALSE,rmSciY=FALSE, customPar=FALSE,col="black",lwd=1,...){
  # this function shows the sample data,
  # discharge on x-axis on a log scale, concentration on y-axis
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample, paLong,paStart)
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
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

  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Concentration versus Discharge") else ""
  
  if (tinyPlot){
    xLab<-qUnit@qUnitTiny
  } else {
    xLab<-qUnit@qUnitExpress
  }
  
  if(logScale){
    logScaleText <- "xy"
    yMin <- concMin
  } else {
    logScaleText <- "x"
    yMin <- 0
  }
  
  yInfo <- generalAxis(x=yHigh, maxVal=concMax, minVal=yMin, tinyPlot=tinyPlot,logScale=logScale,units=localINFO$param.units)
  xInfo <- generalAxis(x=x, maxVal=NA, minVal=NA, logScale=TRUE, tinyPlot=tinyPlot)
  
  genericEGRETDotPlot(x=x, y=yHigh, 
                      xlim=c(xInfo$bottom, xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab=xLab, ylab=yInfo$label,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                      plotTitle=plotTitle, log=logScaleText,cex.axis=cex.axis,cex=cex,
                      cex.main=cex.main, tinyPlot=tinyPlot,xaxt="n",
                      rmSciX=rmSciX,rmSciY=rmSciY,customPar=customPar,col=col,lwd=lwd,...
  )
  
  censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen,col=col,lwd=lwd)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}

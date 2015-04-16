#' Box plot of the water quality data by month
#'
#' @description
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#'  
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param tcl number defaults to 0.5, specifies length of tick marks as fraction of height of a line of text
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param logScale logical if TRUE y plotted in log axis
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param las numeric in {0,1,2,3}; the style of axis labels, see ?par
#' @param showXLabels logical defaults to TRUE. If FALSE, the x axis label is not plotted
#' @param showYLabels logical defaults to TRUE. If FALSE, the y axis label is not plotted
#' @param showXAxis logical defaults to TRUE. If FALSE, the x axis is not plotted
#' @param showYAxis logical defaults to TRUE. If FALSE, the y axis is not plotted
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @import methods
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' boxConcMonth(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' boxConcMonth(eList)
boxConcMonth<-function(eList, printTitle = TRUE,
                       cex=0.8, cex.axis=1.1, cex.main=1.1, las=1,logScale=FALSE,tcl=0.5,
                       tinyPlot = FALSE, customPar=FALSE,showYLabels=TRUE,
                       showXLabels=TRUE,showXAxis=TRUE,showYAxis=TRUE,...) {
  
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
  #This function makes a boxplot of log concentration by month
  #Box width is proportional to the square root of the sample size
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of sample values by month") else ""
  #   nameList <- sapply(c(1:12),function(x){monthINFO[[x]]@monthSingle})
  nameList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  
  namesListFactor <- factor(nameList, levels=nameList)
  monthList <- as.character(apply(localSample, 1, function(x)  monthInfo[[as.numeric(x[["Month"]])]]@monthAbbrev))
  monthList <- factor(monthList, namesListFactor)
  tempDF <- data.frame(month=monthList, conc=localSample$ConcAve)
  
  maxY<-1.02*max(localSample$ConcHigh, na.rm=TRUE)
  ySpan<-c(0,maxY)
#   yTicks<-pretty(ySpan, n = 7)
#   yMax<-yTicks[length(yTicks)]
  
  if(logScale){
    logScaleText <- "y"
  } else {
    logScaleText <- ""
  }
  
  if (tinyPlot) {
    yLabel <- paste("Conc. (",localINFO$param.units,")",sep="")
    if (!customPar) par(mar=c(4,5,1,0.1),cex.lab=cex.axis,tcl=0.5)
    names <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  } else {
    yLabel <- paste("Concentration in", localINFO$param.units)
    if (!customPar) par(mar=c(5,6,4,2)+0.1,cex.lab=cex.axis,tcl=0.5)
    names <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  }
    
  yInfo <- generalAxis(x=tempDF$conc, maxVal=maxY, minVal=min(localSample$ConcHigh, na.rm=TRUE), 
                       tinyPlot=tinyPlot,logScale=logScale,units=localINFO$param.units)
  yTicksLab <- prettyNum(yInfo$ticks)
  
  boxplot(tempDF$conc ~ tempDF$month,
          ylim=c(yInfo$bottom,yInfo$top),yaxs="i", yTicks=yInfo$ticks,
          varwidth=TRUE,yaxt="n", 
          names=names,
          xlab=if(showXLabels) "Month" else "",
          ylab=if(showYLabels) yInfo$label else "",
          main=plotTitle,
          cex=cex,cex.axis=cex.axis,cex.main=cex.main,
          las=las,tcl=tcl,
          log=logScaleText,
          ...)  

  if(showYAxis){
    axis(2,tcl=tcl,las=las,at=yInfo$ticks,cex.axis=cex.axis,labels=yTicksLab)
  } else {
    axis(2,tcl=tcl,las=las,at=yInfo$ticks,cex.axis=cex.axis,labels=FALSE)    
  }

#   if(showXAxis){
#     axis(1,tcl=tcl,at=xTicks,cex.axis=cex.axis,labels=xTicksLab)  
#   } else {
#     axis(1,tcl=tcl,at=xTicks,labels=FALSE)
#   }
  
  
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}
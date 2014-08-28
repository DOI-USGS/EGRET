#' Box plot of the water quality data by month
#'
#' @description
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO. 
#' These data frames must have been created by the dataRetrieval package. 
#'  
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#'  contains an INFO and Sample dataframes, then the following R code will produce a plot:
#'  \code{boxConcMonth()}
#'
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @param localINFO data frame that contains the metadata, default name is INFO 
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param tcl number defaults to 0.5, specifies length of tick marks as fraction of height of a line of text
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param logScale logical if TRUE y plotted in log axis
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param las numeric in {0,1,2,3}; the style of axis labels, see ?par
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' # Water year:
#' boxConcMonth()
#' # Graphs consisting of Jun-Aug
#' INFO <- setPA(paStart=6,paLong=3)
#' boxConcMonth()
boxConcMonth<-function(localSample = Sample, localINFO = INFO, printTitle = TRUE,
                       cex=0.8, cex.axis=1.1, cex.main=1.1, las=1,logScale=FALSE,tcl=0.5,
                       tinyPlot = FALSE, customPar=FALSE,...) {
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(paLong,paStart,localDaily=localSample)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  #This function makes a boxplot of log concentration by month
  #Box width is proportional to the square root of the sample size
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of sample values by month") else ""
  #   nameList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthSingle})
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
                       tinyPlot=tinyPlot,logScale=logScale,localINFO=localINFO)
  yTicksLab <- prettyNum(yInfo$ticks)
  
  boxplot(tempDF$conc ~ tempDF$month,
          #     localSample$ConcAve~localSample$Month,
          ylim=c(yInfo$bottom,yInfo$top),yaxs="i", yTicks=yInfo$ticks,
          varwidth=TRUE,yaxt="n", 
          names=names,
          xlab="Month",
          ylab=yInfo$label,
          main=plotTitle,
          cex=cex,cex.axis=cex.axis,cex.main=cex.main,
          las=las,tcl=tcl,
          log=logScaleText,
          ...)  
  axis(2,tcl=tcl,las=las,at=yInfo$ticks,cex.axis=cex.axis,labels=yTicksLab)
  
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}
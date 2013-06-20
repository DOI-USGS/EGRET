#' Box plot of the water quality data by month
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO. 
#' These data frames must have been created by the dataRetrieval package. 
#'
#' @param localSample string specifying the name of the data frame, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO 
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param las numeric in {0,1,2,3}; the style of axis labels
#' @param font.main font to be used for plot main titles
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' boxConcMonth()
boxConcMonth<-function(localSample = Sample, localINFO = INFO, printTitle = TRUE,
                       cex=0.8, cex.axis=1.1, cex.main=1.1, las=2, 
                       font.main=2, tinyPlot = FALSE, customPar=FALSE,...) {
  #This function makes a boxplot of log concentration by month
  #Box width is proportional to the square root of the sample size
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of sample values by month") else ""
  #   nameList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthSingle})
  nameList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  
  namesListFactor <- factor(nameList, levels=nameList)
  monthList <- as.character(apply(localSample, 1, function(x)  monthInfo[[as.numeric(x[["Month"]])]]@monthAbbrev))
  monthList <- factor(monthList, namesListFactor)
  tempDF <- data.frame(month=monthList, conc=localSample$ConcAve)
  
  maxY<-1.02*max(localSample$ConcHigh)
  ySpan<-c(0,maxY)
  yTicks<-pretty(ySpan, n = 7)
  yMax<-yTicks[length(yTicks)]
  
  if (tinyPlot) {
    yLabel <- "Conc. (mg/L)"
    if (!customPar) par(mar=c(4,5,1,0.1),cex.lab=cex.axis)
  } else {
    yLabel <- "Concentration in mg/L"
    if (!customPar) par(mar=c(5,6,4,2)+0.1,cex.lab=cex.axis)
  }
    
  boxplot(tempDF$conc ~ tempDF$month,
          #     localSample$ConcAve~localSample$Month,
          ylim=c(0,yMax),yaxs="i",
          varwidth=TRUE,
          names=nameList,
          xlab="Month",
          ylab=yLabel,
          main=plotTitle,
          cex=cex,cex.axis=cex.axis,cex.main=cex.main,
          las=las,        #makes all text perpendicular to axis
          font.main=font.main,
          ...)  

}
#' Box plot of the water quality data by month
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO. 
#' These data frames must have been created by the dataRetrieval package. 
#'
#' @param localSample string specifying the name of the data frame, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO 
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSampleStart
#' INFO <- exINFOStart
#' boxConcMonth()
boxConcMonth<-function(localSample = Sample, localINFO = INFO, printTitle = TRUE) {
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
  
  boxplot(tempDF$conc ~ tempDF$month,
          #     localSample$ConcAve~localSample$Month,
          ylim=c(0,yMax),yaxs="i",
          varwidth=TRUE,
          names=nameList,
          xlab="Month",
          ylab="Concentration in mg/L",
          main=plotTitle,
          cex=0.8,cex.axis=0.9,cex.main=1.0,
          las=2,        #makes all text perpendicular to axis
          font.main=2)  
}
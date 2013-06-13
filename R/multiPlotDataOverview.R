#' Produces a 4 panel plot that gives an overview of the data set prior to any processing
#'
#' The four plots produced are 1) log concentration versus log discharge, 2) log concentration versus time
#' 3) a boxplot of log concentration by month, and 
#' 4) a side-by-side boxplot of the sampled discharges and all daily discharges. 
#' To save space, the graphic is labeled only at the top of the 4 graph display. 
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' multiPlotDataOverview(qUnit=1)
multiPlotDataOverview<-function (localSample = Sample, localDaily = Daily, localINFO = INFO, qUnit = 2){
  par(mfcol=c(2,2),oma=c(0,2.4,4.5,2.4),tcl=0.5)
  plotLogConcQ(localSample = localSample, qUnit = qUnit, tinyPlot = TRUE, printTitle = FALSE,rmSciX=TRUE)
  boxConcMonth(localSample = localSample, printTitle = FALSE, tinyPlot=TRUE)
  plotLogConcTime(localSample = localSample, printTitle = FALSE, tinyPlot = TRUE)
  boxQTwice(localSample = localSample, localDaily = localDaily, printTitle = FALSE, qUnit = qUnit, tinyPlot=TRUE)
  title<-paste(localINFO$shortName,"\n",localINFO$paramShortName)
  mtext(title,cex=1.2,outer=TRUE,font=2)
  par(mfcol=c(1,1),oma=c(0,0,0,0))
}
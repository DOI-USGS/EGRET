#' Produces a 4 panel plot that gives an overview of the data set prior to any processing
#'
#' @description
#' The four plots produced are 1) log concentration versus log discharge, 2) log concentration versus time
#' 3) a boxplot of log concentration by month, and 
#' 4) a side-by-side boxplot of the sampled discharges and all daily discharges. 
#' To save space, the graphic is labeled only at the top of the 4 graph display. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'
#' @param eList named list with at least Daily, Sample, and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param logScaleConc logical if TRUE y in concentration graphs plotted in log axis. Default is TRUE.
#' @param logScaleQ logical if TRUE y in streamflow graphs plotted in log axis. Default is TRUE.
#' @keywords graphics water-quality statistics
#' @seealso \code{\link{plotConcQ}}, \code{\link{boxConcMonth}}, \code{\link{plotConcTime}}, \code{\link{boxQTwice}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' multiPlotDataOverview(eList, qUnit=1)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' multiPlotDataOverview(eList, qUnit=1) 
multiPlotDataOverview<-function (eList, qUnit = 2,cex.main=1.2,
                                 logScaleConc=TRUE, logScaleQ=TRUE){
  
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  par(mfcol=c(2,2),oma=c(0,2.4,4.5,2.4),tcl=0.5)
  plotConcQ(eList, qUnit = qUnit, tinyPlot = TRUE, printTitle = FALSE,rmSciX=TRUE,logScale=logScaleConc)
  boxConcMonth(eList, printTitle = FALSE, tinyPlot=TRUE,logScale=logScaleConc)
  plotConcTime(eList, printTitle = FALSE, tinyPlot = TRUE,logScale=logScaleConc)
  boxQTwice(eList, printTitle = FALSE, qUnit = qUnit, tinyPlot=TRUE,logScale=logScaleQ)
  title<-paste(localINFO$shortName,"\n",localINFO$paramShortName)
  
  if("" == title2){
    mtext(title,cex=cex.main,outer=TRUE,font=2)
  } else {
    title <- paste(title, title2, sep="\n")
    mtext(title, cex = cex.main*.75, outer = TRUE, font = 2)    
  }
  
  
  
  par(mfcol=c(1,1),oma=c(0,0,0,0))
}
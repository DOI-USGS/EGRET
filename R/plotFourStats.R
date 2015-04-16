#' Makes four graphs of annual streamflow statistics on a single page
#'
#' @description
#'  Part of the flowHistory system.  The four statistics are 1-day maximum, annual mean, annual median, and annual 7-day minimum.
#'  Although there are a lot of optional arguments to this function, most are set to a logical default.
#'  
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least Daily and INFO dataframes
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{plotFlowSingle}}
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' # Water year:
#' plotFourStats(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList,paStart=6,paLong=3)
#' plotFourStats(eList)
#' }
plotFourStats<-function(eList, yearStart = NA, yearEnd = NA, 
                        printTitle = TRUE, runoff = FALSE, cex.main = 1.2,
                        qUnit =1,cex.axis=1.2,cex=0.8, col="black", lwd=1,...) {
  # prior to running this user must do these two commands
  # INFO<-setPA(pastart,paLong,window) 
  # annualSeries<-makeAnnualSeries()
  #
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localAnnualSeries <- makeAnnualSeries(eList)
  
  par(mfcol=c(2,2),oma=c(0,1.7,6,1.7))
  setYearStart<-if(is.na(yearStart)) min(localAnnualSeries[1,,],na.rm=TRUE) else yearStart
  setYearEnd<-if(is.na(yearEnd)) max(localAnnualSeries[1,,],na.rm=TRUE) else yearEnd
  plotFlowSingle(eList, istat=8, yearStart=setYearStart, yearEnd=setYearEnd, 
                 tinyPlot=TRUE, runoff=runoff, 
                 qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE,
                 cex.axis=cex.axis,cex=cex, col=col,lwd=lwd, cex.main=1,...)
  plotFlowSingle(eList, istat=4, yearStart=setYearStart, yearEnd=setYearEnd, 
                 tinyPlot=TRUE, runoff=runoff, 
                 qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE,
                 cex.axis=cex.axis,cex=cex, col=col,lwd=lwd, cex.main=1, ...)
  plotFlowSingle(eList, istat=5, yearStart=setYearStart, yearEnd=setYearEnd, 
                 tinyPlot=TRUE, runoff=runoff, 
                 qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE,
                 cex.axis=cex.axis,cex=cex, col=col,lwd=lwd, cex.main=1, ...)
  plotFlowSingle(eList, istat=2, yearStart=setYearStart, yearEnd=setYearEnd, 
                 tinyPlot=TRUE, runoff=runoff, 
                 qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE,
                 cex.axis=cex.axis,cex=cex, col=col,lwd=lwd, cex.main=1, ...)
  textPA<-setSeasonLabelByUser(paStartInput=localINFO$paStart, paLongInput=localINFO$paLong)
  title<-if(printTitle) paste(localINFO$shortName,"\n",textPA)
  mtext(title, outer = TRUE, font = 2,cex=cex.main)
  par(mfcol=c(1,1),oma=c(0,0,0,0))	
}
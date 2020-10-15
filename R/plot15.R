#' Makes 15 graphs of streamflow statistics on a single page.  These encompass the 7-day minimum, mean, and 1-day maximum for each of the following 5 Periods of Analysis: Annual, Fall, Winter, Spring and Summer.  
#'
#' Part of flowHistory system.  All results are expressed as runoff (mm/day). The individual plots are constructed by the same method as used in \code{\link{plotFlowSingle}}.  The annual results are based on the Water Year.  The seasons are defined as the following groups of months: SON, DJF, MAM, JJA. 
#'
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param eList named list with at least the Daily and INFO dataframes
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{plot1of15}}
#' @details
#' For formatting purposes it is best to use the following commands before calling the plot15 function (savePath is the pathname for directory to store the output)
#' #   plotName <- paste(savePath, "plot15.", eList$INFO$shortName, ".ps", sep = "")
#' #   postscript(file = plotName, width = 8, height = 10, horizontal = FALSE, family = "Helvetica")
#' Then after running plot15, the user needs to give the command dev.off()
#' @examples
#' eList <- Choptank_eList
#' \donttest{
#' plot15(eList, yearStart = 1980, yearEnd = 2010)
#' }
plot15<-function(eList, yearStart,yearEnd){

  
  localINFO <- getInfo(eList)
  
  par(mfrow=c(5,3),cex=0.6,oma=c(10,8,10,4),mar=c(1,4,1,1))
  qf<-86/localINFO$drainSqKm
  eList<-setPA(eList, 10,12)
  plot1of15(eList, yearStart,yearEnd,qf,istat=2)
  mtext("7-day minimum",cex=0.8,font=1,side=3,line=1)
  mtext("Annual values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=5)
  mtext("Mean",cex=0.8,font=1,side=3,line=1)
  mtext(localINFO$shortName,cex=1.0,font=1,side=3,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=8)
  mtext("1-day maximum",cex=0.8,font=1,side=3,line=1)
  # fall season
  eList<-setPA(eList,9,3)
  plot1of15(eList, yearStart,yearEnd,qf,istat=2)
  mtext("Fall season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=5)
  plot1of15(eList, yearStart,yearEnd,qf,istat=8)
  # winter season
  eList<-setPA(eList, 12,3)
  plot1of15(eList, yearStart,yearEnd,qf,istat=2)
  mtext("Winter season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=5,)
  plot1of15(eList, yearStart,yearEnd,qf,istat=8,)
  # spring season
  eList <- setPA(eList,3,3)
  plot1of15(eList, yearStart,yearEnd,qf,istat=2)
  mtext("Spring season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=5)
  plot1of15(eList, yearStart,yearEnd,qf,istat=8)
  # summer season
  eList <- setPA(eList, 6,3)
  plot1of15(eList, yearStart,yearEnd,qf,istat=2,isBottom=TRUE)
  mtext("Summer season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(eList, yearStart,yearEnd,qf,istat=5,isBottom=TRUE)
  plot1of15(eList, yearStart,yearEnd,qf,istat=8,isBottom=TRUE)
  caption<-paste("Streamflow statistics (circles) in units of millimeters per day, annual values and seasonal values\nFall (Sept., Oct., and Nov.), Winter (Dec., Jan., and Feb.), Spring (Mar., Apr., and May), and Summer (June, July, and Aug.)\nand locally weighted scatterplot smooth (solid curve) for ",
                 localINFO$shortName," for ",yearStart," - ",yearEnd,".",sep="")
  mtext(caption,side=1,outer=TRUE,line=7,adj=0,font=1,cex=0.7)
}

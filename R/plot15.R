#' Makes 15 graphs of streamflow statistics on a single page
#'
#' Part of flowHistory system.
#'
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param eList named list with at least the Daily and INFO dataframes
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{plot1of15}}
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' pdf("plot15.pdf",heigh=10,width=8)
#' plot15(eList, yearStart=1990,yearEnd=2000)
#' dev.off()
#' }
plot15<-function(eList, yearStart,yearEnd){
#   plotName<-paste(savePath,"plot15.",localINFO$staAbbrev,".ps",sep="")
#   postscript(file=plotName,width=8,height=10,horizontal=FALSE,family="Helvetica")
  
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
#   dev.off()
}

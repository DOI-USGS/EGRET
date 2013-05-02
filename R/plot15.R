#' Makes 15 graphs of streamflow statistics on a single page
#'
#' Part of flowHistory system.
#'  This function saves a graphic as a postscript file. 
#'  It assigns the file a name using the abbreviations for station.
#'
#' @param savePath string specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param localDaily string specifying the name of the data frame that contains the daily data, default is Daily
#' @param localINFO string specifying the name of the data frame that contains the metadata, default is INFO
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' Daily <- exDaily
#' INFO <- exINFO
#' \dontrun{plot15("",yearStart=1990,yearEnd=2000)}
plot15<-function(savePath,yearStart,yearEnd,localDaily=Daily,localINFO=INFO){
  plotName<-paste(savePath,"plot15.",localINFO$staAbbrev,".ps",sep="")
  postscript(file=plotName,width=8,height=10,horizontal=FALSE,family="Helvetica")
  par(mfrow=c(5,3),cex=0.6,oma=c(10,8,10,4),mar=c(1,4,1,1))
  qf<-86/localINFO$drainSqKm
  newINFO<-setPAx(10,12,localINFO=localINFO)
  newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
  plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
  mtext("7-day minimum",cex=0.8,font=1,side=3,line=1)
  mtext("Annual values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
  mtext("Mean",cex=0.8,font=1,side=3,line=1)
  mtext(INFO$shortName,cex=1.0,font=1,side=3,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
  mtext("1-day maximum",cex=0.8,font=1,side=3,line=1)
  # fall season
  newINFO<-setPAx(9,3,localINFO=localINFO)
  newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
  plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
  mtext("Fall season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
  plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
  # winter season
  newINFO<-setPAx(12,3,localINFO=localINFO)
  newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
  plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
  mtext("Winter season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
  plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
  # spring season
  newINFO<-setPAx(3,3,localINFO=localINFO)
  newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
  plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
  mtext("Spring season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
  plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
  # summer season
  newINFO<-setPAx(6,3,localINFO=localINFO)
  newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
  plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
  mtext("Summer season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
  plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
  plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
  caption<-paste("Streamflow statistics (circles) in units of millimeters per day, annual values and seasonal values\nFall (Sept., Oct., and Nov.), Winter (Dec., Jan., and Feb.), Spring (Mar., Apr., and May), and Summer (June, July, and Aug.)\nand locally weighted scatterplot smooth (solid curve) for ",localINFO$shortName," for ",yearStart," - ",yearEnd,".",sep="")
  mtext(caption,side=1,outer=TRUE,line=7,adj=0,font=1,cex=0.7)
  dev.off()
}

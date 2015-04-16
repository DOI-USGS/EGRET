#' Print annual results for a given streamflow statistic
#'
#' Part of the flowHistory system.  
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum. 
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @keywords streamflow statistics
#' @return data frame with:
#' \tabular{lll}{
#' years \tab integer \tab year  \cr
#' qActual \tab numeric \tab Actual flow statistic (based on istat)  \cr
#' qSmooth \tab numeric \tab Smoothed flow statistic \cr
#' }
#' @export
#' @examples
#' eList <- Choptank_eList
#' printReturn <- printSeries(eList, 5)
printSeries<-function(eList, istat, qUnit = 1, runoff = FALSE) {
  
  localINFO <- getInfo(eList)
  localAnnualSeries <- makeAnnualSeries(eList)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ###############################################################################
  cat("\n",localINFO$shortName)
  seasonText<-setSeasonLabelByUser(paStartInput=localINFO$paStart,paLongInput=localINFO$paLong)
  cat("\n",seasonText)
  nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
  cat("\n   ",nameIstat[istat])
  unitsText<-if(runoff) "runoff in mm/day" else qUnit@qUnitName
  cat("\n   ",unitsText)
  cat("\n   year   annual   smoothed\n           value    value\n\n")
  qActual<-localAnnualSeries[2,istat,]
  qSmooth<-localAnnualSeries[3,istat,]
  years<-localAnnualSeries[1,istat,]
  qFactor<-qUnit@qUnitFactor
  qActual<-if(runoff) qActual*86.4/localINFO$drainSqKm else qActual*qFactor
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  toPrint<-data.frame(years,qActual,qSmooth)
  toPrint<-subset(toPrint,!is.na(years))
  toPrint$years<-format(toPrint$years,digits=4,width=7)
  toPrint$qActual<-format(toPrint$qActual,digits=3,width=8)
  toPrint$qSmooth<-format(toPrint$qSmooth,digits=3,width=8)
  write.table(toPrint,file="",col.names=FALSE,row.names=FALSE,quote=FALSE)
  toPrint$years <- as.integer(toPrint$years)
  toPrint$qActual <- as.numeric(toPrint$qActual)
  toPrint$qSmooth <- as.numeric(toPrint$qSmooth)
  invisible(toPrint)
}

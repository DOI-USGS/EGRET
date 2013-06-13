#' Computes several values of the flow duration curve for streamflow centered on a specific date of the year
#'
#' This function is useful for helping the analyst determine the empirical probability distribution of 
#' streamflow for a particular part of the year or for the whole year. 
#' This is particularly useful in setting up discharge scales for various other plots in this package. 
#'
#' @param centerDate string specifying the center date of the part of the year for which the flow duration is to be calculated, it is in the form "mm-dd" (it must be in quotes), default is "09-30"
#' @param localDaily string specifying the name of the data frame that contains the daily discharge data, default name is Daily
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param span number this is the half-width of the window over which the discharge values are to be used in constructing the flow-duration curve. If the full year is desired any value greater than 182 will provide serve. Note that for a window of about 2-months width, a span value shoud be about 30.
#' @keywords streamflow, statistics
#' @export
#' @examples
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' flowDuration("06-25", qUnit=1,span=30) # for a window of 30 days either side of June 25 expressed in units of cubic feet per second
#' flowDuration("01-01", qUnit=2) # for a flow-duration curve covering the whole year, expressed in units of cubic meters per second 
flowDuration<-function(centerDate = "09-30", localDaily = Daily, localINFO = INFO, qUnit = 2, span = 365) {
  # this function prints out a set of key points on the flow duration curve of daily flows
  # centerDate is in the form "mm-dd"
  # span is the half-width over which the flows are included in the analysis
  #   for example if we wanted to look at a period that is within 30 days either side of September 1
  #     we would set centerDate="09-01" and span=30
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  spanFull<-span
  span<-if(spanFull>182) 182 else spanFull
  centerDate<-paste("2001-",centerDate)
  centerDate<-as.Date(centerDate)
  centerDay<-as.POSIXlt(centerDate)$yday+1
  startDay<-centerDay-span
  endDay<-centerDay+span
  goodDays<-seq(startDay,endDay,1)
  goodDays<-ifelse(goodDays>0,goodDays,goodDays+365)
  goodDays<-ifelse(goodDays<366,goodDays,goodDays-365)
  numDays<-length(localDaily$Day)
  isGood<-rep(FALSE,numDays)
  for(i in 1:numDays) {
    count<-ifelse(localDaily$Day[i]==goodDays,1,0)
    isGood[i]<-if(sum(count)>0) TRUE else FALSE
  }
  spanDaily<-data.frame(localDaily,isGood)
  spanDaily<-subset(spanDaily,isGood)
  n<-length(spanDaily$Day)
  Q<-spanDaily$Q
  Q<-sort(Q)
  index<-c(1,floor(0.05*n),floor(0.1*n),floor(0.25*n),floor(0.5*n),ceiling(0.75*n),ceiling(0.9*n),ceiling(0.95*n),n)
  QDuration<-c(Q[index[]])
  QDuration<-QDuration*qUnit@qUnitFactor
  monthCenter<-as.POSIXlt(centerDate)$mon+1
  dayCenter<-as.POSIXlt(centerDate)$mday
  monthStart<-as.POSIXlt(centerDate-span)$mon+1
  dayStart<-as.POSIXlt(centerDate-span)$mday
  monthEnd<-as.POSIXlt(centerDate+span)$mon+1
  dayEnd<-as.POSIXlt(centerDate+span)$mday
  cat("\nFlow Duration for",localINFO$shortName,"\n")
  # 	if(spanFull>182) cat("\nFlow duration is based on full year") else cat("\nFlow duration period is centered on",monthFull[monthCenter],dayCenter,"\nAnd spans the period from",monthFull[monthStart],dayStart," To",monthFull[monthEnd],dayEnd)
  if(spanFull>182) cat("\nFlow duration is based on full year") else cat("\nFlow duration period is centered on",monthInfo[[monthCenter]]@monthFull,dayCenter,"\nAnd spans the period from",monthInfo[[monthStart]]@monthFull,dayStart," To",monthInfo[[monthEnd]]@monthFull,dayEnd)
  cat("\n\nDischarge units are",qUnit@qUnitName)
  Qprint<-format(QDuration,digits=3,width=9)
  cat("\n\n       min        5%       10%       25%       50%       75%       90%       95%       max")
  cat("\n",Qprint,"\n")		
}
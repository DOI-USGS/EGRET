#' Produces annual series of 8 streamflow statistics (and a lowess smooth of them) from daily streamflow data
#'
#' Part of the flowHistory system.  The data come from Daily and INFO data frames. 
#' Note that the function setPA must be run before this to establish the period of analysis (e.g. water year).
#'
#' @param localDaily string specifying the name of a data frame that contains the daily streamflow data
#' @param localINFO string specifying the name of a data frame that contains the metadata
#' @keywords statistics streamflow trends
#' @export
#' @return annualSeries data frame that contains the annual series of streamflow statistics
#' @examples 
#' Daily <- exDaily
#' INFO <- exINFO
#' annualSeries <- makeAnnualSeries()
makeAnnualSeries<-function(localDaily = Daily, localINFO = INFO) {
  paStart<-localINFO$paStart
  paLong<-localINFO$paLong
  window<-localINFO$window
  numDays<-length(localDaily$DecYear)
  yearFirst<-trunc(localDaily$DecYear[1])
  yearLast<-trunc(localDaily$DecYear[numDays])
  monthSeqFirst<-localDaily$MonthSeq[1]
  monthSeqLast<-localDaily$MonthSeq[numDays]
  numYears<-yearLast-yearFirst+2
  #     it is ok if numYears is too large, but not ok if it is too small
  annualSeries<-rep(NA,3*8*numYears)
  dim(annualSeries)<-c(3,8,numYears)
  #  annualSeries is the matrix where results are stored
  #  these are annual time series, but may not be based on the whole year
  #     they are based on the period of analysis, which is a period as short as a month
  #     or a consecutive group of months, or an entire year.
  #     the period of analysis is defined by paStart and paLong
  #  the annualSeries matrix has 3 dimensions
  #   first dimension takes on three possible indexes
  #           	1 is the mean date for the values for that year as a decimal year
  #				2 is the actual value of the given flow statistic for the given year
  #				3 is the smoothed value of the given flow statistic for the given year
  #   second dimension is the flow statistic, these run from 1 to 8 (min day, 7-day min, 30-day min, median,mean,30-day max, 7-day max, and max day)  
  #   third dimension is the year index 
  #  first we will do the low flow statistics
  paStartLow<-if(paLong==12&paStart==10) 4 else paStart
  #     this means that if we are doing water years, then the low flow statistics year starts April 1
  #     in all other situations it starts with the month paStart, just like the average and high flow statistics
  Starts<-seq(paStartLow,monthSeqLast,12)
  Ends<-Starts + paLong - 1
  #     Starts is a vector of the sequence number of the months that start each of the years
  #     Ends is a vector of the sequence number of the months that end each of the years
  startEndSeq<-data.frame(Starts,Ends)
  startEndSeq<-subset(startEndSeq,Ends>=monthSeqFirst)
  startEndSeq<-subset(startEndSeq,Starts<=monthSeqLast)
  numYSeq<-length(startEndSeq$Ends)
  for(i in 1:numYSeq) {
    startSeq<-startEndSeq$Starts[i]
    endSeq<-startEndSeq$Ends[i]
    yearDaily<-subset(localDaily,MonthSeq>=startSeq)
    yearDaily<-subset(yearDaily,MonthSeq<=endSeq)
    # count up the missing days
    counter<-ifelse(is.na(yearDaily$Q),1,0)
    goodDay<-length(yearDaily$Q)-sum(counter)
    #  goodDay is how many good days
    #  we want to have at least about 85% of the possible days with data
    good<-if(goodDay>26*paLong) TRUE else FALSE
    meanDecYear<-if(good) mean(yearDaily$DecYear) else NA
    annualSeries[1,1:3,i]<-meanDecYear
    annualSeries[2,1,i]<-if(good) min(yearDaily$Q,na.rm=TRUE) else NA
    annualSeries[2,2,i]<-if(good) min(yearDaily$Q7,na.rm=TRUE) else NA
    annualSeries[2,3,i]<-if(good) min(yearDaily$Q30,na.rm=TRUE) else NA
  }		
  # now we do the median,mean and high flow statistics
  Starts<-seq(paStart,monthSeqLast,12)
  Ends<-Starts + paLong - 1
  #     Starts is a vector of the sequence number of the months that start each of the years
  #     Ends is a vector of the sequence number of the months that end each of the years
  startEndSeq<-data.frame(Starts,Ends)
  startEndSeq<-subset(startEndSeq,Ends>=monthSeqFirst)
  startEndSeq<-subset(startEndSeq,Starts<=monthSeqLast)
  numYSeq<-length(startEndSeq$Ends)
  for(i in 1:numYSeq) {
    startSeq<-startEndSeq$Starts[i]
    endSeq<-startEndSeq$Ends[i]
    yearDaily<-subset(localDaily,MonthSeq>=startSeq)
    yearDaily<-subset(yearDaily,MonthSeq<=endSeq)
    # count up the missing days
    counter<-ifelse(is.na(yearDaily$Q),1,0)
    goodDay<-length(yearDaily$Q)-sum(counter)
    #  goodDay is how many good days
    #  we want to have at least about 85% of the possible days with data
    good<-if(goodDay>26*paLong) TRUE else FALSE
    meanDecYear<-if(good) mean(yearDaily$DecYear) else NA
    annualSeries[1,4:8,i]<-meanDecYear
    annualSeries[2,4,i]<-if(good) median(yearDaily$Q,na.rm=TRUE) else NA
    annualSeries[2,5,i]<-if(good) mean(yearDaily$Q,na.rm=TRUE) else NA
    annualSeries[2,6,i]<-if(good) max(yearDaily$Q30,na.rm=TRUE) else NA
    annualSeries[2,7,i]<-if(good) max(yearDaily$Q7,na.rm=TRUE) else NA
    annualSeries[2,8,i]<-if(good) max(yearDaily$Q,na.rm=TRUE) else NA
  }
  # now we do the lowess smooth	
  for(istat in 1:8) {
    x<-annualSeries[1,istat,]
    y<-log(annualSeries[2,istat,])
    originalYear<-x
    xy<-data.frame(x,y)
    xy<-na.omit(xy)
    numXY<-length(xy$x)
    x<-xy$x
    newYear<-x
    numNewYear<-length(newYear)
    orYear<-originalYear[1:numNewYear]
    diff<-newYear-orYear
    meanDiff<-mean(diff,na.rm=TRUE)
    offset<-round(meanDiff)
    for (i in 1:numXY) {
      w<-triCube(x-x[i],window)
      mod<-lm(xy$y~x,weights=w)
      new<-data.frame(x=x[i])
      z<-exp(predict(mod,new))
      ioffset<-i+offset
      annualSeries[3,istat,ioffset]<-z}}
  return(annualSeries)
}
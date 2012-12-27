#'  Creates the AnnualResults data frame from the Daily data frame
#'
#'   This function aggregates the results stored on a daily basis in the Daily data frame
#'   and stores the average values of these in the new data frame called AnnualResults.
#'      The "annual values" can be a full 12 months, or they can be shorter. 
#'      See manual to understand paLong and paStart arguments. 
#'      The simplest case, a Water Year (October through September), would have
#'      paLong=12, and paStart=10. 
#'      A calendar year would be paLong=12 and paStart=1. 
#'      A winter season of Dec, Jan, Feb would be paLong=3 and paStart=12
#'
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return AnnualResults data frame with one row per year
#' @export
#' @examples 
#' Daily <- exDailyEnd
#' setupYears()
setupYears<-function(paLong = 12, paStart = 10, localDaily = Daily){
  # this function aggregates the results in the data frame Daily into annual values
  # but it gives the user flexibility as to the period of analysis
  # The "annual values" can be a full 12 months, or they can be shorter
  # See manual to understand paLong and paStart arguments
  #   But, the simplest case, a Water Year would have
  #   paLong=12, and paStart=10
  # it is designed to handle NA values
  #
  #  
  numDays<-length(localDaily$MonthSeq)
  firstMonthSeq<-localDaily$MonthSeq[1]
  lastMonthSeq<-localDaily$MonthSeq[numDays]
  #   creating a data frame of starting and ending months for each year
  Starts<-seq(paStart,lastMonthSeq,12)
  Ends<-Starts+paLong-1
  StartEndSeq<-data.frame(Starts,Ends)
  #   need to trim off the front and back, those years that aren't in the Daily data set
  StartEndSeq<-subset(StartEndSeq,Starts>=firstMonthSeq)
  StartEndSeq<-subset(StartEndSeq,Ends<=lastMonthSeq)
  numYears<-length(StartEndSeq$Starts)
  DecYear<-rep(NA,numYears)
  Q<-rep(NA,numYears)
  Conc<-rep(NA,numYears)
  Flux<-rep(NA,numYears)
  FNConc<-rep(NA,numYears)
  FNFlux<-rep(NA,numYears)
  for(i in 1:numYears) {
    startSeq<-StartEndSeq$Starts[i]
    endSeq<-StartEndSeq$Ends[i]
    DailyYear<-subset(localDaily,MonthSeq>=startSeq)
    DailyYear<-subset(DailyYear,MonthSeq<=endSeq)
    #     need to see if the data frame for the year has enough good data
    counter<-ifelse(is.na(DailyYear$ConcDay),1,0)
    #     if we have NA values on more than 10% of the days, then don't use the year
    good<-if((sum(counter)/length(DailyYear$ConcDay))>0.1) FALSE else TRUE
    DecYear[i]<-mean(DailyYear$DecYear)
    Q[i]<-mean(DailyYear$Q)
    Conc[i]<-if(good) mean(DailyYear$ConcDay,na.rm=TRUE) else NA
    Flux[i]<-if(good) mean(DailyYear$FluxDay,na.rm=TRUE) else NA
    FNConc[i]<-if(good) mean(DailyYear$FNConc,na.rm=TRUE) else NA
    FNFlux[i]<-if(good) mean(DailyYear$FNFlux,na.rm=TRUE) else NA
  }
  #  create two more variables that just report paStart and paLong
  #    needed later to verify the period of analysis used in the Annual Results summary
  PeriodStart<-rep(paStart,numYears)
  PeriodLong<-rep(paLong,numYears)
  AnnualResults<-data.frame(DecYear,Q,Conc,Flux,FNConc,FNFlux,PeriodLong,PeriodStart)
  return(AnnualResults)		
}
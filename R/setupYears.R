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
#' @param localDaily data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return AnnualResults data frame with one row per year
#' @export
#' @examples 
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' AnnualResults <- setupYears(Daily, 4, 10)
#' 
#' 
setupYears<-function(localDaily, paLong = 12, paStart = 10){
  # this function aggregates the results in the data frame Daily into annual values
  # but it gives the user flexibility as to the period of analysis
  # The "annual values" can be a full 12 months, or they can be shorter
  # See manual to understand paLong and paStart arguments
  #   But, the simplest case, a Water Year would have
  #   paLong=12, and paStart=10
  # it is designed to handle NA values
  
  numDays<-length(localDaily$MonthSeq)
  firstMonthSeq<-localDaily$MonthSeq[1]
  lastMonthSeq<-localDaily$MonthSeq[numDays]
  
  #   creating a data frame of starting and ending months for each year
  Starts<-seq(paStart,lastMonthSeq,12)
  Ends<-Starts+paLong-1
  StartEndSeq<-data.frame(Starts,Ends)
  #   need to trim off the front and back, those years that aren't in the Daily data set
  StartEndSeq <- StartEndSeq[(StartEndSeq$Starts >=firstMonthSeq) & (StartEndSeq$Ends<=lastMonthSeq),]
  
  firstMonth <- StartEndSeq[1,1]
  
  numYears<-length(StartEndSeq$Starts)
  DecYear<-rep(NA,numYears)
  Q <- rep(NA,numYears)
  Conc <- rep(NA,numYears)
  Flux <- rep(NA,numYears)
  
  FNConc <- rep(NA,numYears)
  FNFlux <- rep(NA,numYears)
  
  # hasFlex <- all(c("flexConc","flexFlux") %in% names(localDaily))
  flexConc <- rep(NA, numYears)
  flexFlux <- rep(NA, numYears)
  GenConc <- rep(NA, numYears) # WRTDS_K
  GenFlux <- rep(NA, numYears) # WRTDS_K
  
  for(i in 1:numYears) {
    
    # This should be even better:
    startMonth <- (i-1)*12+firstMonth
    stopMonth <- startMonth+paLong-1
    DailyYear <- localDaily[which(localDaily$MonthSeq %in% startMonth:stopMonth),]

    # need to see if the data frame for the year has enough good data
    counter <- ifelse(is.na(DailyYear$ConcDay),0,1)

    # if we have NA values on more than 10% of the days, then don't use the year
    if (length(counter) > 0){
      good <- sum(counter) / length(counter) > 0.1
    } else {
      good <- FALSE
    }    
    
    DecYear[i] <- mean(DailyYear$DecYear)
    Q[i] <- mean(DailyYear$Q)
    if(good) {
      Conc[i] <- mean(DailyYear$ConcDay,na.rm=TRUE)
      Flux[i] <- mean(DailyYear$FluxDay,na.rm=TRUE)
      
      FNConc[i] <- mean(DailyYear$FNConc,na.rm=TRUE)
      FNFlux[i] <- mean(DailyYear$FNFlux,na.rm=TRUE)
      
      wrtdsK <- all(c("GenConc", "GenFlux") %in% names(DailyYear))
      if(wrtdsK){
        # ConcDay[i] <- mean(DailyYear$ConcDay, na.rm = TRUE)
        # FluxDay[i] <- sum(DailyYear$FluxDay, na.rm = TRUE) / 1000
        GenConc[i] <- mean(DailyYear$GenConc, na.rm = TRUE)
        GenFlux[i] <- mean(DailyYear$GenFlux, na.rm = TRUE)
      }

    }
  }
  #  create two more variables that just report paStart and paLong
  #    needed later to verify the period of analysis used in the Annual Results summary
  PeriodStart <- rep(paStart,numYears)
  PeriodLong <- rep(paLong,numYears)

  if(wrtdsK){
    AnnualResults <- data.frame(DecYear,Q,
                                Conc, Flux,
                                FNConc,FNFlux,
                                GenConc, GenFlux,
                                PeriodLong,PeriodStart)    
  } else {
    AnnualResults <- data.frame(DecYear,Q,
                                Conc,Flux,
                                FNConc,FNFlux,
                                PeriodLong,PeriodStart)    
  }


  return(AnnualResults)		
}
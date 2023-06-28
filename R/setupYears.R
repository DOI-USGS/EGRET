#'  Creates the AnnualResults data frame from the Daily data frame
#'
#'   This function aggregates the results stored on a daily basis in the Daily data frame
#'   and stores the average values of these in the new data frame called AnnualResults.
#'      Note that the flux values are rates (kg/day) and not a mass (kg).
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
#' @return A data frame 'AnnualResults' of numeric values with the following columns
#' \tabular{lll}{
#' Name \tab Description \cr
#' DecYear \tab Middle of the period in decimal years\cr
#' Q \tab Mean discharge, in m^3/s\cr
#' Conc \tab Estimated mean concentration, in mg/L\cr
#' Flux \tab Estimated mean flux, in kg/day\cr
#' FNConc \tab Flow-normalized concentration, in mg/L\cr
#' FNFlux \tab Flow-normalized flux, in kg/day\cr
#' GenConc \tab Generalized mean concentration, in mg/L. This column is only returned if the WRTDSKalman function was run, which gives the 
#' eList$Daily data frame a column "GenConc".\cr
#' GenFlux \tab Generalized mean flux, in kg/day. This column is only returned if the WRTDSKalman function was run, which gives the 
#' eList$Daily data frame a column "GenFlux".\cr
#' PeriodLong \tab Length of period of analysis (paLong), in months\cr
#' PeriodStart \tab Starting month of period of analysis (paStart), in months (1 = January)\cr
#' } 
#' @export
#' @examples 
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' AnnualResults <- setupYears(Daily, 4, 10)
#' 
#' 
setupYears <- function(localDaily, paLong = 12, paStart = 10) {
  # this function aggregates the results in the data frame Daily into annual values
  # but it gives the user flexibility as to the period of analysis
  # The "annual values" can be a full 12 months, or they can be shorter
  # See manual to understand paLong and paStart arguments
  #   But, the simplest case, a Water Year would have
  #   paLong=12, and paStart=10
  # it is designed to handle NA values
  
  numDays <- length(localDaily$MonthSeq)
  
  complete <- numDays - 1 == as.numeric(max(localDaily$Date, na.rm = TRUE) -
                                          min(localDaily$Date, na.rm = TRUE))
  
  if(!complete){
    stop("Daily dataframe cannot have gaps in the data.")
  }
  
  firstMonthSeq <- localDaily$MonthSeq[1]
  lastMonthSeq <- localDaily$MonthSeq[numDays]
  
  #   creating a data frame of starting and ending months for each year
  Starts <- seq(paStart, lastMonthSeq, 12)
  Ends <- Starts + paLong-1
  StartEndSeq <- data.frame(Starts,Ends)
  #   need to trim off the front and back, those years that aren't in the Daily data set
  withinIndex <- which((StartEndSeq$Starts >= firstMonthSeq) & (StartEndSeq$Ends <= lastMonthSeq))
  StartEndSeq <- StartEndSeq[withinIndex, ]
  
  firstMonth <- StartEndSeq[1,1]
  
  numYears <- nrow(StartEndSeq)

  DecYear <- rep(NA,numYears)
  Q <- rep(NA,numYears)
  Conc <- rep(NA,numYears)
  Flux <- rep(NA,numYears)
  
  FNConc <- rep(NA,numYears)
  FNFlux <- rep(NA,numYears)

  flexConc <- rep(NA, numYears)
  flexFlux <- rep(NA, numYears)
  GenConc <- rep(NA, numYears) # WRTDS_K
  GenFlux <- rep(NA, numYears) # WRTDS_K
  
  daysInMonths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  wrtdsK <- all(c("GenConc", "GenFlux") %in% names(localDaily))
  
  for(i in 1:numYears) {
    
    startMonth <- StartEndSeq$Starts[i]
    stopMonth <- StartEndSeq$End[i]
    
    firstDay_i <- localDaily$Date[localDaily$MonthSeq == startMonth][1]
    lastMonth <- localDaily$Month[localDaily$MonthSeq == stopMonth][1]
    lastDay <- daysInMonths[lastMonth]
    lastYear <- floor(localDaily$DecYear[localDaily$MonthSeq == stopMonth][1])
    
    if(lastMonth == 2 &
       (lastYear %% 4 == 0) & ((lastYear %% 100 != 0) | (lastYear%%400 == 0))){ #leap
      lastDay <- 29
    }
    
    lastDate <- as.Date(paste(lastYear, lastMonth, lastDay, sep = "-"))
    
    numDaysInYear <- as.numeric(lastDate - firstDay_i + 1)

    DailyYear <- localDaily[which(localDaily$MonthSeq %in% startMonth:stopMonth),]

    if(nrow(DailyYear) == 0){
      next
    }
    # need to see if the data frame for the year has enough good data
    counter <- ifelse(is.na(DailyYear$ConcDay),0,1)

    # if we have NA values on more than 10% of the days, then don't use the year
    if (length(counter) > 0){
      good <- sum(counter) / numDaysInYear > 0.99
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
      
      if(wrtdsK){
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

  AnnualResults <- AnnualResults[!is.na(AnnualResults$DecYear),]

  # AnnualResults <- na.omit(AnnualResults)
  
  return(AnnualResults)		
}
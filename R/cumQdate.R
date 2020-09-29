#' Cumulative flow calculation
#' 
#' This function computes the first day of the calendar year
#' at which a specific fraction of the cumulative flow for that year
#' has been exceeded.  Typically one looks for the point where
#' half the cumulative flow has happened (fract = 0.5)
#' the portion of the year being considered are set by paStart and paLong
#' the matrix returned has 2 columns: 
#' the first is the year (integer when the period of analysis ends),
#' the second is the day of the year when the fraction has been exceeded.
#' None of the rows will have any NA values. 
#' 
#' @export
#' @param eList named list with at least the Sample and INFO dataframes
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1 <= paLong <= 12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1 <= paStart <= 12, default is 10 
#' @param fract numeric fraction of the flow
#' @examples 
#' eList <- Choptank_eList
#' annualFlow <- cumQdate(eList)
#' head(annualFlow)
#' plot(annualFlow)
#' mod1 <- lm(annualFlow[,2] ~ annualFlow[,1])
#' summary(mod1)
cumQdate <- function(eList, 
                     paStart = 10, paLong = 12, 
                     fract = 0.5){

  localDaily <- getDaily(eList)
  numDays <- length(localDaily$DecYear)
  yearFirst <- trunc(localDaily$DecYear[1])
  yearLast <- trunc(localDaily$DecYear[numDays])
  monthSeqFirst <- localDaily$MonthSeq[1]
  monthSeqLast <- localDaily$MonthSeq[numDays]
  numYears <- yearLast - yearFirst + 2
  annualSeries <- rep(NA, 2 * numYears)
  dim(annualSeries) <- c(numYears, 2)
  paStartLow <- paStart
  Starts <- seq(paStartLow, monthSeqLast, 12)
  Ends <- Starts + paLong - 1
  startEndSeq <- data.frame(Starts, Ends)
  startEndSeq <- subset(startEndSeq, (Ends >= monthSeqFirst) & 
                          (Starts <= monthSeqLast))
  numYSeq <- length(startEndSeq$Ends)
  for (i in 1:numYSeq) {
    startSeq <- startEndSeq$Starts[i]
    endSeq <- startEndSeq$Ends[i]
    yearDaily <- localDaily[localDaily$MonthSeq >= startSeq & 
                              (localDaily$MonthSeq <= endSeq), ]
    goodDay <- length(yearDaily$Q) - sum(is.na(yearDaily$Q))
    if (goodDay > 26 * paLong) {
      x <- cumsum(yearDaily$Q)
      totalX <- x[goodDay]
      threshold <- fract * totalX
      j <- min(which(x > threshold))
      annualSeries[i,1] <- trunc(yearDaily$DecYear[goodDay])
      annualSeries[i,2] <- yearDaily$Day[j]
    }  
  }
  
  annualSeries <- na.omit(annualSeries)
  return(annualSeries)
}
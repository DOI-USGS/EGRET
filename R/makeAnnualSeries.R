#' Produces annual series of 8 streamflow statistics (and a lowess smooth of them) from daily streamflow data
#'
#' Part of the flowHistory system.  The data come from Daily and INFO data frames. 
#' Note that the function setPA must be run before this to establish the period of analysis (e.g. water year).
#'
#' \tabular{ll}{
#' istat  \tab Name  \cr
#' 1 \tab minimum 1-day daily mean discharge \cr 
#' 2 \tab minimum 7-day mean of the daily mean discharges \cr 
#' 3 \tab minimum 30-day mean of the daily mean discharges \cr 
#' 4 \tab median of the daily mean discharges \cr 
#' 5 \tab mean of the daily mean discharges \cr 
#' 6 \tab maximum 30-day mean of the daily mean discharges \cr 
#' 7 \tab maximum 7-day mean of the daily mean discharges \cr 
#' 8 \tab  maximum 1-day daily mean discharge \cr  
#' }
#'
#' @param eList named list with at least Daily and INFO dataframes
#' @param edgeAdjust logical specifying whether to use the modified method for 
#' calculating the windows at the edge of the record.  The modified method tends to 
#' reduce curvature near the start and end of record.  
#' Default is TRUE, but a logical in INFO$edgeAdjust will override the default.
#' @keywords statistics streamflow trends
#' @export
#' @return annualSeries matrix that contains the annual series of streamflow statistics
#' @examples 
#' eList <- Choptank_eList
#' annualSeries <- makeAnnualSeries(eList)
makeAnnualSeries<-function(eList, edgeAdjust = TRUE) {
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  if (all(c("paStart", "paLong", "window") %in% names(localINFO))) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
    window <- localINFO$window
  } else {
    paLong <- 12
    paStart <- 10
    window <- 20
  }
  if ("edgeAdjust" %in% names(localINFO)) {
    edgeAdjust <- localINFO$edgeAdjust
  }
  numDays <- length(localDaily$DecYear)
  yearFirst <- trunc(localDaily$DecYear[1])
  yearLast <- trunc(localDaily$DecYear[numDays])
  
  monthSeqFirst <- localDaily$MonthSeq[1]
  monthSeqLast <- localDaily$MonthSeq[numDays]
  
  numYears <- yearLast - yearFirst + 2
  annualSeries <- rep(NA, 3 * 8 * numYears)
  dim(annualSeries) <- c(3, 8, numYears)
  paStartLow <- if (paLong == 12 & paStart == 10) 4 else paStart
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
      annualSeries[1, 1:3, i] <- mean(yearDaily$DecYear, 
                                      na.rm = TRUE)
      annualSeries[2, 1, i] <- min(yearDaily$Q, na.rm = TRUE)
      annualSeries[2, 2, i] <- min(yearDaily$Q7, na.rm = TRUE)
      annualSeries[2, 3, i] <- min(yearDaily$Q30, na.rm = TRUE)
    }
  }
  
  Starts <- seq(paStart, monthSeqLast, 12)
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
      annualSeries[1, 4:8, i] <- mean(yearDaily$DecYear, 
                                      na.rm = TRUE)
      annualSeries[2, 4, i] <- median(yearDaily$Q, na.rm = TRUE)
      annualSeries[2, 5, i] <- mean(yearDaily$Q, na.rm = TRUE)
      annualSeries[2, 6, i] <- max(yearDaily$Q30, na.rm = TRUE)
      annualSeries[2, 7, i] <- max(yearDaily$Q7, na.rm = TRUE)
      annualSeries[2, 8, i] <- max(yearDaily$Q, na.rm = TRUE)
    }
  }
  for (istat in 1:8) {
    x <- annualSeries[1, istat, ]
    y <- log(annualSeries[2, istat, ])
    baseYear <- trunc(x[1])
    numYears <- length(x)
    xVec <- seq(1,numYears)
    xy <- data.frame(x,y,xVec)
    xy <- na.omit(xy)
    goodYears <- length(xy$x)
    x <- xy$x
    x1 <- x[1]
    xn <- x[goodYears]
    for (i in 1:goodYears) {
      xi <- x[i]
      distToEdge <- min((xi - x1), (xn - xi))
      close <- (distToEdge < window)
      thisWindow <- if (edgeAdjust & close) {
        (2 * window) - distToEdge
      } else {
        window
      }
      w <- triCube(x - xi, thisWindow)
      mod <- lm(xy$y ~ x, weights = w)
      new <- data.frame(x = x[i])
      z <- exp(predict(mod, new))
      iYear <- xy$xVec[i]
      annualSeries[3, istat, iYear] <- z
    }
  }
  return(annualSeries)
}
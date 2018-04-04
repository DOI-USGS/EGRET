#' stitch
#' 
#' This function creates a continuous surfaces object that starts just before 
#' surfaceStart and ends just after surfaceEnd.
#' 
#' It has a wall in it and the wall comes just after lastDaySample1
#' Arguments 2 through 5 are all dates, but just to make sure we will do a conversion to as.Date
#' the only use being made of eList$Daily is to establish the minimum and maximum discharges for the surfaces
#' the eList$Sample will be split out into Sample1 and Sample2 (there will be no overlap)
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param surfaceStart The Date (or character in YYYY-MM-DD) that is the start of the WRTDS model to be estimated and the last of the 
#' daily outputs to be generated (if NA it is sampleStartDate)
#' @param surfaceEnd The Date (or character in YYYY-MM-DD) that is the end of the WRTDS model to be estimated and the last of the daily outputs 
#' to be generated (if NA it is sampleEndDate)
#' @param sample1StartDate The Date (or character in YYYY-MM-DD) of the first sample to be used (if NA it is the first Date in eList$Sample)
#' @param sample1EndDate The Date (or character in YYYY-MM-DD) of the last sample to be used in the first section
#' @param sample2StartDate The Date (or character in YYYY-MM-DD) of the first sample to be used in the second section
#' @param sample2EndDate The Date (or character in YYYY-MM-DD) of the last sample to be used in the second section
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param verbose logical specifying whether or not to display progress message
#' @param run.parallel logical to run bootstrapping in parallel or not
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @export
#' @examples 
#' eList <- Choptank_eList
#' 
#' surfaceStart <- "1986-10-01"
#' surfaceEnd <- "2010-09-30"
#' 
#' # Surface skips a few years:
#' sample1StartDate <- "1986-10-01"
#' sample1EndDate <- "1992-09-30"
#' sample2StartDate <- "1996-10-01"
#' sample2EndDate <- "2011-09-30"
#' 
#' \dontrun{
#' surface_skip <- stitch(eList, 
#'                          sample1StartDate, sample1EndDate, 
#'                          sample2StartDate, sample2EndDate,
#'                          surfaceStart, surfaceEnd)
#' 
#' # Surface overlaps a few years:
#' sample1StartDate <- "1986-10-01"
#' sample1EndDate <- "1996-09-30"
#' sample2StartDate <- "1992-10-01"
#' sample2EndDate <- "2011-09-30"
#' 
#' surface_overlap <- stitch(eList, 
#'                          sample1StartDate, sample1EndDate, 
#'                          sample2StartDate, sample2EndDate)
#' }
stitch <- function(eList, 
                   sample1StartDate, sample1EndDate, 
                   sample2StartDate, sample2EndDate, 
                   surfaceStart = NA, surfaceEnd = NA, 
                   minNumObs = 100, minNumUncen = 50, 
                   windowY = 7, windowQ = 2, windowS = 0.5, 
                   edgeAdjust = TRUE, verbose = FALSE, run.parallel = FALSE){

  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  surfaceIndexParameters <- surfaceIndex(localDaily)
  bottomLogQ <- surfaceIndexParameters[["bottomLogQ"]]
  stepLogQ <- surfaceIndexParameters[["stepLogQ"]]
  nVectorLogQ <- surfaceIndexParameters[["nVectorLogQ"]]
  bottomYear <- surfaceIndexParameters[["bottomYear"]]
  stepYear <- surfaceIndexParameters[["stepYear"]]
  nVectorYear <- surfaceIndexParameters[["nVectorYear"]]
  vectorYear <- surfaceIndexParameters[["vectorYear"]]
  vectorLogQ <- surfaceIndexParameters[["vectorLogQ"]]
  
  Sample1 <- localSample[localSample$Date >= sample1StartDate & 
                           localSample$Date <= sample1EndDate, ]
  Sample2 <- localSample[localSample$Date >= sample2StartDate & 
                           localSample$Date <= sample2EndDate, ]
  
  message("\n Sample1 mean concentration ", mean(Sample1$ConcAve))
  message("\n Sample2 mean concentration ", mean(Sample2$ConcAve))
  
  minNumObs <- ceiling(min(minNumObs, 0.8 * length(Sample1$Date), 
                           0.8 * length(Sample2$Date), na.rm = TRUE))
  minNumUncen <- ceiling(min(0.5 * minNumObs, 0.8 * sum(Sample1$Uncen), 
                             0.8 * sum(Sample2$Uncen), na.rm = TRUE))
  message("Sample1 has ", nrow(Sample1), " Samples and ", sum(Sample1$Uncen), 
          " are uncensored")
  message("Sample2 has ", nrow(Sample2), " Samples and ", sum(Sample2$Uncen), 
          " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", 
          minNumUncen)
  surfaceStart <- as.Date(surfaceStart)
  if (is.na(surfaceStart)) {
    surfaceStart <- Sample1$Date[1]
  }
  surfaceEnd <- as.Date(surfaceEnd)
  if (is.na(surfaceEnd)) {
    surfaceEnd <- Sample2$Date[nrow(Sample2)]
  }
  surface1End <- as.Date(sample1EndDate) + 1
  surface2Start <- as.Date(sample2StartDate) - 1
  
  highLow <- decimalHighLow(Sample1)
  DecHigh <- highLow[["DecHigh"]]
  DecLow <- highLow[["DecLow"]]
  sliceIndex <- which(vectorYear >= decimalDate(surfaceStart) & 
                        vectorYear <= decimalDate(surface1End))
  Year <- vectorYear[c(sliceIndex[1] - 1, sliceIndex, tail(sliceIndex, n = 1) + 1)]
  nVectorYear <- length(Year)
  Year1 <- Year
  estPtYear <- rep(Year, each = 14)
  estPtLogQ <- rep(vectorLogQ, nVectorYear)
  resultSurvReg <- runSurvReg(estPtYear, estPtLogQ, DecLow, 
                              DecHigh, Sample1, windowY, windowQ, windowS, minNumObs, 
                              minNumUncen, edgeAdjust = edgeAdjust, verbose = verbose, 
                              run.parallel = run.parallel)
  surfaces1 <- array(0, dim = c(14, nVectorYear, 3))
  for (iQ in 1:14) {
    for (iY in 1:nVectorYear) {
      k <- (iY - 1) * 14 + iQ
      surfaces1[iQ, iY, ] <- resultSurvReg[k, ]
    }
  }
  attr(surfaces1, "LogQ") <- vectorLogQ
  attr(surfaces1, "Year") <- Year
  highLow <- decimalHighLow(Sample2)
  DecHigh <- highLow[["DecHigh"]]
  DecLow <- highLow[["DecLow"]]
  sliceIndex <- which(vectorYear >= decimalDate(surface2Start) & 
                        vectorYear <= decimalDate(surfaceEnd))
  Year <- vectorYear[c(sliceIndex[1] - 1, sliceIndex, tail(sliceIndex, n = 1) + 1)]
  nVectorYear <- length(Year)
  Year2 <- Year
  estPtYear <- rep(Year, each = 14)
  estPtLogQ <- rep(vectorLogQ, nVectorYear)
  resultSurvReg <- runSurvReg(estPtYear, estPtLogQ, DecLow, 
                              DecHigh, Sample2, windowY, windowQ, windowS, minNumObs, 
                              minNumUncen, edgeAdjust = edgeAdjust, verbose = verbose, 
                              run.parallel = run.parallel)
  surfaces2 <- array(0, dim = c(14, nVectorYear, 3))
  for (iQ in 1:14) {
    for (iY in 1:nVectorYear) {
      k <- (iY - 1) * 14 + iQ
      surfaces2[iQ, iY, ] <- resultSurvReg[k, ]
    }
  }
  attr(surfaces2, "LogQ") <- vectorLogQ
  attr(surfaces2, "Year") <- Year
  if (!(any(Year1 %in% Year2) | any(Year2 %in% Year1))) {
    surfaceTotal <- array(c(surfaces1, surfaces2), dim = c(14, length(Year1) + length(Year2), 3))
    vectorYear <- c(Year1, Year2)
  }
  else {
    surfaces1Unique <- surfaces1[1:14, which(!(Year1 %in% Year2)), 1:3]
    surfaces2Unique <- surfaces2[1:14, which(!(Year2 %in% Year1)), 1:3]
    surfaces1Slice <- surfaces1[1:14, which((Year1 %in% Year2)), 1:3]
    surfaces2Slice <- surfaces2[1:14, which((Year2 %in% Year1)), 1:3]
    surfacesMean <- (surfaces1Slice + surfaces2Slice)/2
    surfacesMean[, , 3] <- exp((surfacesMean[, , 2]^2)/2) * exp(surfacesMean[, , 1])
    YearStart <- Year1[1]
    YearEnd <- Year2[length(Year2)]
    vectorYear <- seq(YearStart, YearEnd, stepYear)
    nVectorYear <- length(vectorYear)
    bottomYear <- YearStart
    surfaceTotal <- array(0, dim = c(14, nVectorYear, 3))
    for (i in 1:14) {
      for (j in 1:3) {
        surfaceTotal[i, , j] <- c(surfaces1Unique[i,  , j], surfacesMean[i, , j], surfaces2Unique[i, 
                                                                                              , j])
      }
    }
  }
  attr(surfaceTotal, "surfaceIndex")[["bottomLogQ"]] <- vectorLogQ[1]
  attr(surfaceTotal, "surfaceIndex")[["stepLogQ"]] <- vectorLogQ[2] - vectorLogQ[1]
  attr(surfaceTotal, "surfaceIndex")[["nVectorLogQ"]] <- length(vectorLogQ)
  attr(surfaceTotal, "surfaceIndex")[["bottomYear"]] <- vectorYear[1]
  attr(surfaceTotal, "surfaceIndex")[["stepYear"]] <- vectorYear[2] - vectorYear[1]
  attr(surfaceTotal, "surfaceIndex")[["nVectorYear"]] <- length(vectorYear)
  attr(surfaceTotal, "Year") <- vectorYear
  attr(surfaceTotal, "LogQ") <- vectorLogQ
  attr(surfaceTotal, "surfaceStart") <- surfaceStart
  attr(surfaceTotal, "surfaceEnd") <- surfaceEnd
  attr(surfaceTotal, "sample1StartDate") <- sample1StartDate
  attr(surfaceTotal, "sample1EndDate") <- sample1EndDate
  attr(surfaceTotal, "sample2StartDate") <- sample2StartDate
  attr(surfaceTotal, "sample2EndDate") <- sample2StartDate
  return(surfaceTotal)
 
}

#' decimalHighLow
#' 
#' decimalHighLow
#' @param df data.frame with Date, DecYear, and Month columns
#' @return list with DecHigh and DecLow (water year high/low decimal values)
#' @export
#' @examples 
#' eList <- Choptank_Phos
#' highLow <- decimalHighLow(eList$Sample)
#' 
#' DecHigh <- highLow[["DecHigh"]]
#' DecLow <- highLow[["DecLow"]]
decimalHighLow <- function(df){
  
  n <- length(df$Date)
  DecLow <- trunc(df$DecYear[1]) - 0.25
  DecHigh <- trunc(df$DecYear[n]) + 0.75
  
  if(df$Month[1] %in% c(10:12)){
    DecLow <- DecLow + 1
  } 
  
  if(df$Month[n] %in% c(10:12)){
    DecHigh <- DecHigh + 1
  }
  
  return(list(DecHigh = DecHigh, DecLow = DecLow))
}
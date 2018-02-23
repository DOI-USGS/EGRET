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
#' @param surfaceStart character (or Date) in YYYY-MM-DD. Starting date of final surface.
#' If NA, surface starts at beginning of Sample
#' @param surfaceEnd character (or Date) in YYYY-MM-DD. Ending date of final surface
#' If NA, surface ends at end of Sample
#' @param firstSampleDate1 character in YYYY-MM-DD. Starting data of Sample data for first surface to be joined.
#' @param lastSampleDate1 character in YYYY-MM-DD. Ending data of Sample data for first surface to be joined.
#' @param firstSampleDate2 character in YYYY-MM-DD. Starting data of Sample data for second surface to be joined.
#' @param lastSampleDate2 character in YYYY-MM-DD. Ending data of Sample data for second surface to be joined.
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param run.parallel logical to run bootstrapping in parallel or not
#' @param verbose logical specifying whether or not to display progress message
#' 
#' @export
#' @examples 
#' eList <- Choptank_Phos
#' 
#' surfaceStart <- "1986-10-01"
#' surfaceEnd <- "2012-09-30"
#' 
#' # Surface skips a few years:
#' firstSampleDate1 <- "1986-10-01"
#' lastSampleDate1 <- "1992-09-30"
#' firstSampleDate2 <- "1996-10-01"
#' lastSampleDate2 <- "2012-09-30"
#' 
#' \dontrun{
#' surface_skip <- stitch(eList, 
#'                          surfaceStart, surfaceEnd, 
#'                          firstSampleDate1, lastSampleDate1, 
#'                          firstSampleDate2, lastSampleDate2)
#'    
#' eList_skip <- as.egret(eList$INFO, 
#'                        eList$Daily, 
#'                        eList$Sample, 
#'                        surface_skip)
#' eList_skip <- modelEstimation(eList_skip)
#' plotConcHist(eList_skip)
#' 
#' # Surface overlaps a few years:
#' firstSampleDate1 <- "1986-10-01"
#' lastSampleDate1 <- "1996-09-30"
#' firstSampleDate2 <- "1992-10-01"
#' lastSampleDate2 <- "2012-09-30"
#' 
#' surface_overlap <- stitch(eList, 
#'                          surfaceStart, surfaceEnd, 
#'                          firstSampleDate1, lastSampleDate1, 
#'                          firstSampleDate2, lastSampleDate2)
#' }
stitch <- function(eList, 
                   firstSampleDate1, lastSampleDate1, 
                   firstSampleDate2, lastSampleDate2, 
                   surfaceStart = NA, surfaceEnd = NA, 
                   minNumObs = 100, minNumUncen = 50,
                   windowY = 7, windowQ = 2, windowS = 0.5,
                   edgeAdjust = TRUE, verbose = FALSE, run.parallel = FALSE){


  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  # now we will establish the Q dimension of the surfaces
  
  surfaceIndexParameters <- surfaceIndex(localDaily)
  
  bottomLogQ <- surfaceIndexParameters[['bottomLogQ']]
  stepLogQ <- surfaceIndexParameters[['stepLogQ']]
  nVectorLogQ <- surfaceIndexParameters[['nVectorLogQ']]
  bottomYear <- surfaceIndexParameters[['bottomYear']]
  stepYear <- surfaceIndexParameters[['stepYear']]
  nVectorYear <- surfaceIndexParameters[['nVectorYear']]
  vectorYear <- surfaceIndexParameters[['vectorYear']]
  vectorLogQ <- surfaceIndexParameters[['vectorLogQ']]

  # now we split out the two samples
  Sample1 <- localSample[localSample$Date >= firstSampleDate1 &
                           localSample$Date <= lastSampleDate1,]
  Sample2 <- localSample[localSample$Date >=  firstSampleDate2 &
                           localSample$Date <= lastSampleDate2,]
  
  message("\n Sample1 mean concentration ", mean(Sample1$ConcAve))
  message("\n Sample2 mean concentration ", mean(Sample2$ConcAve))
  # need to check on minNumObs and adjust if needed
  minNumObs <- ceiling(min(minNumObs, 0.8 * length(Sample1$Date), 0.8 * length(Sample2$Date), na.rm = TRUE))
  minNumUncen <- ceiling(min(0.5 * minNumObs, 0.8 * sum(Sample1$Uncen), 0.8 * sum(Sample2$Uncen), na.rm = TRUE))
  
  message("Sample1 has ", nrow(Sample1), " Samples and ", sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", nrow(Sample2), " Samples and ", sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", minNumUncen)
  
  ############
  # start in on creating surfaces1
  surfaceStart <- as.Date(surfaceStart)
  if(is.na(surfaceStart)){
    surfaceStart <- Sample1$Date[1]
  }
  
  surfaceEnd <- as.Date(surfaceEnd)
  if(is.na(surfaceEnd)){
    surfaceEnd <- Sample2$Date[nrow(Sample2)]
  }
  
  DecLow <- decimalDate(surfaceStart)
  DecHigh <- decimalDate(as.Date(lastSampleDate1))
  
  sliceIndex <- which(vectorYear >= DecLow & vectorYear <= DecHigh)
  Year <- vectorYear[c(sliceIndex[1] - 1, 
                       sliceIndex, 
                       tail(sliceIndex, n = 1) + 1)]
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

  #############################################
  # now create surfaces2
  DecLow <- decimalDate(as.Date(firstSampleDate2))
  DecHigh <- decimalDate(surfaceEnd)
  
  sliceIndex <- which(vectorYear >= DecLow & vectorYear <= DecHigh)
  Year <- vectorYear[c(sliceIndex[1] - 1, 
                       sliceIndex, 
                       tail(sliceIndex,n = 1) + 1)]
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

  ######## done with second surface ##########
  #  Now stitch them together
  
  if(!(any(Year1 %in% Year2) | any(Year2 %in% Year1))){
    
    surfaceTotal <- array(c(surfaces1, surfaces2), dim = c(14,length(Year1) + length(Year2),3))
    vectorYear <- c(Year1, Year2)
    
  } else {
    
    surfaces1Unique <- surfaces1[1:14,which(!(Year1 %in% Year2)),1:3]
    #      cat("\nsurfaces1Unique ", surfaces1Unique[7,,3],"\n")
    
    surfaces2Unique <- surfaces2[1:14,which(!(Year2 %in% Year1)),1:3]

    surfaces1Slice <- surfaces1[1:14,which((Year1 %in% Year2)),1:3]
    surfaces2Slice <- surfaces2[1:14,which((Year2 %in% Year1)),1:3]

    surfacesMean <- (surfaces1Slice + surfaces2Slice)/2

    surfacesMean[,,3] <- exp((surfacesMean[,,2]^2)/2) * exp(surfacesMean[,,1])

    # year dimensions of surfaceTotal
    YearStart <- Year1[1]
    YearEnd <- Year2[length(Year2)]
    vectorYear <- seq(YearStart, YearEnd, stepYear)
    nVectorYear <- length(vectorYear)

    bottomYear <- YearStart
    surfaceTotal <- array(0,dim=c(14,nVectorYear,3))

    # populate surfaceTotal
    for(i in 1: 14) { 
      for(j in 1: 3) {
      surfaceTotal[i, ,j] <- c(surfaces1Unique[i, ,j], surfacesMean[i, ,j], surfaces2Unique[i, ,j])
      }
    }
  }

  attr(surfaceTotal, "Year") <- vectorYear
  attr(surfaceTotal, "LogQ") <- vectorLogQ

  attr(surfaceTotal, "surfaceStart") <- surfaceStart
  attr(surfaceTotal, "surfaceEnd") <- surfaceEnd
  attr(surfaceTotal, "firstSampleDate1") <- firstSampleDate1
  attr(surfaceTotal, "lastSampleDate1") <- lastSampleDate1
  attr(surfaceTotal, "firstSampleDate2") <- firstSampleDate2
  attr(surfaceTotal, "lastSampleDate2") <- lastSampleDate2

  return(surfaceTotal)
}
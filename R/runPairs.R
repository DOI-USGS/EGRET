#' runPairs
#' 
#' runPairs description
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param year1 integer the ending year of the first year in pairs
#' @param year2 integer the ending year of the second year in pairs
#' @param windowSide integer The width of the flow normalization window on each side of the year being estimated.
#' @param flowBreak logical, is there an abrupt break in the QD
#' @param Q1EndDate The Date just before the flowBreak (or character in YYY-MM-DD format)
#' @param QStartDate The first Date used in the QD (if NA, which is default, it is first Date in eList$Daily)
#' @param QEndDate The last Date used in the QD (if NA, which is default, it is the last Date in eList$Daily)
#' @param wall logical, there is an abrupt break in the CQR
#' @param sample1EndDate The Date of just before the wall
#' @param sampleStartDate The Date of the first sample to be used (if NA, which is default, it is the first Date in eList$Sample)
#' @param sampleEndDate The Date of the last sample to be used (if NA, which is default, it is the last Date in eList$Sample)
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' 
#' @examples 
#' eList <- Choptank_Phos
#' year1 <- 1985
#' year2 <- 2014
#' 
#' \dontrun{
#' # Automatic calculations based on windowSide=7
#' 
#' #Option 1:
#' pairOut_1 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' # Option 2:
#' pairOut_2 <- runPairs(eList, year1, year2, windowSide = 7)
#' 
#' # Option 3:
#' pairOut_3 <- runPairs(eList, year1, year2, 
#'                       windowSide = 0, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#' 
#' # Option 4:
#' pairOut_4 <- runPairs(eList, year1, year2, 
#'                       windowSide = 7, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#' 
#' }
runPairs <- function(eList, year1, year2, windowSide, 
                     flowBreak = FALSE,
                     Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                     wall = FALSE, 
                     sample1EndDate = NA, sampleStartDate = NA, sampleEndDate = NA,
                     paStart = 10, paLong = 12,
                     minNumObs = 100, minNumUncen = 50, 
                     windowY = 7, windowQ = 2, windowS = 0.5, 
                     edgeAdjust = TRUE){
  
  localSample <- getSample(eList)
  sampleStartDate <- if(is.na(sampleStartDate)) localSample$Date[1] else as.Date(sampleStartDate)
  numSamples <- length(localSample$Date)
  sampleEndDate <- if(is.na(sampleEndDate)) localSample$Date[numSamples] else as.Date(sampleEndDate)
  localDaily <- getDaily(eList)
  QStartDate <- if(is.na(QStartDate)) localDaily$Date[1] else as.Date(QStartDate)
  numQDays <- length(localDaily$Date)
  QEndDate <- if(is.na(QEndDate)) localDaily$Date[numQDays] else as.Date(QEndDate)
  startEndSurface1 <- startEnd(paStart, paLong, year1)
  startEndSurface2 <- startEnd(paStart, paLong, year2)
  if(flowBreak & is.na(Q1EndDate)) stop("if there is a flowBreak you must provide Q1EndDate")
  # setting up the two flow windows
  # there are four cases
  flowNormStartCol <- "flowNormStart"
  flowNormEndCol <- "flowNormEnd"
  flowStartCol <- "flowStart"
  flowEndCol <- "flowEnd"
  dateInfo <- if (windowSide <= 0 & !flowBreak) {
    option <- 1
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(QStartDate, QStartDate)
    flowNormEnd <- c(QEndDate, QEndDate)
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else if (windowSide > 0 & !flowBreak) {
    option <- 2
    dateInfo1 <- makeDateInfo(windowSide, startEndSurface1[["startDate"]], startEndSurface1[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo2 <- makeDateInfo(windowSide, startEndSurface2[["startDate"]], startEndSurface2[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  } else if (windowSide <= 0 & flowBreak) {
    option <- 3
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- Q1EndDate + 1
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(as.Date(QStartDate), as.Date(Q2StartDate))
    flowNormEnd <- c(as.Date(Q1EndDate), as.Date(QEndDate))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else {
    option <- 4
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- Q1EndDate + 1
    dateInfo1 <- makeDateInfo(windowSide, startEndSurface1[["startDate"]], startEndSurface1[["endDate"]], 
                                 QStartDate, Q1EndDate)
    dateInfo2 <- makeDateInfo(windowSide, startEndSurface2[["startDate"]], startEndSurface2[["endDate"]],
                                 Q2StartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  }
  #
  #   end of flow normalization 
  #
  
  if (wall) {
    if (is.na(sample1EndDate)) {
      stop("When using the wall option, please specify sample1EndDate")
    }
    sample1EndDate <- as.Date(sample1EndDate)
    sample2StartDate <- as.Date(sample1EndDate) + 1
    sample1StartDate <- as.Date(sampleStartDate)
    sample2EndDate <- as.Date(sampleEndDate)
  } else {
    sample1StartDate <- as.Date(sampleStartDate)
    sample2StartDate <- as.Date(sampleStartDate)
    sample1EndDate <- as.Date(sampleEndDate)
    sample2EndDate <- as.Date(sampleEndDate)
  }
  Sample1 <- localSample[localSample$Date >= sample1StartDate & 
                           localSample$Date <= sample1EndDate, ]
  Sample2 <- localSample[localSample$Date >= sample2StartDate & 
                           localSample$Date <= sample2EndDate, ]
  minNumObs <- ceiling(min(minNumObs, 0.8 * length(Sample1$Date), 
                           0.8 * length(Sample2$Date)))
  minNumUncen <- ceiling(min(0.5 * minNumObs, 0.8 * sum(Sample1$Uncen), 
                             0.8 * sum(Sample2$Uncen)))
  message("Sample1 has ", length(Sample1$Date), " Samples and ", 
          sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", length(Sample2$Date), " Samples and ", 
          sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", 
          minNumUncen)
  Daily1 <- localDaily[localDaily$Date >= dateInfo$flowNormStart[1] & localDaily$Date <= 
                         dateInfo$flowNormEnd[1], ]
  Daily2 <- localDaily[localDaily$Date >= dateInfo$flowNormStart[2] & localDaily$Date <= 
                         dateInfo$flowNormEnd[2], ]
  surfaces1 <- estSurfaces(eList, 
                           surfaceStart = startEndSurface1[["startDate"]],
                           surfaceEnd = startEndSurface1[["endDate"]], 
                           localSample = Sample1,
                           minNumObs = minNumObs, minNumUncen = minNumUncen, 
                           windowY = windowY, windowQ = windowQ, windowS = windowS, 
                           edgeAdjust = edgeAdjust, verbose = FALSE)
  surfaces2 <- estSurfaces(eList, 
                           surfaceStart = startEndSurface2[["startDate"]],
                           surfaceEnd = startEndSurface2[["endDate"]], 
                           localSample = Sample2,
                           minNumObs = minNumObs, minNumUncen = minNumUncen, 
                           windowY = windowY, windowQ = windowQ, windowS = windowS,
                           edgeAdjust = edgeAdjust, verbose = FALSE)
  DailyRS1FD1 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, 
                                      localDaily = Daily1)
  annualFlex <- setupYears(DailyRS1FD1, paLong = paLong, paStart = paStart)
  c11 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f11 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  DailyRS2FD2 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, 
                                      localDaily = Daily2)
  annualFlex <- setupYears(DailyRS2FD2, paLong = paLong, paStart = paStart)
  c22 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f22 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  Daily0 <- localDaily[localDaily$Date >= QStartDate & localDaily$Date <= 
                         QEndDate, ]
  DailyRS1FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, 
                                      localDaily = Daily0)
  annualFlex <- setupYears(DailyRS1FD0, paLong = paLong, paStart = paStart)
  c10 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f10 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  DailyRS2FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, 
                                      localDaily = Daily0)
  annualFlex <- setupYears(DailyRS2FD0, paLong = paLong, paStart = paStart)
  c20 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f20 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  cDeltaTotal <- c22 - c11
  cRSpart <- c20 - c10
  cFDpart <- cDeltaTotal - cRSpart
  fDeltaTotal <- f22 - f11
  fRSpart <- f20 - f10
  fFDpart <- fDeltaTotal - fRSpart
  pairResults <- as.data.frame(matrix(ncol = 7, nrow = 2))
  colnames(pairResults) <- c("DeltaTotal", "RSpart", "FDpart", 
                             "x10", "x11", "x20", "x22")
  rownames(pairResults) <- c("Conc", "Flux")
  pairResults[1, ] <- c(cDeltaTotal, cRSpart, cFDpart, c10, 
                        c11, c20, c22)
  pairResults[2, ] <- 0.00036525 * c(fDeltaTotal, fRSpart, 
                                     fFDpart, f10, f11, f20, f22)
  attr(pairResults, "yearPair") <- c(paStart, paLong, year1, 
                                     year2)
  attr(pairResults, "dateInfo") <- dateInfo
  attr(pairResults, "SampleBlocks") <- c(sample1StartDate, sample1EndDate, sample2StartDate, sample2EndDate)
  attr(pairResults, "Other") <- c(minNumObs, minNumUncen, windowY, 
                                  windowQ, windowS, wall, edgeAdjust, as.Date(QStartDate), as.Date(QEndDate))
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(paStart, paLong)
  cat("\n  ", periodName, "\n")
  if (wall) 
    cat("\n Sample data set was partitioned with a wall right after ", 
        as.character(sample1EndDate), "\n")
  cat("\n Change estimates ", year2, " minus ", year1, "\n")
  totChange <- format(pairResults[1, 1], digits = 3)
  totChangePct <- format(100 * ((c22 - c11)/c11), digits = 2)
  cat("\n For concentration: total change is ", totChange, 
      "mg/L")
  cat("\n expressed as Percent Change is ", totChangePct, "%")
  pctRS <- format(100 * (cRSpart/cDeltaTotal), digits = 2)
  pctFD <- format(100 * (cFDpart/cDeltaTotal), digits = 2)
  cat("\n RS percent of total ", pctRS, "%,    FD percent of total ", 
      pctFD, "% \n\n")
  totChange <- format(pairResults[2, 1], digits = 3)
  totChangePct <- format(100 * ((f22 - f11)/f11), digits = 2)
  cat("\n For flux: total change is ", totChange, "million kg/year")
  cat("\n expressed as Percent Change is ", totChangePct, "%")
  pctRS <- format(100 * (fRSpart/fDeltaTotal), digits = 2)
  pctFD <- format(100 * (fFDpart/fDeltaTotal), digits = 2)
  cat("\n RS percent of total ", pctRS, "%,    FD percent of total ", 
      pctFD, "% \n\n")
  print(pairResults)
  return(pairResults)
  
}
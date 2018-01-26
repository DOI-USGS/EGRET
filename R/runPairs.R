#' runPairs
#' 
#' runPairs description
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param year1 integer year1
#' @param year2 integer year2
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param wall logical set up a "wall" on the Sample data
#' @param lastDaySample1 character in YYYY-MM-DD. Only used if wall is TRUE. Date of "the wall".
#' @param firstQDate0 character in YYYY-MM-DD. Overall trims Daily flow. Use NA to use all data.
#' @param lastQDate0 character in YYYY-MM-DD. Overall trims Daily flow. Use NA to use all data.
#' @param firstQDate1 character in YYYY-MM-DD. First limit of flow data to use in year 1. Use NA to automatically calculate based on windowSide.
#' @param lastQDate1 character in YYYY-MM-DD. Second limit of flow data to use in year 1. Use NA to automatically calculate based on windowSide.
#' @param firstQDate2 character in YYYY-MM-DD. First limit of flow data to use in year 2. Use NA to automatically calculate based on windowSide.
#' @param lastQDate2 character in YYYY-MM-DD. Second limit of flow data to use in year 2. Use NA to automatically calculate based on windowSide. 
#' @param windowSide integer number of automatically generated span sections, 
#' default is 7. If NA, code will use 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param interactive logical, defaults to FALSE. If TRUE, walks user through options.
#' @param \dots additional parameters
#' 
#' @examples 
#' eList <- Choptank_Phos
#' year1 <- 1985
#' year2 <- 2014
#' 
#' \dontrun{
#' # Automatic calculations based on windowSide=7
#' pairOut <- runPairs(eList, year1, year2)
#' 
#' # Specify flow normalize sections:
#' pairOut_custom <- runPairs(eList, year1, year2,
#'                            firstQDate1 = "1977-10-01",
#'                            lastQDate1 = "1990-09-30",
#'                            firstQDate2 = "2000-10-01",
#'                            lastQDate2 = "2014-09-25") 
#' 
#' pairOut_interactive <- runPairs(eList, year1, year2,
#'                                 interactive = TRUE)
#' 
#' }
runPairs <- function(eList, year1, year2, 
                     windowSide = 7, 
                     wall = FALSE, lastDaySample1 = NA, 
                     firstQDate0 = NA, lastQDate0 = NA,
                     firstQDate1 = NA, lastQDate1 = NA,
                     firstQDate2 = NA, lastQDate2 = NA,
                     minNumObs = 100, minNumUncen = 50,
                     windowY=7, windowQ=2, windowS=0.5,
                     interactive = FALSE,edgeAdjust=TRUE,
                     ...){
  
  localDaily <- eList$Daily
  localSample <- eList$Sample
  
  firstDayDaily <- localDaily$Date[1]
  lastDayDaily <- localDaily$Date[length(localDaily$Date)]
  
  firstDaySample <- localSample$Date[1]
  lastDaySample <- localSample$Date[length(localSample$Date)]
  
  if (interactive) {
    nSamples <- length(localSample$Date)
    nUncen <- sum(localSample$Uncen)
    message("Daily  dataframe runs from ", firstDayDaily, 
            " to ", lastDayDaily)
    message("Sample dataframe runs from ", firstDaySample, 
            " to ", lastDaySample)
    message("Sample size is ", nSamples, " number of uncensored samples is ", 
            nUncen)
    message("Default value for minNumObs is 100, it must be smaller than ", 
            nSamples, " enter your minimum")
    message("Enter value for minNumObs:")
    minNumObs <- as.numeric(readline())
    message("Default value for minNumUncen is 50, it must be smaller than ", 
            nUncen, " enter your minimum")
    message("Enter value for minNumUncen:")
    minNumUncen <- as.numeric(readline())
    message("Need to set the flow window width")
    message("Enter an integer value for windowSide, default is 7")
    message("The total width will be 1 + (windowSide * 2)")
    message("If the entry is 0, that means user wants to enter their own start and end dates")
    message("Enter value for windowSide:")
    windowSide <- as.numeric(readline())
    windowSide <- trunc(windowSide)
    if (windowSide == 0) {
      message("Enter firstQDate1 as yyyy-mm-dd, no quotes")
      firstQDate1 <- as.Date(readline())
      message("Enter lastQDate1 as yyyy-mm-dd, no quotes")
      lastQDate1 <- as.Date(readline())
      length1 <- format(as.numeric(as.Date(lastQDate1) - 
                                     as.Date(firstQDate1))/365.25, digits = 1)
      message("Note that the length of Daily1 will be ", 
              as.numeric(length1), " years")
      message("Enter firstQDate2 as yyyy-mm-dd, no quotes")
      firstQDate2 <- as.Date(readline())
      message("Enter lastQDate2 as yyyy-mm-dd, no quotes")
      lastQDate2 <- as.Date(readline())
      length2 <- format(as.numeric(as.Date(lastQDate2) - 
                                     as.Date(firstQDate2))/365.25, digits = 1)
      message("Note that the length of Daily2 will be ", 
              as.numeric(length2), " years")
    }
    message("If you want to divide the sample data with a wall, enter T, else F")
    wall <- as.logical(readline())
    if (wall) {
      message("enter the date for the wall, last day of first segment, yyyy-mm-dd")
      lastDaySample1 <- as.Date(readline())
      firstDaySample2 <- lastDaySample1 + days(1)
    }
  }
  
  if(is.na(firstQDate0)){
    firstQDate0 <- firstDayDaily
  }
  if(is.na(lastQDate0)){
    lastQDate0 <- lastDayDaily
  }
  firstQDate0 <- as.Date(firstQDate0)
  lastQDate0 <- as.Date(lastQDate0)

  localINFO <- getInfo(eList)
  
  if (all(c("paStart", "paLong") %in% names(localINFO))) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
  } else {
    paLong <- 12
    paStart <- 10
  }

  startEnd1 <- startEnd(paStart, paLong, year1)
  startEnd2 <- startEnd(paStart, paLong, year2)
  
  
  if (!is.na(windowSide) & windowSide > 0) {
    windowFull <- 1 + (2 * windowSide)
    first <- startEnd(paStart, paLong, year1 - windowSide)[["startDate"]]
    firstQDate1 <- first
    if (as.Date(first) < as.Date(firstDayDaily)) {
      firstQDate1 <- firstDayDaily
    }
    lastQDate1 <- as.POSIXlt(firstQDate1)
    lastQDate1$year <- lastQDate1$year + windowFull
    lastQDate1 <- as.Date(lastQDate1) - 1
    last <- startEnd(paStart, paLong, year2 + windowSide)[["endDate"]]
    lastQDate2 <- last
    if (as.Date(last) > as.Date(lastDayDaily)) {
      lastQDate2 <- lastDayDaily
    }
    firstQDate2 <- as.POSIXlt(lastQDate2)
    firstQDate2$year <- firstQDate2$year - windowFull
    firstQDate2 <- as.Date(firstQDate2) + 1
    
  } else {
    if (as.Date(startEnd1[["startDate"]]) < as.Date(firstQDate1)) {
      stop("year1 in the specified period of record comes \n         before firstQDate1")
    }
    if (as.Date(startEnd1[["endDate"]]) > as.Date(lastQDate1)) {
      stop("year1 at the end of the specified period of record comes \n         after lastQDate1")
    }
    if (as.Date(startEnd2[["startDate"]]) < as.Date(firstQDate2)) {
      stop("year2 in the specified period of record comes \n           before firstQDate2")
    }
    if (as.Date(startEnd2[["endDate"]]) > as.Date(lastQDate2)) {
      stop("year2 at the end of the specified period of record comes \n           after lastQDate2")
    }
  }
  
  lastDaySample1Set <- lastDaySample1
  firstDaySample1 <- firstDaySample
  lastDaySample1 <- lastDaySample
  firstDaySample2 <- firstDaySample
  lastDaySample2 <- lastDaySample
  
  if (wall) {
    if (is.na(lastDaySample1Set)) {
      stop("When using the wall option, please specify lastDaySample1")
    }
    lastDaySample1 <- lastDaySample1Set
    firstDaySample2 <- as.Date(lastDaySample1) + 1
    lastDaySample2 <- lastDaySample
  }
  
  Sample1 <- localSample[localSample$Date >= firstDaySample1 & 
                           localSample$Date <= lastDaySample1, ]
  Sample2 <- localSample[localSample$Date >= firstDaySample2 & 
                           localSample$Date <= lastDaySample2, ]
  # need to check on minNumObs and adjust if needed
  minNumObs <- ceiling(min(minNumObs, 0.8 * length(Sample1$Date), 0.8 * length(Sample2$Date)))
  minNumUncen <- ceiling(min(0.5 * minNumObs, 0.8 * sum(Sample1$Uncen), 0.8 * sum(Sample2$Uncen)))
  message("Sample1 has ", length(Sample1$Date), " Samples and ", sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", length(Sample2$Date), " Samples and ", sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", minNumUncen)
  #
  Daily1 <- localDaily[localDaily$Date >= firstQDate1 & localDaily$Date <= 
                         lastQDate1, ]
  Daily2 <- localDaily[localDaily$Date >= firstQDate2 & localDaily$Date <= 
                         lastQDate2, ]
  # here is a change, I've added localDaily in here.  It is only needed to set the DecLow and DecHigh 
  # inside the estSurfaces, but it can make a small difference
  surfaces1 <- estSurfaces(eList, surfaceStart = startEnd1[["startDate"]], 
                           surfaceEnd = startEnd1[["endDate"]], localSample = Sample1, localDaily = Daily1, 
                           minNumObs = minNumObs, minNumUncen = minNumUncen, windowY = windowY, 
                           windowQ = windowQ, windowS = windowS, edgeAdjust = edgeAdjust, verbose = FALSE)
  surfaces2 <- estSurfaces(eList, surfaceStart = startEnd2[["startDate"]], 
                           surfaceEnd = startEnd2[["endDate"]], localSample = Sample2, localDaily = Daily2, 
                           minNumObs = minNumObs, minNumUncen = minNumUncen, windowY = windowY, 
                           windowQ = windowQ, windowS = windowS, edgeAdjust = edgeAdjust, verbose = FALSE)
  
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
  Daily0 <- localDaily[localDaily$Date >= firstQDate0 & localDaily$Date <= 
                         lastQDate0, ]
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
  # this is the major change, putting everything in the attributes
  attr(pairResults, "yearPair") <- c(paStart, paLong, year1, 
                                     year2)
  attr(pairResults, "FDblocks") <- c(as.Date(firstQDate0), as.Date(lastQDate0), 
                                     as.Date(firstQDate1), as.Date(lastQDate1), 
                                     as.Date(firstQDate2), as.Date(lastQDate2))
  attr(pairResults, "SampleBlocks") <- c(as.Date(firstDaySample1), as.Date(lastDaySample1),
                                         as.Date(firstDaySample2), as.Date(lastDaySample2))
  attr(pairResults, "Other") <- c(minNumObs, minNumUncen, windowY, windowQ,
                                  windowS, wall, edgeAdjust)
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(paStart, paLong)
  cat("\n  ", periodName, "\n")
  if (wall) {
    cat("\n Sample data set was partitioned with a wall at ", 
        as.character(lastDaySample1), "\n")
  }
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
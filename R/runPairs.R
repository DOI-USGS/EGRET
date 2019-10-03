#' Runs a comparison of any two years in the record.
#' 
#' \code{runPairs} provides comparisons of results, in terms of 
#' flow-normalized concentration and flow-normalzed flux for any pair 
#' of years in the water quality record.  Comparison could involve the 
#' use of the "wall" and/or use of "generalized flow normalization".  
#' These two concepts are described in detail in the vignette.
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param year1 integer the ending year of the first year in the pair
#' @param year2 integer the ending year of the second year in the pair
#' @param windowSide integer. The width of the flow normalization window on each side of the year being estimated.
#' A common value is 7, but no default is specified.  If stationary flow normalization is to be used, then windowSide = 0 (this means that 
#' flow-normalization period for all years is the same).
#' @param flowBreak logical. Is there an abrupt break in the discharge record, default is FALSE.
#' @param Q1EndDate The Date (as character in YYYY-MM-DD) which is the last day, just before the flowBreak.
#' @param QStartDate The first Date (as character in YYYY-MM-DD) used in the  flow normalization method.  Default is 
#' NA, which makes the QStartDate become the first Date in eList$Daily. 
#' @param QEndDate The last Date (as character in YYYY-MM-DD) used in the flow normalization method.  Default is NA, 
#' which makes the QEndDate become the last Date in eList$Daily.
#' @param wall logical. Whether there is an abrupt break in the concentration versus discharge relationship.  Default is FALSE
#' @param sample1EndDate The Date (as character in YYYY-MM-DD) of the last date just before the wall. Default = NA. 
#' A date must be specified if wall = TRUE.
#' @param sampleStartDate The Date (as character in YYYY-MM-DD) of the first sample to be used. Default is NA which sets it 
#' to the first Date in eList$Sample.
#' @param sampleEndDate The Date (as character in YYYY-MM-DD) of the last sample to be used. 
#' Default is NA which sets it to the last Date in eList$Sample.
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12. 
#' Default is NA, which will use the paLong in the eList$INFO data frame. See also \code{\link{setPA}}.
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12.
#' Default is NA, which will use the paStart in the eList$INFO data frame. See also \code{\link{setPA}}.
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param fractMin numeric specifying the minimum fraction of the observations required to run the weighted regression, default is 0.75. The
#' minimum number will be the maximum of minNumObs and fractMin multiplied by total number of observations.
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The edgeAdjust method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param oldSurface logical specifying whether to use the original surface, or create a new one. Default is FALSE.
#' @return data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Description \cr
#' Total Change \tab   The difference between the results for year2 - year1\cr
#' CQTC \tab this number is the difference between between the two years, but only the part that is due to the change 
#' in the CQR. It is x20 - x10. In the results reported above as, "Concentration v. Q Trend 
#' Component" it is computed as 100 * (x20 - x10) / x11 \cr
#' QTC \tab  The difference between the two years, but only the part that is due to the change in the QD. 
#' It is the Total Change - CQTC. Or it can be stated as x22 - x11 - x20 + x10. In the results reported above as, "Q Trend Component" 
#' it is computed as 100 * (x22 - x11 - x20 + x10) / x11. \cr
#' x10 \tab The results using the concentration versus discharge relationship (CQR) for year 1, but using the discharge 
#' distribution (QD) for the entire period of record (starting with QStartDate and 
#' ending with QEndDate, or if they aren't specified, it is all the discharge data 
#' in the Daily data frame).\cr
#' x11 \tab The results using the CQR for year 1, but using the QD specified by the user for year 1.\tab \cr
#' x20 \tab The results using the CQR for year 2, but using the QD for the entire period. \cr
#' x22 \tab The results for the CQR for year 2, but using the QD specified by the user for year 2. \cr
#' }
#' Additionally, there is an attribute on the data frame "Other", containing
#' a list that includes minNumObs=minNumObs, minNumUncen, windowY, windowQ, 
#' windowS, wall, edgeAdjust, QStartDate, QEndDate, PercentChangeConc, and PercentChangeFlux.
#' 
#' PercentChangeConc, and PercentChangeFlux are vectors with:
#' Total Percent Change  is the Total Change divided by x11
#' CQTC Percent is the CQTC divided by x11
#' QTC Percent  is the QTC divided by x11
#' 
#' @examples 
#' eList <- Choptank_eList
#' year1 <- 1985
#' year2 <- 2010
#' 
#' \donttest{
#' # Automatic calculations based on windowSide=7
#' # four possible ways to do generalized flow normalization:
#' 
#' #Option 1: Use all years for flow normalization.
#' 
#' pairOut_1 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' # Option 2:  Use different windows for flow normalization for year1 versus year2
#' #            In each case it is a 15 year window (15 = 1 + 2*7)
#' 
#' pairOut_2 <- runPairs(eList, year1, year2, windowSide = 7)
#' 
#' # Option 3: Flow normalization is based on splitting the flow record at 1990-09-30
#' #          But year1 uses all flow data from before the break, 
#' #          year2 uses all flow data after the break
#' 
#' pairOut_3 <- runPairs(eList, year1, year2, 
#'                       windowSide = 0, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#' 
#' # Option 4: Flow normalization is based on splitting the flow record at 1990-09-30
#' #           but year1 uses a 15 year window before the break
#' #           year2 uses a 15 year window after the break
#' 
#' pairOut_4 <- runPairs(eList, year1, year2, 
#'                       windowSide = 7, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#'                       
#' 
#' }
runPairs <- function(eList, year1, year2, windowSide, 
                     flowBreak = FALSE,
                     Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                     wall = FALSE, oldSurface = FALSE,
                     sample1EndDate = NA, sampleStartDate = NA, sampleEndDate = NA,
                     paStart = NA, paLong = NA,
                     minNumObs = 100, minNumUncen = 50, fractMin = 0.75,
                     windowY = 7, windowQ = 2, windowS = 0.5, 
                     edgeAdjust = TRUE){
  
  if(wall & oldSurface){
    message("Setting both arguments wall and oldSurfaces to TRUE are not allowed.")
    message("Re-calculating surface.")
    oldSurface <- FALSE
  }
  
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(is.na(paStart)){
    paStart <- eList$INFO$paStart
  } else {
    eList$INFO$paStart <- paStart
  }
  
  if(is.na(paLong)){
    paLong <- eList$INFO$paLong
  } else {
    eList$INFO$paLong <- paLong
  }
  
  startEndSurface1 <- startEnd(paStart, paLong, year1)
  startEndSurface2 <- startEnd(paStart, paLong, year2)
  
  if(startEndSurface2$startDate > range(localSample$Date)[2]){
    stop("year2 is outside the Sample range")
  }
  
  if(startEndSurface1$endDate < range(localSample$Date)[1]){
    stop("year1 is outside the Sample range")
  }
  
  if(is.na(sampleStartDate)){
    sampleStartDate <- localSample$Date[1]
  }  else {
    sampleStartDate <- as.Date(sampleStartDate)
  }
  
  numSamples <- length(localSample$Date)
  
  if(is.na(sampleEndDate)){
    sampleEndDate <- localSample$Date[numSamples]
  }  else {
    sampleEndDate <- as.Date(sampleEndDate)
  }
  
  if(is.na(QStartDate)){
    QStartDate <- localDaily$Date[1]
  } else {
    QStartDate <- as.Date(QStartDate)
  }
  
  numQDays <- length(localDaily$Date)
  
  if(is.na(QEndDate)){
    QEndDate <- localDaily$Date[numQDays]
  } else {
    QEndDate <- as.Date(QEndDate)
  }
  
  if (sampleStartDate > as.Date(startEndSurface1[[2]]) ){
    stop("Sample start is later than year2")
  }  
  
  if (sampleEndDate < as.Date(startEndSurface2[[1]]) ){
    stop("Sample end is earlier than year1")
  }
  localsurfaces <- getSurfaces(eList)
  
  if(oldSurface){
    if(all(is.na(localsurfaces))){
      message("No surface included in eList, running estSurface function")
      oldSurface <- FALSE
    } 
  }
  
  if(flowBreak && is.na(Q1EndDate)) stop("if there is a flowBreak you must provide Q1EndDate")
  
  # setting up the two flow windows
  # there are four cases
  flowNormStartCol <- "flowNormStart"
  flowNormEndCol <- "flowNormEnd"
  flowStartCol <- "flowStart"
  flowEndCol <- "flowEnd"
  
  if (windowSide <= 0 && !flowBreak) {
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(QStartDate, QStartDate)
    flowNormEnd <- c(QEndDate, QEndDate)
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else if (windowSide > 0 & !flowBreak) {
    dateInfo1 <- makeDateInfo(windowSide, startEndSurface1[["startDate"]], startEndSurface1[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo2 <- makeDateInfo(windowSide, startEndSurface2[["startDate"]], startEndSurface2[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  } else if (windowSide <= 0 && flowBreak) {
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- Q1EndDate + 1
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(as.Date(QStartDate), as.Date(Q2StartDate))
    flowNormEnd <- c(as.Date(Q1EndDate), as.Date(QEndDate))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else {
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
  
  fractMin <- min(fractMin, 1.0)
  
  minNumObs <- ceiling(min(minNumObs, fractMin * length(Sample1$Date), 
                           fractMin * length(Sample2$Date)))
  minNumUncen <- ceiling(min(0.5 * minNumObs, minNumUncen))
  
  message("Sample1 has ", length(Sample1$Date), " Samples and ", 
          sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", length(Sample2$Date), " Samples and ", 
          sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", 
          minNumUncen)
  check <- rep(1,4)
  if(minNumObs > length(Sample1$Date)) check[1] <- 0
  if(minNumObs > length(Sample2$Date)) check[2] <- 0
  if(minNumUncen > sum(Sample1$Uncen)) check[3] <- 0
  if(minNumUncen > sum(Sample2$Uncen)) check[4] <- 0
  
  if(sum(check) < 4) {
    stop("Data set too small for minNumObs or minNumUncen")
  }
  
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
  if(oldSurface){
    
    checkSurfaceSpan(eList)
    
    if(all(c("Year","LogQ","surfaceIndex") %in% names(attributes(localsurfaces)))){
      surfaceYear <- attr(localsurfaces, "Year")
      LogQ <- attr(localsurfaces, "LogQ")
    } else {
      localINFO <- getInfo(eList)
      LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
      surfaceYear <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    }
    
    startDec1 <- decimalDate(startEndSurface1[["startDate"]])
    endDec1 <- decimalDate(startEndSurface1[["endDate"]])
    startDec2 <- decimalDate(startEndSurface2[["startDate"]])
    endDec2 <- decimalDate(startEndSurface2[["endDate"]])
    
    surfIndex1 <- which(surfaceYear >= startDec1 & surfaceYear <= endDec1)
    surfIndex1 <- c(surfIndex1[1]-1,surfIndex1,surfIndex1[length(surfIndex1)]+1)
    surfIndex2 <- which(surfaceYear >= startDec2 & surfaceYear <= endDec2)
    surfIndex2 <- c(surfIndex2[1]-1,surfIndex2,surfIndex2[length(surfIndex2)]+1)
    
    surfaces1 <- localsurfaces[,surfIndex1,]
    surfaces2 <- localsurfaces[,surfIndex2,]
    
    attr(surfaces1, "LogQ") <- LogQ
    attr(surfaces1, "Year") <- surfaceYear[surfIndex1]
    
    attr(surfaces2, "LogQ") <- LogQ
    attr(surfaces2, "Year") <- surfaceYear[surfIndex2]
    
  } else {
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
  }

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
  
  totChangePct_conc <- 100*cDeltaTotal/c11
  totChangePct_flux <- 100*fDeltaTotal/f11
  
  CQTC_percent_conc <- 100*cRSpart / c11 # CQTC Percent
  QTC_percent_conc  <- 100*cFDpart / c11 # Q Trend Component Percent

  CQTC_percent_flux <- 100*fRSpart / f11
  QTC_percent_flux  <- 100*fFDpart / f11
  
  pairResults <- as.data.frame(matrix(ncol = 7, nrow = 2))
  colnames(pairResults) <- c("TotalChange", "CQTC", "QTC", 
                             "x10", "x11", "x20", "x22")
  rownames(pairResults) <- c("Conc", "Flux")
  pairResults[1, ] <- c(cDeltaTotal, cRSpart, cFDpart, c10, 
                        c11, c20, c22)
  # 0.00036525 is magic number to convert to million kg/year
  pairResults[2, ] <- 0.00036525 * c(fDeltaTotal, fRSpart, 
                                     fFDpart, f10, f11, f20, f22)
  
  yearPairInfo <- c(paStart, paLong, year1, year2)
  names(yearPairInfo) <- c("paStart","paLong","year1","year2")
  attr(pairResults, "yearPair") <- yearPairInfo
  
  attr(pairResults, "dateInfo") <- dateInfo
  
  SampleBlocks <- c(sample1StartDate, sample1EndDate, sample2StartDate, sample2EndDate)
  names(SampleBlocks) <- c("sample1StartDate", "sample1EndDate", "sample2StartDate", "sample2EndDate")
  attr(pairResults, "SampleBlocks") <- SampleBlocks
  
  Other <- list(minNumObs=minNumObs, 
                minNumUncen=minNumUncen, 
                windowY=windowY, 
                windowQ = windowQ, 
                windowS=windowS, 
                wall=wall,
                edgeAdjust=edgeAdjust,
                QStartDate = as.Date(QStartDate), 
                QEndDate = as.Date(QEndDate),
                PercentChangeConc = c("Total Percent Change" = totChangePct_conc, 
                                      "CQTC Percent" = CQTC_percent_conc, 
                                      "QTC Percent" = QTC_percent_conc),
                PercentChangeFlux = c("Total Percent Change" = totChangePct_flux, 
                                      "CQTC Percent" = CQTC_percent_flux, 
                                      "QTC Percent" = QTC_percent_flux))

  attr(pairResults, "Other") <- Other
  
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(paStart, paLong)
  cat("\n  ", periodName, "\n")
  if (wall) 
    cat("\n Sample data set was partitioned with a wall right after ", 
        as.character(sample1EndDate), "\n")
  cat("\n Change estimates ", year2, " minus ", year1, "\n")
  totChange <- format(pairResults[1, 1], digits = 3)
  totChangePct_conc_f <- format(totChangePct_conc, digits = 2)
  cat("\n For concentration: total change is ", totChange, 
      "mg/L")
  cat("\n expressed as Percent Change is ", totChangePct_conc_f, "%")
  pctRS <- format(CQTC_percent_conc, digits = 2)
  pctFD <- format(QTC_percent_conc, digits = 2)
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "%\n       Q Trend Component            ", 
      pctFD, "% \n\n")
  totChange <- format(pairResults[2, 1], digits = 3)
  totChangePct_flux_f <- format((totChangePct_flux), digits = 2)
  cat("\n For flux: total change is ", totChange, "million kg/year")
  cat("\n expressed as Percent Change is ", totChangePct_flux_f, "%")
  pctRS <- format(CQTC_percent_flux, digits = 2)
  pctFD <- format(QTC_percent_flux, digits = 2)
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "%\n       Q Trend Component            ", 
      pctFD, "% \n\n")
  print(pairResults[,1:7], digits = 2)
  return(pairResults)
  
}
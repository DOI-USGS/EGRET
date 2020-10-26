#' Runs a comparison of any group of years in the record.
#' 
#' \code{runGroups} provides comparisons of results, in terms of 
#' flow-normalized concentration and flow-normalized flux for any groups of years
#' of years in the water quality record.  Comparison could involve the 
#' use of the "wall" and/or use of "generalized flow-normalization".  
#' These two concepts are described in detail in the vignette:
#' \code{vignette("Enhancements", package = "EGRET")}.
#' 
#' @details
#' When using generalized flow-normalization, it is best to have the Daily data frame
#' extend well beyond the years that are in the Sample data frame.  Ideally, 
#' the Daily data frame would start windowSide years before the
#' start of the Sample data set, if the data exist to provide for that. Generally
#' that isn't possible for the end of the record because the Sample data
#' may end very close to the present. To the extent that is possible therefore, it is better to
#' include more discharge data after the end of the Sample record. 
#' Also note that in the case run in the examples don't do that, 
#' because the data set needs to be appropriate for stationary flow 
#' normalization as well (and package size considerations make it difficult to
#' include specialized examples).
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param group1firstYear decimal year. Starting year of first group.
#' @param group1lastYear decimal year. Ending year of first group.
#' @param group2firstYear decimal year. Starting year of second group.
#' @param group2lastYear decimal year. Ending year of second group.
#' @param windowSide integer. The width of the flow normalization window on each side of the year being estimated.
#' A common value is 11, but no default is specified.  If stationary flow normalization is to be used, then windowSide = 0 (this means that 
#' flow-normalization period for all years is the same).
#' @param flowBreak logical. Is there an abrupt break in the discharge record, default is FALSE.
#' @param Q1EndDate The Date (as character in YYYY-MM-DD) which is the last day, just before the flowBreak.
#' @param QStartDate The first Date (as character in YYYY-MM-DD) used in the  flow normalization method.  Default is 
#' NA, which makes the QStartDate become the first Date in eList$Daily. 
#' @param QEndDate The last Date (as character in YYYY-MM-DD) used in the flow normalization method.  Default is NA, 
#' which makes the QEndDate become the last Date in eList$Daily.
#' @param wall logical. Whether there is an abrupt break in the concentration versus discharge relationship due to some major change in 
#' pollution control or water management.  Default is FALSE.
#' @param surfaceStart The Date (or character in YYYY-MM-DD) that is the start of the WRTDS model to be estimated and the first of the 
#' daily outputs to be generated. Default is NA, which means that the surfaceStart is based on the date of the first sample. 
#' @param surfaceEnd The Date (or character in YYYY-MM-DD) that is the end of the WRTDS model to be estimated and the last of the daily outputs 
#' to be generated.  Default is NA, which means that the surfaceEnd is based on the date of the last sample. 
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
#' @param verbose logical specifying whether or not to display progress message
#' @param fractMin numeric specifying the minimum fraction of the observations required to run the weighted regression, default is 0.75. The
#' minimum number will be the maximum of minNumObs and fractMin multiplied by total number of observations.
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The edgeAdjust method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param oldSurface logical specifying whether to use the original surface, or create a new one. Default is FALSE.
#' @return Dataframe with 7 columns and 2 rows.  The first row is about trends in concentration (mg/L), the second column is about trends in flux (million kg/year).
#' The data frame has a number of attributes.
#' \tabular{ll}{
#' Column Name \tab Description \cr
#' Total Change \tab   The difference between the results for group2 - group1 (x22 - x11). \cr
#' CQTC \tab CQTC is the "Concentration v. Q Trend Component." It is the component of total change due to the change in the CQR (Concentration Discharge Relationship). (x20 - x10). \cr
#' QTC \tab QTC is the "Q Trend Component." It is the component of total change due to the trend in the QD (Discharge Distribution). (x22 - x11 - x20 + x10). \cr
#' x10 \tab The estimated value based on the CQR computed for the years in group1, integrated over the QD for the entire timespan of the Daily data frame (or the 
#' period QStartDate and to QEndDate if these are specified).\cr
#' x11 \tab The estimated value based on the CQR for the years in group1, integrated over the QD specified by the user for group1. \cr
#' x20 \tab The estimated value based on the CQR computed for the years in group2, integrated over the QD for the entire period of record. \cr
#' x22 \tab The estimated value based on the CQR for the years in group2, integrated over the QD specified by the user for group2. \cr
#' }
#' @examples 
#' eList <- Choptank_eList
#' \donttest{
#' 
#'#Option 1:  Use all years for group flow normalization.
#'groupOut_1 <- runGroups(eList,  windowSide = 0,
#'                        group1firstYear = 1980, group1lastYear = 1990,
#'                        group2firstYear = 1995, group2lastYear = 2005)
#'
#'# Option 2: Use sliding window.
#'#                In each case it is a 23 year window (23 = 1 + 2 * 11)
#'
#'groupOut_2 <- runGroups(eList,  windowSide = 11,
#'                        group1firstYear = 1980, group1lastYear = 1990,
#'                        group2firstYear = 1995, group2lastYear = 2005)
#'
#'# Option 3: Flow normalization is based on splitting the flow record at 1990-09-30
#'#                But in years before the break it uses all flow data from before the break,
#'#                and years after the break uses all flow data after the break
#'
#'groupOut_3 <- runGroups(eList,  windowSide = 0,
#'                        group1firstYear = 1980, group1lastYear = 1990,
#'                        group2firstYear = 1995, group2lastYear = 2005,
#'                        flowBreak = TRUE, 
#'                        Q1EndDate = "1990-09-30")
#'
#'# Option 4: Flow normalization is based on splitting the flow record at 1990-09-30
#'#                but before the break uses a 23 year window of years before the break
#'#                after the break uses a 23 year window of years after the break
#'groupOut_4 <- runGroups(eList,  windowSide = 11,
#'                        group1firstYear = 1980, group1lastYear = 1990,
#'                        group2firstYear = 1995, group2lastYear = 2005,
#'                        flowBreak = TRUE, 
#'                        Q1EndDate = "1990-09-30")
#' 
#' }
runGroups <- function (eList, windowSide, 
                       group1firstYear, group1lastYear,
                       group2firstYear, group2lastYear, 
                       surfaceStart = NA, surfaceEnd = NA,
                       flowBreak = FALSE, 
                       Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                       wall = FALSE, oldSurface = FALSE,  fractMin = 0.75,
                       sample1EndDate = NA, sampleStartDate = NA, 
                       sampleEndDate = NA, 
                       paStart = NA, paLong = NA, 
                       minNumObs = 100, minNumUncen = 50, 
                       windowY = 7, windowQ = 2, windowS = 0.5, 
                       edgeAdjust = TRUE, verbose = TRUE) {
  
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  if (is.na(sampleStartDate)){ 
    sampleStartDate <- localSample$Date[1]
  } else {
    sampleStartDate <- as.Date(sampleStartDate)
  }
  
  numSamples <- length(localSample$Date)
  
  if(is.na(sampleEndDate)){
    sampleEndDate <- localSample$Date[numSamples]
  } else {
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
  
  localDaily <- localDaily[localDaily$Date >= QStartDate & 
                             localDaily$Date <= QEndDate, ]
  firstSample <- localSample$Date[1]
  lastSample <- localSample$Date[length(localSample$Date)]
  localSample <- localSample[localSample$Date >= sampleStartDate & 
                               localSample$Date <= sampleEndDate, ]
  if (is.null(surfaceStart) || is.na(surfaceStart)) {
    surfaceStart <- surfaceStartEnd(paStart, paLong, sampleStartDate, 
                                    sampleEndDate)[["surfaceStart"]]
  }
  surfaceStart <- as.Date(surfaceStart)
  if (is.null(surfaceEnd) || is.na(surfaceEnd)) {
    surfaceEnd <- surfaceStartEnd(paStart, paLong, sampleStartDate, 
                                  sampleEndDate)[["surfaceEnd"]]
  }
  surfaceEnd <- as.Date(surfaceEnd)
  eList <- as.egret(eList$INFO, localDaily, localSample, localsurfaces)
  if (wall) {
    if (is.na(sample1EndDate)) {
      stop("if there is a wall, the user must specify sample1EndDate")
    }
    sample1EndDate <- as.Date(sample1EndDate)
    sample2StartDate <- as.Date(sample1EndDate) + 1
    sample1StartDate <- as.Date(sampleStartDate)
    sample2EndDate <- as.Date(sampleEndDate)
    surfaces <- stitch(eList, surfaceStart = surfaceStart, 
                       surfaceEnd = surfaceEnd, sample1StartDate = sampleStartDate, 
                       sample1EndDate = sample1EndDate, sample2StartDate = sample2StartDate, 
                       sample2EndDate = sampleEndDate, windowY = windowY, fractMin = fractMin,
                       windowQ = windowQ, windowS = windowS, minNumObs = minNumObs, 
                       minNumUncen = minNumUncen, edgeAdjust = edgeAdjust)
  } else {
    sample1StartDate <- as.Date(sampleStartDate)
    sample2StartDate <- as.Date(sampleStartDate)
    sample1EndDate <- as.Date(sampleEndDate)
    sample2EndDate <- as.Date(sampleEndDate)
    if (oldSurface) {
      if (all(is.na(localsurfaces))) {
        message("No surface included in eList, running estSurface function")
        oldSurface <- FALSE
      } else {
        surfaces <- localsurfaces
        checkSurfaceSpan(eList)
      }
    }
    if (!oldSurface) {
      surfaces <- estSurfaces(eList, surfaceStart = surfaceStart, 
                              surfaceEnd = surfaceEnd, windowY = windowY, windowQ = windowQ, 
                              windowS = windowS, minNumObs = minNumObs, minNumUncen = minNumUncen, 
                              edgeAdjust = edgeAdjust, verbose = verbose)
    }
  }
  eListS <- as.egret(eList$INFO, localDaily, localSample, surfaces)
# set up a version of dateINFO for the stationary flow case
  flowStart <- as.Date(surfaceStart)
  flowEnd <- as.Date(surfaceEnd)
  flowNormStart <- as.Date(QStartDate)
  flowNormEnd <- as.Date(QEndDate)
  dateInfoStationary <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                         flowEnd, stringsAsFactors = FALSE)
# end of stationary set up
  if (windowSide <= 0 && !flowBreak) {
    flowStart <- as.Date(surfaceStart)
    flowEnd <- as.Date(surfaceEnd)
    flowNormStart <- as.Date(QStartDate)
    flowNormEnd <- as.Date(QEndDate)
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else if (windowSide > 0 && !flowBreak) {
    option <- 2
    dateInfo <- makeDateInfo(windowSide, surfaceStart, surfaceEnd, 
                             QStartDate, QEndDate)
  } else if (windowSide <= 0 && flowBreak) {
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- as.Date(Q1EndDate) + 1
    flowStart <- c(as.Date(surfaceStart), as.Date(Q2StartDate))
    flowEnd <- c(as.Date(Q1EndDate), as.Date(surfaceEnd))
    flowNormStart <- c(as.Date(QStartDate), as.Date(Q2StartDate))
    flowNormEnd <- c(as.Date(Q1EndDate), as.Date(QEndDate))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else {
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- as.Date(Q1EndDate) + 1
    dateInfo1 <- makeDateInfo(windowSide, surfaceStart, Q1EndDate, 
                              QStartDate, Q1EndDate)
    dateInfo2 <- makeDateInfo(windowSide, Q2StartDate, surfaceEnd, 
                              Q2StartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  }
  eListOut <- flexFN(eListS, dateInfo, flowNormStartCol = "flowNormStart", 
                     flowNormEndCol = "flowNormEnd", flowStartCol = "flowStart", 
                     flowEndCol = "flowEnd", oldSurface = oldSurface)
# redo the eList for stationary flow
  eListOutStationary <- flexFN(eListS, dateInfoStationary, flowNormStartCol = "flowNormStart", 
                     flowNormEndCol = "flowNormEnd", flowStartCol = "flowStart", 
                     flowEndCol = "flowEnd", oldSurface = oldSurface)
  eListOut$INFO$wall <- wall
  if (!oldSurface) {
    eListOut$INFO$surfaceStart <- surfaceStart
    eListOut$INFO$surfaceEnd <- surfaceEnd
  }
  DailyFlex <- eListOut$Daily
  DailySta <- eListOutStationary$Daily
  annFlex <- setupYears(DailyFlex, paLong = paLong, paStart = paStart)
  annFlex$year <- floor(annFlex$DecYear + (annFlex$PeriodLong / 12) * 0.5)
  annSta <- setupYears(DailySta, paLong = paLong, paStart = paStart)
  annSta$year <- floor(annSta$DecYear + (annSta$PeriodLong / 12) * 0.5)
  annFlex1 <- annFlex[annFlex$DecYear >= group1firstYear & annFlex$DecYear <= group1lastYear,]
  annSta1 <- annSta[annSta$DecYear >= group1firstYear & annSta$DecYear <= group1lastYear,]
  annFlex2 <- annFlex[annFlex$DecYear >= group2firstYear & annFlex$DecYear <= group2lastYear,]
  annSta2 <- annSta[annSta$DecYear >= group2firstYear & annSta$DecYear <= group2lastYear,]
  c11 <- mean(annFlex1$FNConc, na.rm = TRUE)
  f11 <- mean(annFlex1$FNFlux, na.rm = TRUE)
  c22 <- mean(annFlex2$FNConc, na.rm = TRUE)
  f22 <- mean(annFlex2$FNFlux, na.rm = TRUE)
  c10 <- mean(annSta1$FNConc, na.rm = TRUE)
  f10 <- mean(annSta1$FNFlux, na.rm = TRUE)
  c20 <- mean(annSta2$FNConc, na.rm = TRUE)
  f20 <- mean(annSta2$FNFlux, na.rm = TRUE)
  # this next part comes right out of the runPairs code on June 13, 2018)
  cDeltaTotal <- c22 - c11
  cRSpart <- c20 - c10
  cFDpart <- cDeltaTotal - cRSpart
  fDeltaTotal <- f22 - f11
  fRSpart <- f20 - f10
  fFDpart <- fDeltaTotal - fRSpart
  groupResults <- as.data.frame(matrix(ncol = 7, nrow = 2))
  colnames(groupResults) <- c("TotalChange", "CQTC", "QTC", 
                             "x10", "x11", "x20", "x22")
  rownames(groupResults) <- c("Conc", "Flux")
  groupResults[1, ] <- c(cDeltaTotal, cRSpart, cFDpart, c10, 
                        c11, c20, c22)
  groupResults[2, ] <- 0.00036525 * c(fDeltaTotal, fRSpart, 
                                     fFDpart, f10, f11, f20, f22)
  groupInfo <- c(paStart, paLong, group1firstYear, group1lastYear, group2firstYear, group2lastYear)
  names(groupInfo) <- c("paStart", "paLong", "group1firstYear", "group1lastYear",
                           "group2firstYear", "group2lastYear")
  attr(groupResults, "groupInfo") <- groupInfo
  attr(groupResults, "dateInfo") <- dateInfo
  SampleBlocks <- c(sample1StartDate, sample1EndDate, sample2StartDate, 
                    sample2EndDate, surfaceStart, surfaceEnd)
  names(SampleBlocks) <- c("sample1StartDate", "sample1EndDate", 
                           "sample2StartDate", "sample2EndDate", "surfaceStart", "surfaceEnd")
  attr(groupResults, "SampleBlocks") <- SampleBlocks
  Other <- list(minNumObs = minNumObs, minNumUncen = minNumUncen, fractMin = fractMin,
                windowY = windowY, windowQ = windowQ, windowS = windowS, 
                wall = wall, edgeAdjust = edgeAdjust, QStartDate = as.Date(QStartDate), 
                QEndDate = as.Date(QEndDate))
  attr(groupResults, "Other") <- Other
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(paStart, paLong)
  cat("\n  ", periodName, "\n")
  if (wall) 
    cat("\n Sample data set was partitioned with a wall right after ", 
        as.character(sample1EndDate), "\n")
  cat("\n Change estimates for\n average of", group2firstYear," through",group2lastYear,
      " minus average of", group1firstYear," through", group1lastYear, "\n")
  totChange <- format(groupResults[1, 1], digits = 3)
  totChangePct <- format(100 * ((c22 - c11)/c11), digits = 2)
  cat("\n For concentration: total change is ", totChange, 
      "mg/L")
  cat("\n expressed as Percent Change is ", totChangePct, "%")
  pctRS <- format(100 * (cRSpart/c11), digits = 2)
  pctFD <- format(100 * (cFDpart/c11), digits = 2)
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "%\n       Q Trend Component            ", 
      pctFD, "% \n\n")
  totChange <- format(groupResults[2, 1], digits = 3)
  totChangePct <- format(100 * ((f22 - f11)/f11), digits = 2)
  cat("\n For flux: total change is ", totChange, "million kg/year")
  cat("\n expressed as Percent Change is ", totChangePct, "%")
  pctRS <- format(100 * (fRSpart/f11), digits = 2)
  pctFD <- format(100 * (fFDpart/f11), digits = 2)
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "%\n       Q Trend Component            ", 
      pctFD, "% \n\n")
  print(groupResults, digits = 2)
  return(groupResults)
}
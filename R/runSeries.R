#' runSeries
#' 
#' runSeries
#' four possible situations 
#'  1 no flexible flow normalization at all
#'  2 moving window flexible flow normalization, but no break
#'  3 there is a break (must be specified as lastQDate1) but no moving window on each side
#'  4 there is a break (must be specified as lastQDate1) but there is a moving window on each side
#' 
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowSide integer The width of the flow normalization window on each side of the year being estimated.
#' @param flowBreak logical, is there an abrupt break in the QD
#' @param Q1EndDate The Date just before the flowBreak (or character in YYY-MM-DD format)
#' @param QStartDate The first Date used in the QD (if NA, which is default, it is first Date in eList$Daily)
#' @param QEndDate The last Date used in the QD (if NA, which is default, it is the last Date in eList$Daily)
#' @param wall logical, there is an abrupt break in the CQR
#' @param sample1EndDate The Date of just before the wall
#' @param sampleStartDate The Date of the first sample to be used (if NA, which is default, it is the first Date in eList$Sample)
#' @param sampleEndDate The Date of the last sample to be used (if NA, which is default, it is the last Date in eList$Sample)
#' @param surfaceStart The Date that is the start of the WRTDS model to be estimated and the last of the daily outputs to be 
#' generated (if NA it is the first day of the flow record)
#' @param surfaceEnd The Date that is the end of the WRTDS model to be estimated and the last of the daily outputs to be 
#' generated (if NA it is the last day of the flow record)
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param oldSurface logical specifying whether to use the original surface, or create a new one.
#' @export
#' @examples 
#' eList <- Choptank_Phos
#' 
#' \dontrun{
#' # Automatic calculations based on windowSide=7
#' 
#' #Option 1:
#' seriesOut_1 <- runSeries(eList,  windowSide = 0)
#' 
#' # Option 2:
#' seriesOut_2 <- runSeries(eList, windowSide = 7)
#' 
#' # Option 3:
#' seriesOut_3 <- runSeries(eList,
#'                        windowSide = 0, 
#'                        flowBreak = TRUE,
#'                        Q1EndDate = "1990-09-30")
#' 
#' # Option 4:
#' seriesOut_4 <- runSeries(eList, 
#'                       windowSide = 7, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#' 
#' }
runSeries <- function(eList, windowSide, 
                      surfaceStart = NA, surfaceEnd = NA, 
                      flowBreak = FALSE, 
                      Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                      wall = FALSE, oldSurface = FALSE,
                      sample1EndDate = NA, sampleStartDate = NA, sampleEndDate = NA,
                      paStart = 10, paLong = 12,
                      minNumObs = 100, minNumUncen = 50, windowY = 7, 
                      windowQ = 2, windowS = 0.5, edgeAdjust = TRUE){

  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  sampleStartDate <- if(is.na(sampleStartDate)) localSample$Date[1] else as.Date(sampleStartDate)
  numSamples <- length(localSample$Date)
  sampleEndDate <- if(is.na(sampleEndDate)) localSample$Date[numSamples] else as.Date(sampleEndDate)
  
  QStartDate <- if(is.na(QStartDate)) localDaily$Date[1] else as.Date(QStartDate)
  numQDays <- length(localDaily$Date)
  QEndDate <- if(is.na(QEndDate)) localDaily$Date[numQDays] else as.Date(QEndDate)
  localDaily <- localDaily[localDaily$Date >= QStartDate & 
                             localDaily$Date <= QEndDate, ]
  firstSample <- localSample$Date[1]
  lastSample <- localSample$Date[length(localSample$Date)]
  
  localSample <- localSample[localSample$Date >= sampleStartDate & 
                               localSample$Date <= sampleEndDate, ]
  
  fractionOfWaterYear <- decimalDate("2010-09-30") - trunc(decimalDate("2010-09-30"))
  
  surfaceStart <- as.Date(surfaceStart)
  if (is.na(surfaceStart)) {
    fractionOfYear <- decimalDate(sampleStartDate) - trunc(decimalDate(sampleStartDate))
    if(fractionOfYear > fractionOfWaterYear){
      surfaceStart <- paste0(trunc(decimalDate(sampleStartDate)),"-10-01")
    } else {
      surfaceStart <- paste0(trunc(decimalDate(sampleStartDate))-1,"-10-01")
    }
  }
  
  surfaceEnd <- as.Date(surfaceEnd)
  if (is.na(surfaceEnd)) {
    fractionOfYear <- decimalDate(sampleEndDate) - trunc(decimalDate(sampleEndDate))
    if(fractionOfYear > fractionOfWaterYear){
      surfaceEnd <- paste0(trunc(decimalDate(sampleEndDate))+1,"-09-30")
    } else {
      surfaceEnd <- paste0(trunc(decimalDate(sampleEndDate)),"-09-30")
    }
  }

  
  if (isTRUE(surfaceStart < QStartDate)) {
    stop("surfaceStart can't be before QStartDate")
  }
  if (isTRUE(surfaceEnd > QEndDate)) {
    stop("surfaceEnd can't be after QEndDate")
  }
  
  eList <- as.egret(eList$INFO, localDaily, localSample, localsurfaces)
  
  if (wall) {
    if (is.na(sample1EndDate)) {
      stop("if there is a wall, the user must specify sample1EndDate")
    }
    
    sample1EndDate <- as.Date(sample1EndDate)
    sample2StartDate <- as.Date(sample1EndDate) + 1
    surfaces <- stitch(eList, surfaceStart = surfaceStart, 
                          surfaceEnd = surfaceEnd, sample1StartDate = sampleStartDate, 
                          sample1EndDate = sample1EndDate, sample2StartDate = sample2StartDate, 
                          sample2EndDate = sampleEndDate, windowY = windowY, 
                          windowQ = windowQ, windowS = windowS, minNumObs = minNumObs, 
                          minNumUncen = minNumUncen, edgeAdjust = TRUE)
  } else {
    
    if(oldSurface){
      if(all(is.na(localsurfaces))){
        message("No surface included in eList, running estSurface function")
        oldSurface <- FALSE
      } else {
        #Need to do surfaceStart/End
        surfaces <- localsurfaces
        checkSurfaceSpan(eList)
      }
    }
    
    if(!oldSurface){
      surfaces <- estSurfaces(eList, surfaceStart = surfaceStart, surfaceEnd = surfaceEnd,
                                  windowY = windowY, windowQ = windowQ, 
                                  windowS = windowS, minNumObs = minNumObs, minNumUncen = minNumUncen, 
                                  edgeAdjust = TRUE)      
    } 
    
  }
  
  eListS <- as.egret(eList$INFO, localDaily, localSample, surfaces)

  
  if (windowSide <= 0 & !flowBreak) {
    option <- 1
    
    flowStart <- as.Date(surfaceStart)
    flowEnd <- as.Date(surfaceEnd)
    flowNormStart <- as.Date(QStartDate)
    flowNormEnd <- as.Date(QEndDate)
    
    dateInfo <- data.frame(flowNormStart, flowNormEnd,
                           flowStart,flowEnd, 
                           stringsAsFactors = FALSE)
    
  } else if (windowSide > 0 & !flowBreak) {
    option <- 2
    dateInfo <- makeDateInfo(windowSide, surfaceStart, surfaceEnd, 
                                QStartDate, QEndDate)
  } else if (windowSide <= 0 & flowBreak) {
    option <- 3
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- as.Date(Q1EndDate) + 1
    flowStart <- c(as.Date(surfaceStart), as.Date(Q2StartDate))
    flowEnd <- c(as.Date(Q1EndDate), as.Date(surfaceEnd))
    flowNormStart <- c(as.Date(QStartDate), as.Date(Q2StartDate))
    flowNormEnd <- c(as.Date(Q1EndDate), as.Date(QEndDate))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else {
    option <- 4
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
  
  eListOut$INFO$wall <- wall
  eListOut$INFO$surfaceStart <- surfaceStart
  eListOut$INFO$surfaceEnd <- surfaceEnd
  eListOut$INFO$sampleStartDate <- sampleStartDate
  eListOut$INFO$sampleEndDate <- sampleEndDate
  eListOut$INFO$sample1EndDate <- sample1EndDate
  eListOut$INFO$flowBreak <- flowBreak
  eListOut$INFO$windowSide <- windowSide
  eListOut$INFO$Q1EndDate <- Q1EndDate
  eListOut$INFO$QStartDate <- QStartDate
  eListOut$INFO$QEndDate <- QEndDate
  eListOut$INFO$paLong <- paLong
  eListOut$INFO$paStart <- paStart
  eListOut$INFO$minNumUncen <- minNumUncen
  eListOut$INFO$minNumObs <- minNumObs
  eListOut$INFO$windowQ <- windowQ
  eListOut$INFO$windowY <- windowY
  eListOut$INFO$windowS <- windowS
  
  attr(eListOut, "runSeries") <- TRUE
  
  return(eListOut)
  
}


#' makeDateInfo
#' 
#' makeDateInfo
#' 
#' @param windowSide integer number of automatically generated span sections, 
#' default is 7. If NA, code will use 
#' @param surfaceStart character (or Date) in YYYY-MM-DD. Date on which we want the analysis to start, it must be at or after the 
#    start of the eList$Daily dataframe
#' @param surfaceEnd character (or Date) in YYYY-MM-DD. Date on which we want the analysis to end, it must be at or before the end of 
#    the eList$Daily data frame
#' @param firstQDate0 character (or Date) in YYYY-MM-DD. The first day used in flow normalizing distributions, default is 
#'         the start of eList$Daily
#' @param lastQDate0 character (or Date) in YYYY-MM-DD. The last day used in flow normalizating distributions, default is
#'         the end of eList$Daily
#' @export
#' @examples 
#' windowSide <- 7
#' surfaceStart <- "1984-01-01"
#' surfaceEnd <- "2012-12-31"
#' firstQDate0 <- "1970-01-01"
#' lastQDate0 <- "2014-06-01"
#' dateInfo <- makeDateInfo(windowSide, 
#'                          surfaceStart, surfaceEnd, 
#'                          firstQDate0, lastQDate0)
makeDateInfo <- function(windowSide, 
                         surfaceStart, surfaceEnd, 
                         firstQDate0, lastQDate0){
  windowFull <- 1 + (2 * windowSide)
  
  surfaceStart <- as.Date(surfaceStart)
  surfaceEnd <- as.Date(surfaceEnd)
  firstQDate0 <- as.Date(firstQDate0)
  lastQDate0 <- as.Date(lastQDate0)
  
  daysApart <- surfaceEnd - surfaceStart
  daysApart <- daysApart + 1
  daysApart <- as.numeric(daysApart)
  yearsApart <- daysApart / 365
  nSeg <- ceiling(yearsApart)
  
  flowStart <- seq.Date(surfaceStart, by = "1 year", length.out = nSeg)
  
  if(length(flowStart)>1){
    flowEnd <- as.Date(seq.Date(flowStart[2], by = "1 year", length.out = nSeg)-1)
    flowEnd[flowEnd > surfaceEnd] <- surfaceEnd    
  } else {
    flowEnd <- surfaceEnd
  }

  flowNormStart <- rep(as.Date(firstQDate0), nSeg)
  flowNormEnd <- rep(as.Date(firstQDate0), nSeg)
  
  tempStart <- as.POSIXlt(flowStart)
  tempStart$year <-  tempStart$year - windowSide
  tempStart <- as.Date(tempStart)
  
  tempEnd <- as.POSIXlt(flowEnd)
  tempEnd$year <-  tempEnd$year + windowSide
  tempEnd <- as.Date(tempEnd)
  
  flowNormStart <- tempStart
  flowNormEnd <- tempEnd
  
  firstQDate0_lt <- as.POSIXlt(firstQDate0)
  lastQDate0_lt <- as.POSIXlt(lastQDate0)
  
  
  flowNormEnd[tempStart < firstQDate0] <- as.Date(paste(1900 + firstQDate0_lt$year+windowFull,firstQDate0_lt$mon+1, firstQDate0_lt$mday, sep = "-"))
  flowNormEnd[tempStart < firstQDate0] <- as.Date(flowNormEnd[tempStart < firstQDate0] - 1)
  
  flowNormStart[tempEnd > lastQDate0] <- as.Date(paste(1900 + lastQDate0_lt$year-windowFull,lastQDate0_lt$mon+1, lastQDate0_lt$mday, sep = "-"))
  flowNormStart[tempEnd > lastQDate0] <-  as.Date(flowNormStart[tempEnd > lastQDate0] + 1)
  
  flowNormEnd[flowNormEnd > lastQDate0] <- lastQDate0
  flowNormStart[flowNormStart < firstQDate0] <- firstQDate0
  
  dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, flowEnd, stringsAsFactors = FALSE)
  
  dateInfo <- dateInfo[dateInfo$flowStart < dateInfo$flowEnd,]
  
  return(dateInfo)
}

#' checkSurfaceSpan
#' 
#' checkSurfaceSpan
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @export
#' @examples  
#' eList <- Choptank_eList
#' checkSurfaceSpan(eList)
checkSurfaceSpan <- function(eList){
  
  surfaces <- getSurfaces(eList)
  localSample <- getSample(eList)
  
  if("Year" %in% names(attributes(surfaces))){
    Year <- attr(surfaces, "Year")
  } else {
    localINFO <- getInfo(eList)
    Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  }
  
  preSurface <- localSample$DecYear[1] - Year[1]
  postSurface <- Year[length(Year)] - localSample$DecYear[length(localSample$DecYear)]
  
  preSurfaceFormat <- format(preSurface, digits = 2)
  postSurfaceFormat <- format(postSurface, digits = 2)
  
  if(preSurface > 1 & postSurface > 1){
    message(paste0("\nThe surface you are using extends ", preSurfaceFormat , " years prior to the start,",
                   "and ",postSurfaceFormat," years past the end of the data of the water quality data set.",
                   "Extension of more than about a year is not recommended."))
  } else if (preSurface > 1){
    message(paste0("\nThe surface you are using extends ",  preSurfaceFormat,
                   " years prior to the start of the water quality data set.",
                   "Extension of more than about a year is not recommended."))           
  } else if (postSurface > 1){
    message(paste0("\nThe surface you are using extends ",  postSurfaceFormat,
                   "years past the end of the data of the water quality data set.",
                   "Extension of more than about a year is not recommended.")) 
  }
}

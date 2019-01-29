#' Annual series of flow-normalized concentration and flow-normalzed flux 
#'
#' \code{runSeries} provides annual series of flow-normalized concentration and flow-normalzed flux for the water quality record.  
#' Computations could involve the use of the "wall" and/or use of "generalized flow 
#' normalization".  These two concepts are described in detail in the vignette 
#' [need a simple name for it here].  
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowSide integer The width of the flow normalization window on each side of the year being estimated.  A common value is 7, but no default is specified.  If stationary flow normalization is to be used, then windowSide = 0 (this means that flow-normalization period for all years is the same).
#' @param surfaceStart The Date (or character in YYYY-MM-DD) that is the start of the WRTDS model to be estimated and the first of the 
#' daily outputs to be generated.  Default is NA, which means that the surfaceStart is based on the date of the first sample. 
#' @param surfaceEnd The Date (or character in YYYY-MM-DD) that is the end of the WRTDS model to be estimated and the last of the daily outputs 
#' to be generated.  Default is NA, which means that the surfaceEnd is based on the date of the last sample. 
#' @param flowBreak logical, is there an abrupt break in the discharge record, default is FALSE.
#' @param Q1EndDate The Date (as character in YYYY-MM-DD format) which is the last day, just before the flowBreak. Required if flowBreak = TRUE.
#' @param QStartDate The first Date (as character in YYYY-MM-DD format) used in the flow normalization.  Default is NA, which makes the QStartDate become the first Date in eList$Daily. 
#' @param QEndDate The last Date (as character in YYYY-MM-DD format) used in the flow normalization.  Default is NA, which makes the QEndDate become the last Date in eList$Daily. 
#' @param wall logical, there is an abrupt break in concentration versus discharge relationship.  Default is FALSE.
#' @param sample1EndDate The Date (as character in YYYY-MM-DD format) of the last day just before the wall.  Default = NA.  A date must be specified if wall = TRUE.
#' @param sampleStartDate The Date (as character in YYYY-MM-DD format) of the first sample to be used.  Default is NA which sets it to the first Date in eList$Sample.
#' @param sampleEndDate The Date (as character in YYYY-MM-DD format) of the last sample to be used. Default is NA which sets it to the last Date in eList$Sample. 
#' @param oldSurface logical, if TRUE, use surface previously estimated using modelEstimation.  Default is FALSE.
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12.
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 (used when period is water year). 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record. The edgeAdjust method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param fractMin numeric specifying the minimum fraction of the observations required to run the weighted regression, default is 0.75. The
#' minimum number will be the maximum of minNumObs and fractMin multiplied by total number of observations.
#' @param verbose logical specifying whether to output status messages.
#' @return eList named list with INFO, Daily, and Sample dataframes, along with the surfaces matrix.
#' @examples 
#' eList <- Choptank_eList
#' 
#' \dontrun{
#' # Automatic calculations based on windowSide=7
#' # four possible ways to do generalized flow normalization
#' 
#' #Option 1:  Use all years for flow normalization.
#' seriesOut_1 <- runSeries(eList,  windowSide = 0)
#' plotConcHist(seriesOut_1)
#' plotFluxHist(seriesOut_1)
#' 
#' # Option 2: Use sliding window throughout the whole flow normalization process.
#' #                In each case it is a 15 year window (15 = 1 + 2*7)
#' seriesOut_2 <- runSeries(eList, windowSide = 7)
#' 
#' plotConcHist(seriesOut_2)
#' plotFluxHist(seriesOut_2)
#' 
#' # Option 3: Flow normalization is based on splitting the flow record at 1990-09-30
#' #                But in years before the break it uses all flow data from before the break, 
#' #                and years after the break uses all flow data after the break
#' seriesOut_3 <- runSeries(eList,
#'                        windowSide = 0, 
#'                        flowBreak = TRUE,
#'                        Q1EndDate = "1990-09-30")
#'                        
#' plotConcHist(seriesOut_3)
#' plotFluxHist(seriesOut_3)
#' 
#' # Option 4: Flow normalization is based on splitting the flow record at 1990-09-30
#' #                but before the break uses a 15 year window of years before the break
#' #                after the break uses a 15 year window of years after the break
#' seriesOut_4 <- runSeries(eList, 
#'                       windowSide = 7, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#'                       
#' plotConcHist(seriesOut_4)
#' plotFluxHist(seriesOut_4)
#' 
#' }
runSeries <- function(eList, windowSide, 
                      surfaceStart = NA, surfaceEnd = NA, 
                      flowBreak = FALSE, 
                      Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                      wall = FALSE, oldSurface = FALSE,
                      sample1EndDate = NA, sampleStartDate = NA, sampleEndDate = NA,
                      paStart = 10, paLong = 12, fractMin = 0.75,
                      minNumObs = 100, minNumUncen = 50, windowY = 7, 
                      windowQ = 2, windowS = 0.5, edgeAdjust = TRUE, verbose = TRUE){

  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
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
  
  if(is.null(surfaceStart) || is.na(surfaceStart)){
    surfaceStart <- surfaceStartEnd(paStart, paLong, sampleStartDate, sampleEndDate)[["surfaceStart"]]
  }
  surfaceStart <- as.Date(surfaceStart)
  
  if (is.null(surfaceEnd) || is.na(surfaceEnd)) {
    surfaceEnd <- surfaceStartEnd(paStart, paLong, sampleStartDate, sampleEndDate)[["surfaceEnd"]]
  }
  surfaceEnd <- as.Date(surfaceEnd)
  
  eList <- as.egret(eList$INFO, localDaily, localSample, localsurfaces)
  
  minNumUncen <- ceiling(min(0.5 * minNumObs, minNumUncen))
  
  if (wall) {
    if (is.na(sample1EndDate)) {
      stop("if there is a wall, the user must specify sample1EndDate")
    }
    
    sample1EndDate <- as.Date(sample1EndDate)
    sample2StartDate <- as.Date(sample1EndDate) + 1
    surfaces <- stitch(eList, surfaceStart = surfaceStart, 
                          surfaceEnd = surfaceEnd, sample1StartDate = sampleStartDate, 
                          sample1EndDate = sample1EndDate, sample2StartDate = sample2StartDate, 
                          sample2EndDate = sampleEndDate, windowY = windowY, fractMin = fractMin,
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
                                  edgeAdjust = TRUE, verbose = verbose)      
    } 
    
  }
  
  eListS <- as.egret(eList$INFO, localDaily, localSample, surfaces)

  
  if (windowSide <= 0 && !flowBreak) {
    flowStart <- as.Date(surfaceStart)
    flowEnd <- as.Date(surfaceEnd)
    flowNormStart <- as.Date(QStartDate)
    flowNormEnd <- as.Date(QEndDate)
    
    dateInfo <- data.frame(flowNormStart, flowNormEnd,
                           flowStart,flowEnd, 
                           stringsAsFactors = FALSE)
    
  } else if (windowSide > 0 && !flowBreak) {
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
  
  eListOut$INFO$wall <- wall
  
  if(!oldSurface){
    eListOut$INFO$surfaceStart <- surfaceStart
    eListOut$INFO$surfaceEnd <- surfaceEnd    
  }

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
  eListOut$INFO$fractMin <- fractMin
  eListOut$INFO$minNumObs <- minNumObs
  eListOut$INFO$windowQ <- windowQ
  eListOut$INFO$windowY <- windowY
  eListOut$INFO$windowS <- windowS
  
  attr(eListOut, "runSeries") <- TRUE
  
  return(eListOut)
  
}


#' makeDateInfo
#' 
#' Create a data frame that organizes date segmentations for runSeries.
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
    if(all(c("bottomYear","stepYear","nVectorYear") %in% names(localINFO))){
      Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    } else {
      surfaceIndexParameters <- surfaceIndex(eList$Daily)
      bottomYear <- surfaceIndexParameters[['bottomYear']]
      stepYear <- surfaceIndexParameters[['stepYear']]
      nVectorYear <- surfaceIndexParameters[['nVectorYear']]
      Year <- seq(bottomYear, by=stepYear, length.out=nVectorYear)
    }
  }
  
  preSurface <- localSample$DecYear[1] - Year[1]
  postSurface <- Year[length(Year)] - localSample$DecYear[length(localSample$DecYear)]
  
  preSurfaceFormat <- format(preSurface, digits = 2)
  postSurfaceFormat <- format(postSurface, digits = 2)
  
  if(preSurface > 2 & postSurface > 2){
    message(paste0("\nThe surface you are using extends ", preSurfaceFormat , " years prior to the start,",
                   "and ",postSurfaceFormat," years past the end of the data of the water quality data set.",
                   "The surface is only reliable within the time period of the water quality data set.",
                   "Extensions of a year or more should not be used to characterize trends.  However, the fact",
                   "that there are such extensions, does not harm the reliability of the surface or the trend",
                   "results within the period for which there are water quality data"))
  } else if (preSurface > 2){
    message(paste0("\nThe surface you are using extends ",  preSurfaceFormat,
                   " years prior to the start of the water quality data set.",
                   "The surface is only reliable within the time period of the water quality data set.",
                   "Extensions of a year or more should not be used to characterize trends.  However, the fact",
                   "that there are such extensions, does not harm the reliability of the surface or the trend",
                   "results within the period for which there are water quality data"))           
  } else if (postSurface > 2){
    message(paste0("\nThe surface you are using extends ",  postSurfaceFormat,
                   " years past the end of the data of the water quality data set.",
                   "The surface is only reliable within the time period of the water quality data set.",
                   "Extensions of a year or more should not be used to characterize trends.  However, the fact",
                   "that there are such extensions, does not harm the reliability of the surface or the trend",
                   "results within the period for which there are water quality data")) 
  }
}

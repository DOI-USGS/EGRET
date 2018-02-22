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
#' @param wall logical set up a "wall" on the Sample data
#' @param flowBreak logical. User-specified break in the flow distribution
#' @param firstSampleDate0 character (or Date) in YYYY-MM-DD. First day of the Sample data set we will use, default is 
#'    use all samples starting with the first sample
#' @param lastSampleDate0 character (or Date) in YYYY-MM-DD. Last day of the Sample data set we will use, default is 
#'    use up through the last sample
#' @param lastSampleDate1 character (or Date) in YYYY-MM-DD. Day just before the wall in the RS
#' @param surfaceStart character (or Date) in YYYY-MM-DD. Date on which we want the analysis to start, it must be at or after the 
#    start of the eList$Daily dataframe
#' @param surfaceEnd character (or Date) in YYYY-MM-DD. Date on which we want the analysis to end, it must be at or before the end of 
#    the eList$Daily data frame
#' @param windowSide integer number of automatically generated span sections, 
#' default is 7. If NA, code will use 
#' @param firstQDate0 character (or Date) in YYYY-MM-DD. The first day used in flow normalizing distributions, default is 
#'         the start of eList$Daily
#' @param lastQDate0 character (or Date) in YYYY-MM-DD. The last day used in flow normalizating distributions, default is
#'         the end of eList$Daily
#' @param lastQDate1 character (or Date) in YYYY-MM-DD. The last day of the period leading up to the flow break, must be specified if
#'      flowBreak = TRUE
#'
#' @export
#' @examples 
#' eList <- Choptank_Phos
#' 
#' eList_series1 <- runSeries(eList, windowSide = 7)
#' plotConcHist(eList_series1)
#'
runSeries <- function(eList, 
                      surfaceStart = NA, surfaceEnd = NA, 
                      wall = FALSE, 
                      firstSampleDate0 = NA, 
                      lastSampleDate0 = NA, 
                      lastSampleDate1 = NA, 
                      flowBreak = FALSE, 
                      windowSide = 7, 
                      lastQDate1 = NA,
                      firstQDate0 = NA, lastQDate0 = NA, 
                      minNumObs = 100, minNumUncen = 50,
                      windowY = 7, windowQ = 2, windowS = 0.5, 
                      edgeAdjust = TRUE){


  localDaily <- getDaily(eList)
  localSample <- getSample(eList)
  
  firstQDate0 <- as.Date(firstQDate0)
  
  if(is.na(firstQDate0)) {
    firstQDate0 <- localDaily$Date[1]
  } 
  
  lastQDate0 <- as.Date(lastQDate0)
  if(is.na(lastQDate0)) {
    lastQDate0 <- localDaily$Date[length(localDaily$Date)]
  }
  
  localDaily <- localDaily[localDaily$Date >= firstQDate0 &
                           localDaily$Date <= lastQDate0,]
  # print(summary(localDaily$Date))
  firstSample <- localSample$Date[1]
  lastSample <- localSample$Date[length(localSample$Date)]
  
  firstSampleDate0 <- as.Date(firstSampleDate0)
  if(is.na(firstSampleDate0)) {
    firstSampleDate0 <- localSample$Date[1]
  }
  
  lastSampleDate0 <- as.Date(lastSampleDate0)
  if(is.na(lastSampleDate0)) {
    lastSampleDate0 <- localSample$Date[length(localSample$Date)]
  }
  
  localSample <- localSample[localSample$Date >= firstSampleDate0 &
                             localSample$Date <= lastSampleDate0,]
  eList <- as.egret(eList$INFO, localDaily, localSample)
  
  surfaceStart <- as.Date(surfaceStart)
  if(is.na(surfaceStart)){
    surfaceStart <- localSample$Date[1]
  }
  
  surfaceEnd <- as.Date(surfaceEnd)
  if(is.na(surfaceEnd)){
    surfaceEnd <- localSample$Date[nrow(localSample)]
  }
  
  # some logic checks here
  if(surfaceStart < firstQDate0) {
    stop ("surfaceStart can't be before firstQDate0")
  }
  if(surfaceEnd > lastQDate0) {
    stop ("surfaceEnd can't be after lastQDate0")
  }
  
  # first we will create the surfaces object
  if(wall) {
    if(is.na(lastSampleDate1)) {
      stop ("if there is a wall, the user must specify lastSampleDate1")
    }

    lastSampleDate1 <- as.Date(lastSampleDate1)
    firstSampleDate2 <- as.Date(lastSampleDate1) + 1

    surfaces <- stitch(eList, surfaceStart = surfaceStart, surfaceEnd = surfaceEnd, 
            firstSampleDate1 = firstSampleDate1, lastSampleDate1 = lastSampleDate1,
            firstSampleDate2 = firstSampleDate2, lastSampleDate2 = lastSampleDate2,
            windowY = windowY, windowQ = windowQ, windowS = windowS,
            minNumObs = minNumObs, minNumUncen = minNumUncen, edgeAdjust = TRUE)
    
  } else {
    surfaces <- estSurfaces(eList, windowY = windowY, windowQ = windowQ, windowS = windowS,
                            minNumObs = minNumObs, minNumUncen = minNumUncen, edgeAdjust = TRUE)
    
  }
  
  eListS <- as.egret(eList$INFO, localDaily, localSample, surfaces)
  
  # at this point, we have a new eList that has surfaces that use the wall if wall = TRUE)
  # next thing is to figure out how we are doing flexible flow normalization
  # four possible situations 
  #  1 no flexible flow normalization at all
  #  2 moving window flexible flow normalization, but no break
  #  3 there is a break (must be specified as lastQDate1) but no moving window on each side
  #  4 there is a break (must be specified as lastQDate1) but there is a moving window on each side
  flowNormStartCol <- "flowNormStart"
  flowNormEndCol <- "flowNormEnd"
  flowStartCol <- "flowStart"
  flowEndCol <- "flowEnd"
  dateInfo <- if (windowSide <= 0 & !flowBreak) {
    option <- 1
    # option 1, no normalization

    flowStart <- as.Date(surfaceStart)
    flowEnd <- as.Date(surfaceEnd)
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, flowEnd, stringsAsFactors = FALSE)
  } else if (windowSide > 0 & !flowBreak) {
    option <- 2
    # option 2, no flowBreak, standard moving window
    dateInfo <- makeDateInfo(windowSide, surfaceStart, surfaceEnd, firstQDate0, lastQDate0)
  } else if (windowSide <= 0 & flowBreak) {
    option <- 3
    # option 3, flowBreak but no moving window
    firstQDate2 <- as.Date(lastQDate1) + 1
    flowStart <- c(as.Date(surfaceStart), as.Date(firstQDate2))
    flowEnd <- c(as.Date(lastQDate1), as.Date(surfaceEnd))
    flowNormStart <- c(as.Date(firstQDate0), as.Date(firstQDate2))
    flowNormEnd <- c(as.Date(lastQDate1), as.Date(lastQDate0))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, flowEnd, stringsAsFactors = FALSE)
  } else {
    option <- 4
    # option 4, flowBreak and moving window
    firstQDate2 <- as.Date(lastQDate1) + 1
    dateInfo1 <- makeDateInfo(windowSide, surfaceStart, lastQDate1, firstQDate0, lastQDate1)
    dateInfo2 <- makeDateInfo(windowSide, firstQDate2, surfaceEnd, firstQDate2, lastQDate0)
    dateInfo <- rbind(dateInfo1, dateInfo2) 
  }
  # now we put it together the surfaces and the flex FN
  eListOut <- flexFN(eListS, dateInfo, 
                     flowNormStartCol = "flowNormStart", 
                     flowNormEndCol = "flowNormEnd", 
                     flowStartCol = "flowStart", 
                     flowEndCol = "flowEnd")
  eListOut$INFO$wall <- wall
  eListOut$INFO$surfaceStart <- surfaceStart
  eListOut$INFO$surfaceEnd <- surfaceEnd
  eListOut$INFO$lastSampleDate0 <- lastSampleDate0
  eListOut$INFO$lastSampleDate1 <- lastSampleDate1
  eListOut$INFO$flowBreak <- flowBreak
  eListOut$INFO$windowSide <- windowSide
  eListOut$INFO$lastQDate1 <- lastQDate1
  eListOut$INFO$firstQDate0 <- firstQDate0
  eListOut$INFO$lastQDate0 <- lastQDate0
  
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
  
  flowNormEnd[tempEnd > lastQDate0] <- lastQDate0
  flowNormStart[tempStart < firstQDate0] <- firstQDate0
  
  dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, flowEnd, stringsAsFactors = FALSE)
  
  dateInfo <- dateInfo[dateInfo$flowStart < dateInfo$flowEnd,]
  
  return(dateInfo)
}


#' makeSeriesOutputs
#' 
#' makeSeriesOutputs
#' 
#' @export
#' @param eList named list with at least Daily and INFO dataframes
#' @param startDate character or Date
#' @param endDate character or Date
#' @param printTable logical
#' @param plotConc logical
#' @param plotFlux logical
#' @examples 
#' eList <- Choptank_Phos
#' \dontrun{
#' eList_series1 <- runSeries(eList, windowSide = 7)
#' makeSeriesOutputs(eList_series1)
#' 
#' }
makeSeriesOutputs <- function(eList, 
                              startDate = NA, endDate = NA, 
                              printTable = TRUE,
                              plotConc = TRUE, plotFlux = TRUE) {
  # this function takes the output of runSeries and creates a table of annual values and makes two figures
  # that show time series of flexFNConc and flexFNFlux
  # this function will be swapped out for a better version when we have all the regular values, and regular flow normalized values
  # placed into the eList$Daily  This would also take in some of the other options available in tableResults and plotConcHist and plotFluxHist
  #  default for startDate is the date of the first flex values in Daily, endDate is the date of the last ones
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localDaily <- na.omit(eList$Daily)
  firstDate <- localDaily$Date[1]
  lastDate <- localDaily$Date[length(localDaily$Date)]
  startDate <- max(as.Date(firstDate), as.Date(startDate), na.rm = TRUE)
  endDate <- min(as.Date(lastDate), as.Date(endDate), na.rm = TRUE)
  localDaily <- localDaily[localDaily$Date >= startDate & localDaily$Date <= endDate,]

  localDaily$ConcDay <- localDaily$flexConc
  localDaily$FNConc <- localDaily$flexConc
  localDaily$FluxDay <- localDaily$flexFlux
  localDaily$FNFlux <- localDaily$flexFlux
  print("This is a temporary feature, the results shown are all flex values")
  tableResults(eList, localDaily = localDaily)
  eListBad <- as.egret(eList$INFO,localDaily,eList$Sample,eList$surfaces)
  plotConcHist(eListBad)
  plotFluxHist(eListBad)
  invisible(eListBad)
}


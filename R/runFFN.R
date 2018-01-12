#' runFFN
#' 
#' runFFN
#' 
#' @param eList named list with at least Daily and INFO dataframes
#' @param dateInfo data frame with 4 columns. The column names and descriptions are described in the next set of arguments
#' @param windowSide integer number of automatically generated span sections, 
#' default is 7. If NA, code will use 
#' @param interactive logical, defaults to FALSE. If TRUE, walks user through options.
#' @param verbose logical specifying whether or not to display progress message
#' @export
#' 
#' @examples 
#' eList <- Choptank_Phos
#' 
#' flowNormStart <- c("1979-10-01","1990-01-01","1992-10-10")
#' flowNormEnd <- c("1995-06-06","2004-03-03","2011-09-29")
#' flowStart <- c("1979-10-01","1995-06-07","2004-03-04")
#' flowEnd <- c("1995-06-06","2004-03-03","2011-09-29") 
#' 
#' dateInfo <- data.frame(flowNormStart,
#'                        flowNormEnd,
#'                        flowStart, 
#'                        flowEnd, 
#'                        stringsAsFactors = FALSE)
#' \dontrun{
#' AnnualResultsFlex <- runFFN(eList, dateInfo=dateInfo)
#' 
#' AnnualResultsFlex <- runFFN(eList, windowSide = 7)
#' 
#' AnnualResultsFlex <- runFFN(eList, interactive = TRUE)
#' 
#' eListWinter <- setPA(eList, paStart = 12, paLong=3)
#' AnnualResultsFlexWinter <- runFFN(eList, dateInfo)
#' }
runFFN <- function(eList, windowSide = NA, dateInfo = NA,
                   verbose = TRUE, interactive = FALSE) {

  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  if(!is.na(windowSide) & all(!is.na(dateInfo))){
    stop("Cannot specify BOTH windowSide and dateInfo")
  }
  
  if(is.na(windowSide) & all(is.na(dateInfo)) & !interactive){
    stop("Must specify 1 parameter, EITHER: windowSide, dateInfo, 
         or choose TRUE for interactive")
  }
  
  if(interactive){
    firstDayDaily <- localDaily$Date[1]
    lastDayDaily <- localDaily$Date[length(localDaily$Date)]
    
    firstDaySample <- localSample$Date[1]
    lastDaySample <- localSample$Date[length(localSample$Date)]
    nSamples <- length(localSample$Date)
    nUncen <- sum(localSample$Uncen)
    
    message("Daily  dataframe runs from ", firstDayDaily, " to ", lastDayDaily)
    message("Sample dataframe runs from ", firstDaySample, " to ", lastDaySample)
    message("Daily must cover a span of dates greater than or equal to Sample, at both ends")
    message("If this is not the case exit and fix it")
    message("Sample size is ", nSamples," number of uncensored samples is ", nUncen)

    message("How to create the segments for flow normalization")
    message("Enter 0 if you want an automatic sliding span. Or if you want to create them yourself")
    message("Enter a positive integer for the number of segments")
    segmentsIndicator <- as.numeric(readline())
    if(segmentsIndicator == 0){
      message("Specify windowSide, must be an integer 1 or greater, 7 is a typical choice")
      windowSide <- as.numeric(readline())
    } else {
      nSeg <- segmentsIndicator
      flowStart <- rep(as.Date(firstQDate0), nSeg) 
      flowEnd <- rep(as.Date(firstQDate0), nSeg)
      flowNormStart <- rep(as.Date(firstQDate0), nSeg)
      flowNormEnd <- rep(as.Date(firstQDate0), nSeg)
      for(iSeg in 1: nSeg){
        message("For segment ", iSeg," enter flowStart, flowEnd, flowNormStart, flowNormEnd, each on a separate line")
        message("flowStart[",iSeg,"]:")
        flowStart[iSeg] <- as.Date(readline())
        message("flowEnd[",iSeg,"]:")
        flowEnd[iSeg] <- as.Date(readline())
        message("flowNormStart[",iSeg,"]:")
        flowNormStart[iSeg] <- as.Date(readline())
        message("flowNormEnd[",iSeg,"]:")
        flowNormEnd[iSeg] <- as.Date(readline())
      }
    }
  }
  
  if(!is.na(windowSide) && windowSide > 0){
    windowFull <- 1 + (2 * windowSide)
    daysApart <- lastQDate0 - firstQDate0
    daysApart <- daysApart + 1
    daysApart <- as.numeric(daysApart)
    yearsApart <- daysApart / 365
    nSeg <- floor(yearsApart)
  
    flowStart <- seq.Date(firstQDate0, by = "1 year", length.out = 42)
    flowEnd <- as.Date(seq.Date(flowStart[2], by = "1 year", length.out = 42)-1)
  
    flowNormStart <- rep(as.Date(firstQDate0), nSeg)
    flowNormEnd <- rep(as.Date(firstQDate0), nSeg)
  
    tempStart <- as.POSIXlt(flowStart)
    tempStart$year <-  tempStart$year - windowSide
    tempStart <- as.Date(tempStart)
  
    tempEnd <- as.POSIXlt(flowEnd)
    tempEnd$year <-  tempEnd$year + windowSide
    tempEnd <- as.Date(tempEnd)
  
    for(iSeg in 1:nSeg) {
  
      if (tempStart[iSeg] >= firstDayDaily & tempEnd[iSeg] <= lastDayDaily) {
        # this is the case where the flow norm period can be symmetrical around the segment
        flowNormStart[iSeg] <- tempStart
        flowNormEnd[iSeg] <- tempEnd
      } else {
        # these are the cases where the flow norm period has to be pushed to one end or the other
        if (tempStart[iSeg] < firstDayDaily) {
          # this is the case where the flow norm period goes at the start of the record
          flowNormStart[iSeg] <- firstDayDaily
          flowNormEnd[iSeg] <- firstDayDaily + lubridate::years(windowFull)
        } else {
          # this is the case where the flow norm period goes at the end of the record
          flowNormStart[iSeg] <- lastDayDaily - lubridate::years(windowFull)
          flowNormEnd[iSeg] <- lastDayDaily}
      }
    }
  
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, flowEnd, stringsAsFactors = FALSE)
  }
  
  nSeg <- length(dateInfo$flowStart)
  surfaceStart <- dateInfo$flowStart[1]
  surfaceEnd <- dateInfo$flowEnd[nSeg]
  newSurfaces <- estSurfaces(eList, surfaceStart = surfaceStart, surfaceEnd = surfaceEnd, verbose = verbose)
  
  Daily0 <- localDaily[localDaily$Date >= surfaceStart &
                         localDaily$Date <= surfaceEnd,]
  oldDaily <- Daily0
  newList <- flexFN(eList, dateInfo, localsurfaces = newSurfaces)
  newDaily <- newList$Daily[newList$Daily$Date >= surfaceStart &
                              newList$Daily$Date <= surfaceEnd,]

  AnnualResultsFlex <- setupYears(localDaily = newDaily, paStart = paStart, paLong = paLong)
  
  if(verbose){
    message(" ")
    tableResults(eList, localDaily = newDaily)
    message("Flexible Flow Normalization segmentation is shown here")
    message("It uses ", nSeg, " segments")
  }
  return(AnnualResultsFlex)
}

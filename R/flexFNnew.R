#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo data frame with 3 columns, their names defined by sampleStart, flowStart, flowEnd
#' @param sampleStart integer vector of start years (water) for each FN conc/flux segment
#' @param flowStart integer vector of start years (water) for flow normalization
#' @param flowEnd integer vector of end years (water) for flow normalization
#' @export
#' @importFrom dataRetrieval calcWaterYear
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleSegStart <- c(1980,1990,2000)
#' flowSegStart <- c(1980,1985,1992)
#' flowSegEnd <- c(1994,2004,2011)
#' dateInfo <- data.frame(sampleSegStart, 
#'                        flowSegStart, 
#'                        flowSegEnd)
#' \dontrun{
#' eList <- flexFN(eList, dateInfo)
#' plotFluxHist(eList)
#' 
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleSegStart <- c(1980,1985,2000)
#' flowSegStart <- c(1980,1990,2000)
#' flowSegEnd <- c(1990,2000,2010)
#' dateInfo <- data.frame(sampleSegStart, 
#'                        flowSegStart, 
#'                        flowSegEnd)
#' eList <- flexFN(eList, dateInfo)
#' plotFluxHist(eList)
#' }
flexFN <- function(eList, dateInfo, sampleStart="sampleSegStart",
                   flowStart="flowSegStart", flowEnd="flowSegEnd"){
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Sample$WaterYear <- calcWaterYear(Sample$Date)
  Daily$WaterYear <- calcWaterYear(Daily$Date)
  
  dateInfo$sampleSegEnd <- c(dateInfo[2:nrow(dateInfo),sampleStart]-1,max(Sample$WaterYear))

  DailyFN <- estDailyFromSurfaces(eList)
  DailyFN$FNConc <- NA
  DailyFN$FNFlux <- NA
  
  DailyFN$WaterYear <- calcWaterYear(DailyFN$Date)
  
  newList <- as.egret(eList$INFO,DailyFN,Sample,eList$surfaces)
  
  for(i in seq(nrow(dateInfo))){
    dailyReturn <- estFNsegs(newList,dateInfo[i,])
    # sampleSegments <- c(dateInfo$sampleSegStart[i]:dateInfo$sampleSegEnd[i])
    sampleSegments <- c(dateInfo[i,sampleStart]:dateInfo[i,"sampleSegEnd"])
    DailyFN$FNConc[DailyFN$WaterYear %in% sampleSegments] <- dailyReturn$FNConc[dailyReturn$WaterYear %in% sampleSegments]
    DailyFN$FNFlux[DailyFN$WaterYear %in% sampleSegments] <- dailyReturn$FNFlux[dailyReturn$WaterYear %in% sampleSegments]
  }
  
  INFO <- eList$INFO
  
  INFO$nSegments <- nrow(dateInfo)
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,DailyFN,Sample,eList$surfaces)
  
  return(newList)
  
}


#' Segment estimates for flow normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo dataframe with sampleSegStart, flowSegStart, flowSegEnd, sampleSegEnd
#' @importFrom fields interp.surface
#' @importFrom dataRetrieval calcWaterYear
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleSegStart <- c(1980,1990,2000)
#' flowSegStart <- c(1980,1985,1992)
#' flowSegEnd <- c(1994,2004,2011)
#' dateInfo <- dateInfo <- data.frame(sampleSegStart, 
#'                                    flowSegStart, 
#'                                    flowSegEnd)
#' dateInfo$sampleSegEnd <- c(dateInfo$sampleSegStart[2:nrow(dateInfo)]-1,
#' floor(max(eList$Sample$DecYear)))
#' 
#' eList$Daily$WaterYear <- dataRetrieval::calcWaterYear(eList$Daily$Date)
#' eList$Sample$WaterYear <- dataRetrieval::calcWaterYear(eList$Sample$Date)
#' eList <- estFNsegs(eList,dateInfo[1,])
estFNsegs <- function(eList, dateInfo){
  
  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  localsurfaces <- getSurfaces(eList)
  
  # First argument in calls below is the "known" x-y-z surface, second argument is matrix of 
  # "target" x-y points.
  LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  localDaily$yHat <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,1]), 
                                    loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$SE <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,2]), 
                                  loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$ConcDay <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$FluxDay <- as.numeric(localDaily$ConcDay * localDaily$Q * 86.4)
  
  # Calculate "flow-normalized" concentration and flux:
  sampleIndex <- localDaily$WaterYear >= dateInfo$sampleSegStart & localDaily$WaterYear <= dateInfo$sampleSegEnd
  flowIndex <- localDaily$WaterYear >= dateInfo$flowSegStart & localDaily$WaterYear <= dateInfo$flowSegEnd
 
  # First, bin the LogQ values by day-of-year.
  allLogQsByDayOfYear <- split(localDaily$LogQ[flowIndex], localDaily$Day[flowIndex])
  
  allLogQsByDayOfYear[['59']] <- c(unlist(allLogQsByDayOfYear['59']),   # Bob's convention
                                   unlist(allLogQsByDayOfYear['60']))
  allLogQsByDayOfYear['60'] <- allLogQsByDayOfYear['59']
  
  # Using the above data structure as a "look-up" table, list all LogQ values that occured on every
  # day of the entire daily record. When "unlisted" into a vector, these will become the "x" values 
  # for the interpolation.
  allLogQsReplicated <- allLogQsByDayOfYear[localDaily$Day]
  
  # Replicate the decimal year field for each day of the record to correspond to all the LogQ 
  # values listed for that day. These are the "y" values for the interpolation.
  allDatesReplicated <- rep(localDaily$DecYear, lapply(allLogQsReplicated, length))
   
  # Interpolate.
  allConcReplicated <- interp.surface( obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(unlist(x=allLogQsReplicated),
                                                       y=allDatesReplicated))
  allFluxReplicated <- allConcReplicated * exp(unlist(allLogQsReplicated)) * 86.4
  
  # Finally bin the collective results by days (the decimal year), and calculate the desired means.
  localDaily$FNConc[sampleIndex] <-  as.numeric(tapply(allConcReplicated, allDatesReplicated, "mean"))[sampleIndex]
  localDaily$FNFlux[sampleIndex] <-  as.numeric(tapply(allFluxReplicated, allDatesReplicated, "mean"))[sampleIndex]

  return(localDaily)

}



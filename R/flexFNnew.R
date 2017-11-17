#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo data frame with 3 columns, their names defined by sampleStart, flowStart, flowEnd
#' @param waterYear logical. Should years be water years (\code{TRUE}) or calendar years (\code{FALSE})
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
#' flexPlotAddOn(eList)
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
#' flexPlotAddOn(eList)
#' 
#' eList <- flexFN(eList, dateInfo, waterYear = FALSE)
#' plotFluxHist(eList)
#' flexPlotAddOn(eList)
#' }
flexFN <- function(eList, dateInfo, waterYear = TRUE,sampleStart="sampleSegStart",
                   flowStart="flowSegStart", flowEnd="flowSegEnd"){
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  if(waterYear){
    Sample$WaterYear <- calcWaterYear(Sample$Date)
    Daily$WaterYear <- calcWaterYear(Daily$Date)    
  } else {
    Sample$WaterYear <- floor(Sample$DecYear)
    Daily$WaterYear <- floor(Daily$DecYear)
  }
  
  dateInfo$sampleSegEnd <- c(dateInfo[2:nrow(dateInfo),sampleStart]-1,max(Sample$WaterYear))

  newList <- as.egret(eList$INFO,Daily,Sample,eList$surfaces)
  
  for(i in seq(nrow(dateInfo))){
    sampleSegments <- c(dateInfo[[sampleStart]][i]:dateInfo[["sampleSegEnd"]][i])
    
    sampleIndex <- which(Daily$WaterYear %in% sampleSegments)
    flowIndex <- which(Daily$WaterYear >= dateInfo$flowSegStart[i] & Daily$WaterYear <= dateInfo$flowSegEnd[i])
    
    allLogQsByDayOfYear <- bin_Qs(Daily[flowIndex,])
    dailyReturn_list <- getConcFluxFromSurface(newList, allLogQsByDayOfYear, Daily)

    Daily$FNConc[sampleIndex] <- as.numeric(tapply(dailyReturn_list[["allConcReplicated"]], dailyReturn_list[["allDatesReplicated"]], "mean"))[sampleIndex]
    Daily$FNFlux[sampleIndex] <- as.numeric(tapply(dailyReturn_list[["allFluxReplicated"]], dailyReturn_list[["allDatesReplicated"]], "mean"))[sampleIndex]
  }
  
  INFO <- eList$INFO
  
  INFO$nSegments <- nrow(dateInfo)
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  if(!waterYear){
    Daily$WaterYear <- calcWaterYear(Daily$Date)
  }
  
  newList <- as.egret(INFO,Daily,Sample,eList$surfaces)
  
  return(newList)
  
}


#' subFN
#' 
#' Calculates flow normalized flux and concentration with a subset of the flow record.
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param flowNormYears vector of flow years
#' @param waterYear logical. Should years be water years (\code{TRUE}) or calendar years (\code{FALSE})
#' @return data frame in the Daily format
#' @export
#' @examples
#' eList <- Choptank_eList
#' 
#' flowNormYears <- c(1985:2002,2006:2010)
#' temp_daily <- subFN(eList, flowNormYears)
#' plotFluxHist(eList, flowNormYears =  c(1985:2002,2006:2010))
subFN <- function(eList, flowNormYears = "all", 
                  waterYear = TRUE){
  
  if(any(tolower(flowNormYears) != "all")){

    flow_splits <- split(flowNormYears, cumsum(c(1, diff(flowNormYears) != 1)))
    
    flow_ranges <- sapply(flow_splits, range)
    
    dateInfo <- data.frame(flowSegStart = as.integer(flow_ranges[1,]),
                           flowSegEnd = as.integer(flow_ranges[2,]),
                           sampleSegStart = as.integer(flow_ranges[1,]))
    
    localDaily <- getDaily(eList)
    localINFO <- getInfo(eList)
    localsurfaces <- getSurfaces(eList)
    
    if(waterYear){
      localDaily$WaterYear <- calcWaterYear(localDaily$Date)
    } else {
      localDaily$WaterYear <- floor(localDaily$DecYear)
    }
    
    # Calculate "flow-normalized" concentration and flux:
    flowIndex <- localDaily$WaterYear >= dateInfo$flowSegStart & localDaily$WaterYear <= dateInfo$flowSegEnd
    
    allLogQsByDayOfYear <- bin_Qs(localDaily[flowIndex,])
    estFN <- getConcFluxFromSurface(eList, allLogQsByDayOfYear, localDaily)

    localDaily$FNConc <-  as.numeric(tapply(estFN[["allConcReplicated"]], estFN[["allDatesReplicated"]], "mean"))
    localDaily$FNFlux <-  as.numeric(tapply(estFN[["allFluxReplicated"]], estFN[["allDatesReplicated"]], "mean"))
      
  } else {
    localDaily <- estDailyFromSurfaces(eList = eList)
  }
  
  return(localDaily)
}



#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo data frame with 4 columns, their names defined by sampleStart, sampleEnd, flowStart, flowEnd
#' @param sampleStart integer vector of start years (water) for each FN conc/flux segment
#' @param flowStart integer vector of start years (water) for flow normalization
#' @param flowEnd integer vector of end years (water) for flow normalization
#' @export
#' @importFrom dataRetrieval calcWaterYear
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleStart <- c("1980-01-01","1990-01-01","2000-01-01")
#' sampleEnd <- c("1995-06-06","2002-09-30","2011-02-01")
#' flowStart <- c("1980-02-01","1985-06-01","1992-10-10")
#' flowEnd <- c("1994-06-15","2004-03-03","2009-03-15")
#' dateInfo <- data.frame(sampleStart,
#'                        sampleEnd,
#'                        flowStart, 
#'                        flowEnd, stringsAsFactors = FALSE)
#' \dontrun{
#' newEList <- flexFN(eList, dateInfo)
#' plotFluxHist(newEList)
#' flexPlotAddOn(newEList)
#' }
flexFN <- function(eList, dateInfo, 
                   sampleStartCol="sampleStart", sampleEndCol="sampleEnd",
                   flowStartCol="flowStart", flowEndCol="flowEnd"){
  
  localDaily <- getDaily(eList)
  localSample <- getSample(eList)
  
  localDaily$FNConc <- NA
  localDaily$FNFlux <- NA
  
  surfaceList <- list()
  
  for(seg in seq_len(nrow(dateInfo))){
    
    segSampleIndex <- which(localSample$Date >= as.Date(dateInfo[[sampleStartCol]][seg]) & 
                              localSample$Date <= as.Date(dateInfo[[sampleEndCol]][seg]))
    segSample <- localSample[segSampleIndex,]
    
    surfaceSeg <- estSurfaces(eList, 
                              surfaceStart = dateInfo[[sampleStartCol]][seg], 
                              surfaceEnd = dateInfo[[sampleEndCol]][seg],
                              localSample = segSample)
    
    segIndex <- which(localDaily$Date >= as.Date(dateInfo[[flowStartCol]][seg]) & 
                        localDaily$Date <= as.Date(dateInfo[[flowEndCol]][seg]))
    
    DailySeg <- localDaily[segIndex,]
    DailySeg <- estDailyFromSurfaces(eList, localsurfaces = surfaceSeg, localDaily = DailySeg)
    
    surfaceList[[seg]] <- surfaceSeg
    
    localDaily$FNConc[segIndex[!is.na(DailySeg$FNConc)]] <- DailySeg$FNConc[!is.na(DailySeg$FNConc)]
    localDaily$FNFlux[segIndex[!is.na(DailySeg$FNConc)]] <- DailySeg$FNFlux[!is.na(DailySeg$FNFlux)]
  }
  
  INFO <- eList$INFO
  
  INFO$nSegments <- nrow(dateInfo)
  
  #Rename dateInfo cols so that they are internally consistent now:
  names(dateInfo)[names(dateInfo) == sampleStartCol] <- "sampleStart"
  names(dateInfo)[names(dateInfo) == sampleEndCol] <- "sampleEnd"
  names(dateInfo)[names(dateInfo) == flowStartCol] <- "flowStart"
  names(dateInfo)[names(dateInfo) == flowEndCol] <- "flowEnd"
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,localDaily,eList$Sample,NA)
  attr(newList, "surfaceLists") <- surfaceList
  
  return(newList)
  
}


#' Flexible Flow Normalization Plot Add On
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param showArrows logical whether or not to show arrows representing flow segments
#' @param showRect logical whether or not to show rectangles representing sample segments
#' @param customPalette character vector of colors as a hexadecimal string of the form "#rrggbb". 
#' Defaults to NULL, which indicates the use of a default palette (up to 21 segments).
#' @export
#' @importFrom graphics rect
#' @importFrom graphics arrows
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' sampleStart <- c("1980-01-01","1990-01-01","2000-01-01")
#' sampleEnd <- c("1995-06-06","2002-09-30","2011-02-01")
#' flowStart <- c("1980-02-01","1985-06-01","1992-10-10")
#' flowEnd <- c("1994-06-15","2004-03-03","2011-02-01")
#' dateInfo <- data.frame(sampleStart,
#'                        sampleEnd,
#'                        flowStart, 
#'                        flowEnd, stringsAsFactors = FALSE)
#' \dontrun{
#' newEList <- flexFN(eList, dateInfo)
#' plotFluxHist(newEList)
#' flexPlotAddOn(newEList)
#' 
#' plotFluxHist(newEList)
#' flexPlotAddOn(eList, customPalette=c("#d5ce48", "#fd300f", "#3e0289"))
#' }
flexPlotAddOn <- function(eList, showArrows = TRUE, showRect = TRUE, customPalette = NULL){
  if('segmentInfo' %in% names(attributes(eList$INFO))){
    segmentINFO <- attr(eList$INFO, "segmentInfo")
    
    if(!is.null(customPalette)){
      pal <- customPalette
    } else {
      pal <- c("#1856fb", "#af2b18", "#fdd76a", "#013919", "#a4927c", "#16f9d1", 
               "#a40e0e", "#089db6", "#edc56f", "#13ad5f", "#b26d63", "#5e6c9c", 
               "#c07a62", "#4b4c13", "#11d8be", "#435749", "#ae5175", "#88756c", 
               "#628490", "#8f07e4", "#8e3f98")
    }
    
    if(nrow(segmentINFO) > length(pal)){
      stop(paste("The number of segments exceed the length of the color palette.", 
                 "Supply custom palette of length", nrow(segmentINFO)))
    }
    
    colors <- suppressWarnings(pal[seq_len(nrow(segmentINFO))])
    
    arrowYs <- seq(par()$usr[4], par()$usr[3], length=10)[c(-1,-10)]
    
    segmentINFO <- data.frame(sapply(segmentINFO, function(x) lubridate::decimal_date(as.Date(x, origin = "1970-01-01"))))
    
    if(nrow(segmentINFO) > 8){
      arrowYs <- rep(arrowYs,  ceiling(nrow(segmentINFO)/8))
      colors <- rep(colors, ceiling(nrow(segmentINFO)/8))
    }
    
    for(i in seq_len(nrow(segmentINFO))){
      
      if(showRect){
        rect(segmentINFO$sampleStart[i], par()$usr[3], 
             segmentINFO$sampleEnd[i], par()$usr[4],
             col= paste0(colors[i],"50"))         
      }
      
      if(showArrows){
        arrows(segmentINFO$flowStart[i], arrowYs[i], 
               segmentINFO$flowEnd[i]+1, arrowYs[i], code=3)
      }
    }
    
  }
}

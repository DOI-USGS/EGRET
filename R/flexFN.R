#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo data frame with 4 columns. The column names and descriptions are described in the next set of arguments
#' @param localsurfaces surface over-riding the one stored in eList
#' @param flowNormStartCol character, name of the column in dateInfo that starts the segment
#' for the flow normalization
#' @param flowNormEndCol character, name of the column in dateInfo that ends the segment
#' for the flow normalization
#' @param flowStartCol character, name of the column in dateInfo that starts the segment
#' for the portion of the flow to be populated with flow-normalized values.
#' @param flowEndCol character, name of the column in dateInfo that ends the segment
#' for the portion of the flow to be populated with flow-normalized values.
#' @export
#' @importFrom dataRetrieval calcWaterYear
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' flowNormStart <- c("1979-10-01","1990-01-01","1992-10-10")
#' flowNormEnd <- c("1995-06-06","2004-03-03","2011-09-29")
#' flowStart <- c("1979-10-01","1995-06-07","2004-03-04")
#' flowEnd <- c("1995-06-06","2004-03-03","2011-09-29") 
#' dateInfo <- data.frame(flowNormStart,
#'                        flowNormEnd,
#'                        flowStart, 
#'                        flowEnd, 
#'                        stringsAsFactors = FALSE)
#' \dontrun{
#' newEList <- flexFN(eList, dateInfo)
#' plotFluxHist(newEList)
#' flexPlotAddOn(newEList)
#' 
#' wallSurface <- estSurfaces(eList, localSample = eList$Sample[1:500,])
#' wallEList <- flexFN(eList, dateInfo, localsurface = wallSurface)
#' plotFluxHist(wallEList)
#' }
flexFN <- function(eList, dateInfo, localsurfaces = NA,
                   flowNormStartCol = "flowNormStart", flowNormEndCol = "flowNormEnd",
                   flowStartCol="flowStart", flowEndCol="flowEnd"){
  
  localDaily <- getDaily(eList)
  
  localDaily$FNConc <- NA
  localDaily$FNFlux <- NA
  
  if(all(is.na(localsurfaces))){
    localsurfaces <- getSurfaces(eList)    
  }

  for(seg in seq_len(nrow(dateInfo))){

    flowIndex <- which(localDaily$Date >= as.Date(dateInfo[[flowStartCol]][seg]) & 
                        localDaily$Date <= as.Date(dateInfo[[flowEndCol]][seg]))

    flowNormIndex <- which(localDaily$Date >= as.Date(dateInfo[[flowNormStartCol]][seg]) & 
                             localDaily$Date <= as.Date(dateInfo[[flowNormEndCol]][seg]))
    
    DailySeg <- estDailyFromSurfaces(eList, localsurfaces = localsurfaces, localDaily = localDaily[flowNormIndex,])
    
    localDaily$FNConc[flowIndex] <- DailySeg$FNConc[which(flowNormIndex %in% flowIndex)]
    localDaily$FNFlux[flowIndex] <- DailySeg$FNFlux[which(flowNormIndex %in% flowIndex)]
  }
  
  INFO <- eList$INFO
  
  INFO$nSegments <- nrow(dateInfo)
  
  #Rename dateInfo cols so that they are internally consistent now:
  names(dateInfo)[names(dateInfo) == flowStartCol] <- "flowStart"
  names(dateInfo)[names(dateInfo) == flowEndCol] <- "flowEnd"
  names(dateInfo)[names(dateInfo) == flowNormStartCol] <- "flowNormStart"
  names(dateInfo)[names(dateInfo) == flowNormEndCol] <- "flowNormEnd"
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,localDaily,eList$Sample,localsurfaces)
  
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
#' flowNormStart <- c("1979-10-01","1990-01-01","1992-10-10")
#' flowNormEnd <- c("1995-06-06","2004-03-03","2011-09-29")
#' flowStart <- c("1979-10-01","1995-06-07","2004-03-04")
#' flowEnd <- c("1995-06-06","2004-03-03","2011-09-29") 
#' dateInfo <- data.frame(flowNormStart,
#'                        flowNormEnd,
#'                        flowStart, 
#'                        flowEnd, 
#'                        stringsAsFactors = FALSE)
#' \dontrun{
#' newEList <- flexFN(eList, dateInfo)
#' plotFluxHist(newEList)
#' flexPlotAddOn(newEList)
#' 
#' plotFluxHist(newEList)
#' flexPlotAddOn(newEList, customPalette=c("#d5ce48", "#fd300f", "#3e0289"))
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
        rect(segmentINFO$flowStart[i], par()$usr[3], 
             segmentINFO$flowEnd[i], par()$usr[4],
             col= paste0(colors[i],"50"))         
      }
      
      if(showArrows){
        arrows(segmentINFO$flowNormStart[i], arrowYs[i], 
               segmentINFO$flowNormEnd[i]+1, arrowYs[i], code=3)
      }
    }
    
  }
}

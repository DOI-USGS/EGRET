#' Flexible Flow Normalization
#' 
#' This function implements generalized flow normalization.  This means that for 
#' determining the  flow normalized concentration and flow normalized flux for any 
#' given year, there is a specified list of years from which to create the discharge 
#' record used in the flow-normalization process.  That set of years is defined by 
#' the dateInfo object.
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param dateInfo data frame with 4 columns. The column names and descriptions 
#' are described below.  Default is NA.
#' @param localsurfaces surface  (3-dimensional matrix) over-riding the one stored in 
#' eList Default = NA.
#' @param flowNormStartCol character, name of the column in dateInfo that starts the segment
#' for the flow normalization
#' @param flowNormEndCol character, name of the column in dateInfo that ends the segment
#' for the flow normalization
#' @param flowStartCol character, name of the column in dateInfo that starts the segment
#' for the portion of the flow to be populated with flow-normalized values.
#' @param flowEndCol character, name of the column in dateInfo that ends the segment
#' for the portion of the flow to be populated with flow-normalized values.
#' @param oldSurface logical, if TRUE, use the surface object in eList.  Default is FALSE.
#' @export
#' @return named list, eList, containing INFO, Daily, Sample, and surfaces objects
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
flexFN <- function(eList, dateInfo, localsurfaces = NA, oldSurface = FALSE,
                   flowNormStartCol = "flowNormStart", flowNormEndCol = "flowNormEnd",
                   flowStartCol="flowStart", flowEndCol="flowEnd"){
  
  localDaily <- getDaily(eList)
  
  if(!oldSurface){
    localDaily$ConcDay <- NA
    localDaily$FluxDay <- NA    
  }

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
    if(oldSurface){
      DailySeg <- localDaily[flowNormIndex,]
      
      allLogQsByDayOfYear <- bin_Qs(DailySeg)
      
      concFlux_list <- getConcFluxFromSurface(eList, allLogQsByDayOfYear, localDaily = DailySeg, localsurfaces=localsurfaces)
      
      # Finally bin the collective results by days (the decimal year), and calculate the desired means.
      DailySeg$FNConc <-  as.numeric(tapply(concFlux_list[["allConcReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))
      DailySeg$FNFlux <-  as.numeric(tapply(concFlux_list[["allFluxReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))
      
    } else {
      DailySeg <- estDailyFromSurfaces(eList, localsurfaces = localsurfaces, localDaily = localDaily[flowNormIndex,])
      
      localDaily$ConcDay[flowIndex] <- DailySeg$ConcDay[which(flowNormIndex %in% flowIndex)]
      localDaily$FluxDay[flowIndex] <- DailySeg$FluxDay[which(flowNormIndex %in% flowIndex)]      
    }

    localDaily$FNConc[flowIndex] <- DailySeg$FNConc[which(flowNormIndex %in% flowIndex)]
    localDaily$FNFlux[flowIndex] <- DailySeg$FNFlux[which(flowNormIndex %in% flowIndex)]
  }
  
  INFO <- eList$INFO
  INFO$nVectorYear <- attr(eList$surfaces, "surfaceIndex") [["nVectorYear"]]
  INFO$bottomYear <- attr(eList$surfaces, "surfaceIndex") [["bottomYear"]]
  INFO$stepYear <- attr(eList$surfaces, "surfaceIndex") [["stepYear"]]
  INFO$nVectorLogQ <- attr(eList$surfaces, "surfaceIndex") [["nVectorLogQ"]]
  INFO$bottomLogQ <- attr(eList$surfaces, "surfaceIndex") [["bottomLogQ"]]
  INFO$stepLogQ <- attr(eList$surfaces, "surfaceIndex") [["stepLogQ"]]
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
      if(nrow(segmentINFO) <= 21){
        pal <- c("#1856fb", "#af2b18", "#fdd76a", "#013919", "#a4927c", "#16f9d1", 
                 "#a40e0e", "#089db6", "#edc56f", "#13ad5f", "#b26d63", "#5e6c9c", 
                 "#c07a62", "#4b4c13", "#11d8be", "#435749", "#ae5175", "#88756c", 
                 "#628490", "#8f07e4", "#8e3f98")
      } else if(nrow(segmentINFO) <= 74){
        pal <- c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F",
                 "#BF5B17","#666666","#1B9E77","#D95F02","#7570B3","#E7298A",
                 "#66A61E","#E6AB02","#A6761D","#666666","#A6CEE3","#1F78B4",
                 "#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00",
                 "#CAB2D6","#6A3D9A","#FFFF99","#B15928","#FBB4AE","#B3CDE3",
                 "#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC",
                 "#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9",
                 "#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A",
                 "#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999",
                 "#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F",
                 "#E5C494","#B3B3B3","#8DD3C7","#FFFFB3","#BEBADA","#FB8072",
                 "#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
                 "#CCEBC5","#FFED6F")
      } else {
        stop(paste("The number of segments exceed the length of the color palette.", 
                   "Supply custom palette of length", nrow(segmentINFO)))
      }
    }

    colors <- suppressWarnings(pal[seq_len(nrow(segmentINFO))])
    
    arrowYs <- seq(par()$usr[4], par()$usr[3], length=10)[c(-1,-10)]
    
    segmentINFO[["flowNormStart"]] <- decimalDate(as.Date(segmentINFO[["flowNormStart"]], origin = "1970-01-01"))
    segmentINFO[["flowStart"]] <- decimalDate(as.Date(segmentINFO[["flowStart"]], origin = "1970-01-01"))
    segmentINFO[["flowNormEnd"]] <- decimalDate(as.Date(segmentINFO[["flowNormEnd"]], origin = "1970-01-01"))
    segmentINFO[["flowEnd"]] <- decimalDate(as.Date(segmentINFO[["flowEnd"]], origin = "1970-01-01"))
    
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

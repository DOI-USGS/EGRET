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
#' sampleSegStart <- c(1980,1990,2000)
#' flowSegStart <- c(1980,1985,1992)
#' flowSegEnd <- c(1994,2004,2011)
#' dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
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
#' dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
#' eList <- flexFN(eList, dateInfo)
#' plotFluxHist(eList)
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
    
    if(nrow(segmentINFO) > 8){
      arrowYs <- rep(arrowYs,  ceiling(nrow(segmentINFO)/8))
      colors <- rep(colors, ceiling(nrow(segmentINFO)/8))
    }
    
    for(i in 1:nrow(segmentINFO)){
      
      if(showRect){
        rect(segmentINFO$sampleSegStart[i], par()$usr[3], 
             segmentINFO$sampleSegEnd[i]+1, par()$usr[4],
             col= paste0(colors[i],"50"))         
      }

      if(showArrows){
        arrows(segmentINFO$flowSegStart[i], arrowYs[i], 
               segmentINFO$flowSegEnd[i]+1, arrowYs[i], code=3)
      }
    }
    
  }
}
#' Flexible Flow Normalization Plot Add On
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param showArrows logical whether or not to show arrows representing flow segments
#' @param showRect logical whether or not to show rectangles representing sample segments
#' @export
#' @importFrom RColorBrewer brewer.pal
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
#' flexPlotAddOn(eList)
#' }
flexPlotAddOn <- function(eList, showArrows = TRUE, showRect = TRUE){
  if('segmentInfo' %in% names(attributes(eList$INFO))){
    segmentINFO <- attr(eList$INFO, "segmentInfo")
    
    colors <- suppressWarnings(brewer.pal(nrow(segmentINFO), "Accent"))
    
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
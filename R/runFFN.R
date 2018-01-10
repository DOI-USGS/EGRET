#' runFFN
#' 
#' runFFN
#' 
#' @param eList named list with at least Daily and INFO dataframes
#' @param dateInfo data frame with 4 columns. The column names and descriptions are described in the next set of arguments
#' @param verbose logical specifying whether or not to display progress message
#' @export
#' 
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
#' AnnualResultsFlex <- runFFN(eList, dateInfo)
#' 
#' eListWinter <- setPA(eList, paStart = 12, paLong=3)
#' AnnualResultsFlexWinter <- runFFN(eList, dateInfo)
runFFN <- function(eList, dateInfo, verbose = TRUE) {

  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  # after running seriesSetUpAlt
  # note that this lacks any consideration of the wall
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

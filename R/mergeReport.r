#' Merge Sample and Daily Data for WRTDS
#'
#' Merges the flow data from the daily record into the sample record.
#'
#' @param INFO dataframe containing the INFO dataframe
#' @param Daily dataframe containing the daily data
#' @param Sample dataframe containing the sample data
#' @param surfaces matrix returned from \code{modelEstimation}. Default is NA. 
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return eList named list with Daily, Sample, and INFO dataframes, along with the surfaces matrix.
#' Any of these values can be NA, not all EGRET functions will work with missing parts of the named list eList.
#' @seealso \code{\link{readNWISDaily}}, \code{\link{readNWISSample}}
#' @examples
#' siteNumber <- '01594440'
#' pCode <- '01075'
#' Daily <- readNWISDaily(siteNumber,'00060', '1985-01-01', '1990-03-31')
#' Sample <- readNWISSample(siteNumber,pCode, '1985-01-01', '1990-03-31')
#' INFO <- readNWISInfo(siteNumber,pCode,interactive=FALSE)
#' eList <- mergeReport(INFO, Daily, Sample)
#' Sample <- eList$Sample
#' 
#' Daily2 <- ChopDaily
#' Sample2 <- ChopSample
#' INFO2 <- ChopINFO
#' surfaces2 <- exsurfaces
#' eList2 <- mergeReport(INFO2, Daily2, Sample2, surfaces2, FALSE)
#' is.egret(eList2)
#' eList2
mergeReport<-function(INFO, Daily, Sample, surfaces=NA, interactive=FALSE){
  
  if (interactive){
    dataOverview(Daily, Sample)  
  }
  
  if(!all(is.na(Sample)) & !all(is.na(Daily))){
    Sample <- merge(Daily[,c("Date","Q","LogQ")],Sample,by = "Date",all.y = TRUE)
  }
  
  eList <- as.egret(INFO, Daily, Sample, surfaces)
  
  return(eList)
}


#' @export
as.egret <- function(INFO, Daily, Sample, surfaces) {
  eList <- list(INFO=INFO, 
                Daily=Daily, 
                Sample=Sample, 
                surfaces=surfaces)
  
  attr(eList, "param.units") <- INFO$param.units
  attr(eList, "shortName") <- INFO$shortName
  attr(eList, "paramShortName") <- INFO$paramShortName
  attr(eList, "constitAbbrev") <- INFO$constitAbbrev
  attr(eList, "drainSqKm") <- INFO$drainSqKm
  
  class(eList) <- "egret"
  invisible(eList)
}

print.egret <- function(x, ...){
  
  localDaily <- daily(x)
  localSample <- sample(x)
  localINFO <- info(x)
  
  if(!all(is.na(x$Daily))){
    cat("Daily discharge:\n")
    print(head(localDaily[,c("Date","Q","Qualifier")]))
    cat("...\n")
    print(tail(localDaily[,c("Date","Q","Qualifier")]))
  }
  if(!all(is.na(x$Sample))){
    cat("\nSample data:\n")
    print(head(localSample[,c("Date","ConcLow","ConcHigh","Q")]))
    cat("...\n")
    print(tail(localSample[,c("Date","ConcLow","ConcHigh","Q")]))
  }
  cat("\n",attr(x, "shortName"), ":",attr(x, "paramShortName"),"\n",sep="")
  cat("Parameter units:", attr(x, "param.units"),"\n")
  cat("Drainage area:", attr(x, "drainSqKm"), "km^2\n")      

}


is.egret <- function(x) {
  inherits(x, "egret")
}

daily <- function(x, ...){
  UseMethod("daily")
}

daily.egret <- function(x, ...){
  Daily <- x$Daily
  return(Daily)
}

daily.default <- function(x, ...){
  if("Daily" %in% names(x)){
    return(x$Daily)
  } else {
    stop("Please provide a named list that includes a Daily dataframe")
  }
}

info <- function(x, ...){
  UseMethod("info")
}

info.egret <- function(x, ...){
  INFO <- x$INFO
  return(INFO)
}

info.default <- function(x, ...){
  if("INFO" %in% names(x)){
    return(x$INFO)
  } else {
    stop("Please provide a named list that includes a INFO dataframe")
  }
}

sample <- function(x, ...){
  UseMethod("sample")
}

sample.egret <- function(x, ...){
  Sample <- x$Sample
  return(Sample)
}

sample.default <- function(x, ...){
  if("Sample" %in% names(x)){
    return(x$Sample)
  } else {
    stop("Please provide a named list that includes a Sample dataframe")
  }
}

surfaces <- function(x, ...){
  UseMethod("surfaces")
}

surfaces.egret <- function(x, ...){
  surfaces <- x$surfaces
  return(surfaces)
}

surfaces.default <- function(x, ...){
  if("surfaces" %in% names(x)){
    return(x$surfaces)
  } else {
    stop("Please provide a named list that includes a surfaces matrix")
  }
}

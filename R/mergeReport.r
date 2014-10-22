#' Merge Sample and Daily Data for WRTDS
#'
#' Merges the flow data from the daily record into the sample record.
#'
#' @param INFO dataframe containing the INFO dataframe
#' @param Daily dataframe containing the daily data
#' @param Sample dataframe containing the sample data
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return newSample dataframe with merged flow information
#' @seealso \code{\link{getNWISDaily}}, \code{\link{getNWISSample}}
#' @examples
#' siteNumber <- '01594440'
#' pCode <- '01075'
#' Daily <- getNWISDaily(siteNumber,'00060', '1985-01-01', '1985-03-31')
#' Sample <- getNWISSample(siteNumber,pCode, '1985-01-01', '1985-03-31')
#' INFO <- getNWISInfo(siteNumber,pCode,interactive=FALSE)
#' returnedList <- mergeReport(INFO, Daily, Sample)
#' Sample <- returnedList$Sample
mergeReport<-function(INFO, Daily, Sample, interactive=TRUE){
  
  if (interactive){
    dataOverview(Daily, Sample)  
  }
  
  newSample <- merge(Daily[,c("Date","Q","LogQ")],Sample,by = "Date",all.y = TRUE)
  returnList <- list(INFO=INFO, 
                     Daily=Daily, 
                     Sample=newSample, 
                     surfaces=NA)
  return(returnList)
}

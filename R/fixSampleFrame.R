#' Update Sample dataframe
#'
#' Used for updating the Sample dataframe if ConcLow or ConcHigh is manually adjusted. 
#' Adjusts ConcAve and Uncen columns.
#'
#' @param eList named list with at least the Sample dataframes
#' @keywords WRTDS flow
#' @return localSample data frame
#' @export
#' @examples
#' eList <- Choptank_eList
#' Sample <- eList$Sample
#' Sample[1,c("ConcLow","ConcHigh")] <- c(NA, 0.01) # Adjusted to left-censored
#' Sample[2,c("ConcLow","ConcHigh")] <- c(1.1, 1.3) # Adjusted to interval-censored
#' Sample[3,c("ConcLow","ConcHigh")] <- c(1.3, 1.3) # Simple adjustment
#' eList$Sample <- Sample
#' eList <- fixSampleFrame(eList)
#' eList$Sample[1:3,]
fixSampleFrame<-function(eList) {
	localSample <- getSample(eList)
  localSample$ConcAve <- ifelse(is.na(localSample$ConcLow), 
                                localSample$ConcHigh/2, 
                                (localSample$ConcLow+localSample$ConcHigh)/2)
	localSample$Uncen <- ifelse(localSample$ConcHigh==localSample$ConcAve, 1, 0)
  eList$Sample <- localSample
	return(eList)
}
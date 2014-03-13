#' Update Sample dataframe
#'
#' Used for updating the Sample dataframe if ConcLow or ConcHigh is manually adjusted. Adjusts ConcAve and Uncen columns.
#'
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @keywords WRTDS flow
#' @return localSample data frame
#' @export
#' @examples
#' Sample <- ChopSample
#' Sample[1,c("ConcLow","ConcHigh")] <- c(NA, 0.01) # Adjusted to left-censored
#' Sample[2,c("ConcLow","ConcHigh")] <- c(1.1, 1.3) # Adjusted to interval-censored
#' Sample[3,c("ConcLow","ConcHigh")] <- c(1.3, 1.3) # Simple adjustment
#' Sample <- fixSampleFrame()
#' Sample[1:3,]
fixSampleFrame<-function(localSample=Sample) {
	localSample$ConcAve <- ifelse(is.na(localSample$ConcLow), 
                                localSample$ConcHigh/2, 
                                (localSample$ConcLow+localSample$ConcHigh)/2)
	localSample$Uncen <- ifelse(localSample$ConcHigh==localSample$ConcAve, 1, 0)
	return(localSample)
}
#' Import user daily data for EGRET analysis
#'
#' Imports data from a user-supplied file, and converts it to a Daily data frame, appropriate for WRTDS calculations.
#'
#' @param filePath character specifying the path to the file
#' @param fileName character name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells
#' @param qUnit number 1 is cubic feet per second, 2 is cubic meters per second, 3 is 10^3 cubic feet per second, and 4 is 10^3 cubic meters per second
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import file
#' @keywords data import USGS WRTDS
#' @export
#' @return Daily dataframe
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' fileName <- "ChoptankRiverFlow.txt"
#' Daily <- readUserDaily(filePath,fileName,separator="\t")
readUserDaily <- function (filePath,fileName,hasHeader=TRUE,separator=",",qUnit=1,interactive=TRUE){
  data <- readDataFromFile(filePath,fileName,hasHeader=hasHeader,separator=separator)
  convertQ<-c(35.314667,1,0.035314667,0.001)
  qConvert<-convertQ[qUnit]
  if (interactive){
    if(qUnit==1) cat("\n the input discharge are assumed to be in cubic feet per second\nif they are in cubic meters per second, then the call to readUserDaily should specify qUnit=2\n")
  }
  names(data) <- c("dateTime", "value")
  localDaily <- populateDaily(data,qConvert, interactive=interactive)
  localDaily <- localDaily[!is.na(localDaily$Q),]
  return(localDaily)
}

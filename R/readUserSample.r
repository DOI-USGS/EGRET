#' Import user sample data for EGRETdemo analysis
#'
#' Imports data from a user-supplied file, and converts it to a Sample data frame (including summing multiple constituents), appropriate for WRTDS calculations. 
#'
#' @param filePath string specifying the path to the file
#' @param fileName string name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator string character that separates data cells
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import file
#' @seealso \code{\link{compressData}}, \code{\link{populateSampleColumns}}
#' @export
#' @return Sample dataframe
#' @examples
#' filePath <- system.file("extdata", package="EGRETdemo")
#' fileName <- 'ChoptankRiverNitrate.csv'
#' Sample <- readUserSample(filePath,fileName, separator=";",interactive=FALSE)
readUserSample <- function (filePath,fileName,hasHeader=TRUE,separator=",", interactive=TRUE){
  data <- readDataFromFile(filePath,fileName,hasHeader=hasHeader,separator=separator)
  compressedData <- compressData(data, interactive=interactive)
  Sample <- populateSampleColumns(compressedData)
  return(Sample)
}

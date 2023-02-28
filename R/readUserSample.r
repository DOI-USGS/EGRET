#' Import user-supplied sample data for EGRET analysis
#'
#' Imports data from a user-supplied file, and converts it to a Sample data frame 
#' (including summing multiple constituents), appropriate for EGRET analysis. 
#' First column is date, second is remark code, and third is value. If multiple constituents 
#' are to be combined with interval censoring, additional pairs of columns can be inserted, each pair starting with
#' remark code (specifically looking for <), followed by the values. If the date format is not automatically
#' detected, the format that is expected is "MM\/DD\/YYYY". 
#'
#' @param filePath character specifying the path to the file
#' @param fileName character name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells. , default is ","
#'  which is separator used in a .csv file.
#' @param verbose logical specifying whether or not to display progress message
#' @param interactive logical deprecated. Use 'verbose' instead
#' @keywords data import file
#' @seealso \code{\link{compressData}}, \code{\link{populateSampleColumns}}
#' @export
#' @return A data frame 'Sample' with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' Date \tab Date \tab Date \cr
#' ConcLow \tab numeric \tab Lower limit of concentration \cr
#' ConcHigh \tab numeric \tab Upper limit of concentration \cr
#' Uncen \tab integer \tab Uncensored data (1=TRUE, 0=FALSE) \cr
#' ConcAve \tab numeric \tab Average concentration \cr
#' Julian \tab integer \tab Number of days since Jan. 1, 1850\cr
#' Month \tab integer \tab Month of the year [1-12] \cr 
#' Day \tab integer \tab Day of the year [1-366] \cr
#' DecYear \tab numeric \tab Decimal year \cr
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \cr
#' SinDY \tab numeric \tab Sine of the DecYear \cr
#' CosDY \tab numeric \tab Cosine of the DecYear
#' }
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' fileName <- 'ChoptankRiverNitrate.csv'
#' Sample <- readUserSample(filePath,fileName, separator=";",verbose=FALSE)
readUserSample <- function (filePath,fileName,hasHeader=TRUE,separator=",", verbose=TRUE, interactive=NULL){
  
  if(!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  
  data <- readDataFromFile(filePath,fileName,hasHeader=hasHeader,separator=separator)
  compressedData <- compressData(data, verbose=verbose)
  Sample <- populateSampleColumns(compressedData)
  return(Sample)
}

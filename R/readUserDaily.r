#' Import user daily data for EGRET analysis
#'
#' Imports data from a user-supplied file, and converts it to a Daily data frame,
#'  appropriate for WRTDS calculations. The file can use most any separators
#'  as a delimiter. The default is comma separated.
#'  
#'  The first column is expected to be dates, the second column is expected
#'  to be discharge values. If the date format is not automatically
#'  detected, the format can be specified using the "format" argument.
#'
#' @param filePath character specifying the path to the file. If it 
#' is in the working directory, use ".".
#' @param fileName character name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells. The default
#' is "," for a csv file. Tab delimited would be "\\t".
#' @param qUnit number 1 is cubic feet per second, 
#' 2 is cubic meters per second, 
#' 3 is 10^3 cubic feet per second,
#'  and 4 is 10^3 cubic meters per second. The default is 1.
#' @param format character indicating the format of the date (which should
#' be in the first column). Default is "\%m\/\%d/\%Y". See \code{?strptime}
#' for options. The code will initially look for R's standard YYYY-MM-DD, and
#' check this format as a backup.
#' @param verbose logical specifying whether or not to display progress message
#' @keywords data import USGS WRTDS
#' @export
#' @return A data frame 'Daily' with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' Date \tab Date \tab Date \cr
#' Q \tab numeric \tab Discharge in m^3/s\cr
#' Julian \tab integer \tab Number of days since Jan. 1, 1850\cr
#' Month \tab integer \tab Month of the year [1-12] \cr 
#' Day \tab integer \tab Day of the year [1-366] \cr
#' DecYear \tab numeric \tab Decimal year \cr
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \cr
#' Qualifier \tab character \tab Qualifying code \cr
#' i \tab integer \tab Index of days, starting with 1 \cr
#' LogQ \tab numeric \tab Natural logarithm of Q  \cr
#' Q7 \tab numeric \tab 7 day running average of Q \cr
#' Q30 \tab numeric \tab 30 day running average of Q \cr
#' }
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' fileName <- "ChoptankRiverFlow.txt"
#' Daily <- readUserDaily(filePath,fileName,separator="\t")
readUserDaily <- function (filePath,
                           fileName,
                           hasHeader = TRUE,
                           separator = ",",
                           qUnit = 1,
                           format = "%m/%d/%Y",
                           verbose = TRUE){
  
  data <- readDataFromFile(filePath,
                           fileName,
                           hasHeader = hasHeader,
                           separator = separator,
                           format = format)
  convertQ <- c(35.314667, 1, 0.035314667, 0.001)
  qConvert<-convertQ[qUnit]

  if(qUnit==1) message("The input discharge are assumed to be in cubic feet per second, if they are in cubic meters per second, then the call to readUserDaily should specify qUnit=2")

  names(data) <- c("dateTime", "value")
  localDaily <- populateDaily(data,qConvert, verbose=verbose)
  localDaily <- localDaily[!is.na(localDaily$Q),]
  return(localDaily)
}

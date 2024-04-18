#' Basic Data Import for Water Flow Data
#'
#' Imports data from user-supplied data file. Specifically used to import water flow data for use in the EGRET package.
#' For EGRET usage, the first column is expected to be dates. If the data is daily data, then next column is 
#' expected to be the measured values. If the data is sampled data, the next column is remark codes, and the third
#' column is values. If the date format is not automatically
#' detected, the format can be specified using the "format" argument. 
#'
#' @param filePath character specifying the path to the file. If it 
#' is in the working directory, use ".".
#' @param fileName character name of file to open.
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells. The default
#' is "," for a csv file. Tab delimited would be "\\t".
#' @param format character indicating the format of the date (which should
#' be in the first column). Default is "\%m\/\%d/\%Y". See \code{?strptime}
#' for options. The code will initially look for R's standard YYYY-MM-DD, and
#' check this format as a backup.
#' @keywords data import file
#' @return retval dataframe with dateTime, value, and code columns
#' @export
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' fileName <- 'ChoptankRiverFlow.txt'
#' ChopData <- readDataFromFile(filePath,fileName, separator="\t")
readDataFromFile <- function (filePath, fileName,
                              hasHeader = TRUE, separator=",",
                              format = "%m/%d/%Y"){
  totalPath <- file.path(filePath,fileName)
  if(file.exists(totalPath)){
    retval <- utils::read.delim(  
      totalPath, 
      header = hasHeader,
      sep = separator,
      colClasses = c('character'),
      fill = TRUE, 
      comment.char = "#")
    
    if(ncol(retval) == 2){
      numCol <- 2
    } else {
      numCol <- seq(from = 3,to = ncol(retval), by = 2)
    }
    
    if(dateFormatCheck(retval[,1])){
      retval[,1] <- as.Date(retval[,1])  
    } else {
      dates <- as.Date(retval[,1], format = format)
      if(any(is.na(dates))){
        stop("Some dates are missing, EGRET functions cannot handle missing dates.
             Check the date format in the file and adjust the 'format' argument.")
      }
      retval[,1] <- dates
    }
    
    retval[,numCol] <- sapply(retval[,numCol], as.numeric)
  
    return (retval)
  } else {

    stop("File does not exist")
  }
}

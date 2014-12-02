#' Basic Data Import for Water Flow Data
#'
#' Imports data from user-supplied data file. Specifically used to import water flow data for use in the EGRET package.
#' For EGRET usage, the first column is expected to be dates. If the data is daily data, then next column is 
#' expected to be the measured values. If the data is sampled data, the next column is remark codes, and the third
#' column is values.
#'
#' @param filePath character specifying the path to the file
#' @param fileName character name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells
#' @keywords data import file
#' @return retval dataframe with dateTime, value, and code columns
#' @export
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' fileName <- 'ChoptankRiverFlow.txt'
#' ChopData <- readDataFromFile(filePath,fileName, separator="\t")
readDataFromFile <- function (filePath,fileName,hasHeader=TRUE,separator=","){
  totalPath <- file.path(filePath,fileName)
  if(file.exists(totalPath)){
    retval <- read.delim(  
      totalPath, 
      header = hasHeader,
      sep=separator,
      colClasses=c('character'),
      fill = TRUE, 
      comment.char="#")
    
    if(ncol(retval) == 2){
      numCol <- 2
    } else {
      numCol <- seq(from = 3,to = ncol(retval), by = 2)
    }
    
    if(dateFormatCheck(retval[,1])){
      retval[,1] <- as.Date(retval[,1])  
    } else {
      retval[,1] <- as.Date(retval[,1],format="%m/%d/%Y")
    }
    
    retval[,numCol] <- sapply(retval[,numCol], as.numeric)
  
    return (retval)
  } else {

    stop("File does not exist")
  }
}

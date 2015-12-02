#' Processing of USGS NWIS Water Quality Data
#'
#' Processes water quality portal data. This function looks at detection limit and detection 
#' conditions to determine if a value is left censored or not. Censored values are given the qualifier
#' "<".  The dataframe is also converted from a long to wide format.
#' 
#' @param data dataframe from Water Quality Portal
#' @param pCode logical if TRUE, assume data came from a pCode search, if FALSE, characteristic name.
#' @keywords data import USGS web service
#' @return data dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on the number of parameter codes)
#' @export
#' @seealso \code{\link[dataRetrieval]{readWQPqw}}
#' @examples
#' \dontrun{
#' library(dataRetrieval)
#' 
#' rawSample <- readWQPqw('USGS-01594440','', '', '')
#' rawSampleSelect <- processQWData(rawSample)
#' 
#' rawWQP <- readWQPqw('21FLEECO_WQX-IMPRGR80','Phosphorus', '', '')
#' Sample2 <- processQWData(rawWQP)
#' }
processQWData <- function(data,pCode=TRUE){

  detectText <- data$ResultDetectionConditionText
  detectText <- toupper(detectText)
  
  qualifier <- rep("",length(detectText))
  qualifier[grep("NON-DETECT",detectText)] <- "<"
  qualifier[grep("NON DETECT",detectText)] <- "<"
  qualifier[grep("NOT DETECTED",detectText)] <- "<"
  qualifier[grep("DETECTED NOT QUANTIFIED",detectText)] <- "<"
  qualifier[grep("BELOW QUANTIFICATION LIMIT",detectText)] <- "<"
  
  qualifier[!is.na(data$DetectionQuantitationLimitMeasure.MeasureValue) && 
              data$ResultMeasureValue < data$DetectionQuantitationLimitMeasure.MeasureValue] <- "<"
    
  correctedData<-ifelse((nchar(qualifier)==0),data$ResultMeasureValue,data$DetectionQuantitationLimitMeasure.MeasureValue)
  test <- data.frame(data$USGSPCode)
  
  test$dateTime <- data$ActivityStartDate
  
  originalLength <- nrow(test)
  test$qualifier <- qualifier
  test$value <- as.numeric(correctedData)
  
  test <- test[!is.na(test$dateTime),]
  newLength <- nrow(test)
  if (originalLength != newLength){
    numberRemoved <- originalLength - newLength
    warningMessage <- paste(numberRemoved, " rows removed because no date was specified", sep="")
    warning(warningMessage)
  }
  
  if (pCode){
    colnames(test)<- c("USGSPCode","dateTime","qualifier","value")
    newTimeVar <- "USGSPCode"
  } else {
    colnames(test)<- c("CharacteristicName","dateTime","qualifier","value")
    newTimeVar <- "CharacteristicName"
  }
  
  data <- suppressWarnings(reshape(test, idvar="dateTime", timevar = newTimeVar, direction="wide"))  
  data$dateTime <- format(data$dateTime, "%Y-%m-%d")
  data$dateTime <- as.Date(data$dateTime)
  return(data)
}

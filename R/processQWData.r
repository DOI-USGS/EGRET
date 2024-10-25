#' Processing of Water Quality Data
#'
#' Processes water quality data. This function looks at detection limit and detection 
#' conditions to determine if a value is left censored or not. Censored values are given the qualifier
#' "<".  The dataframe is also converted from a long to wide format.
#' 
#' @param data dataframe from Water Quality Portal
#' @keywords data import USGS web service
#' @return data dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on the number of parameter codes)
#' @export
#' @seealso \code{\link[dataRetrieval]{readWQPqw}}
#' @examples
#' \donttest{
#' #rawWQP <- dataRetrieval::readWQPqw('21FLEECO_WQX-IMPRGR80','Phosphorus', '', '')
#' #Sample2 <- processQWData(rawWQP)
#' }
processQWData <- function(data){

  detectText <- data$ResultDetectionConditionText
  detectText <- toupper(detectText)
  
  qualifier <- rep("",length(detectText))
  qualifier[grep("NON-DETECT",detectText)] <- "<"
  qualifier[grep("NON DETECT",detectText)] <- "<"
  qualifier[grep("NOT DETECTED",detectText)] <- "<"
  qualifier[grep("DETECTED NOT QUANTIFIED",detectText)] <- "<"
  qualifier[grep("BELOW QUANTIFICATION LIMIT",detectText)] <- "<"
  
  # qualifier[!(is.na(data$DetectionQuantitationLimitMeasure.MeasureValue)) & 
  #             data$ResultMeasureValue < data$DetectionQuantitationLimitMeasure.MeasureValue] <- "<"
    
  correctedData <- ifelse((nchar(qualifier)==0),data$ResultMeasureValue,
                          data$DetectionQuantitationLimitMeasure.MeasureValue)
  
  test <- data.frame(data$CharacteristicName)
  
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
  
  colnames(test)[1:4] <- c("CharacteristicName",
                           "dateTime",
                           "qualifier",
                           "value")

  test$USGSPCode <- data$USGSPCode
  test$ActivityStartDateTime <- data$ActivityStartDateTime
  test$ActivityMediaSubdivisionName <- data$ActivityMediaSubdivisionName
  test$ActivityMediaName <- data$ActivityMediaName
  test$ResultSampleFractionText <- data$ResultSampleFractionText
  test$ResultStatusIdentifier <- data$ResultStatusIdentifier
  test$ResultValueTypeName <- data$ResultValueTypeName
  
  if(length(unique(test$USGSPCode)) > 1){
    message("More than one USGSPCode returned")
  }
  
  if(length(unique(test$CharacteristicName)) > 1){
    message("More than one CharacteristicName returned")
  }
  
  if(length(unique(test$ActivityMediaName)) > 1){
    message("More than one ActivityMediaName returned")
  }
  
  if(length(unique(test$ActivityMediaSubdivisionName)) > 1){
    message("More than one ActivityMediaSubdivisionName returned")
  }
  
  if(length(unique(test$ResultSampleFractionText)) > 1){
    message("More than one ResultSampleFractionText returned")
  }
  
  test$dateTime <- format(test$dateTime, "%Y-%m-%d")
  test$dateTime <- as.Date(test$dateTime)
  
  return(test)
}

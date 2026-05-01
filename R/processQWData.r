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
processQWData <- function(data) {
  
  # Create qualifier column with "<" for any of detectText values
  detectText <- data$ResultDetectionConditionText
  detectText <- toupper(detectText)
  qualifier <- rep("", length(detectText))
  qualifier[grep("NON-DETECT", detectText)] <- "<"
  qualifier[grep("NON DETECT", detectText)] <- "<"
  qualifier[grep("NOT DETECTED", detectText)] <- "<"
  qualifier[grep("DETECTED NOT QUANTIFIED", detectText)] <- "<"
  qualifier[grep("BELOW QUANTIFICATION LIMIT", detectText)] <- "<"

  # Create processed data frame
  df <- data.frame(CharacteristicName = data$CharacteristicName,
                   dateTime = data$ActivityStartDate,
                   qualifier = qualifier,
                   value = as.numeric(ifelse(
                     (nchar(qualifier) == 0),
                     data$ResultMeasureValue,
                     data$DetectionQuantitationLimitMeasure.MeasureValue)),
                   USGSPCode = data$USGSPCode,
                   ActivityStartDateTime = data$ActivityStartDateTime,
                   ActivityTypeCode = data$ActivityTypeCode,
                   ActivityMediaName = data$ActivityMediaName,
                   ActivityMediaSubdivision = data$Activity_MediaSubdivision,
                   SampleCollectionMethod = data$SampleCollectionMethod.MethodName,
                   ResultAnalyticalMethod = data$ResultAnalyticalMethod.MethodIdentifier,
                   ResultSampleFraction = data$ResultSampleFractionText,
                   ResultStatusIdentifier = data$ResultStatusIdentifier,
                   ResultValueTypeName = data$ResultValueTypeName)
  
  # Filter out samples with no date
  n_orig <- nrow(data)
  df <- df[!is.na(df$dateTime),]
  n_date <- length(df$dateTime)
  message(paste(n_orig, "samples retrieved."))
  if (n_orig != n_date) {
    warning(paste(n_orig - n_date, "samples removed because date is missing."))
  }

  # Check for multiple unique values in specified sample characteristics
  cols <- c("USGSPCode", "CharacteristicName", "ActivityTypeCode", "ActivityMediaName",
            "ActivityMediaSubdivision", "SampleCollectionMethod", "ResultAnalyticalMethod",
            "ResultSampleFraction")
  
  multi <- Filter(function(col) length(unique(df[[col]])) > 1, cols)
  
  if (length(multi) > 0) {
    message("Multiple values for some sample characteristics:")
    for (col in multi) {
      message(col, ": ", toString(unique(df[[col]])))
    }
  }

  return(df)
}

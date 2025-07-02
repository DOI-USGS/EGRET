#' Import USGS Sample Data for EGRET analysis
#'
#' Imports data from USGS web service. 
#' For raw data, use \code{\link[dataRetrieval]{read_USGS_samples}} from the dataRetrieval package.
#' This function will retrieve the raw data, and compress it (summing constituents) if 
#' more than 1 parameter code is supplied. See
#' section 3.2.4 of the vignette for more details.
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' Default is empty quotes "" which will retrieve the full period of record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' Default is empty quotes "" which will retrieve the full period of record.
#' @param verbose logical specifying whether or not to display progress message
#' @keywords data import USGS WRTDS
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
#' @seealso \code{\link{compressData}}, \code{\link{populateSampleColumns}},
#' \code{\link[dataRetrieval]{read_waterdata_samples}}
#' @examplesIf interactive()
#' \donttest{
#' # These examples require an internet connection to run
#' 
#' Sample_01075 <- readNWISSample('01594440','01075', '1985-01-01', '1985-03-31')
#' }
readNWISSample <- function(siteNumber,
                           parameterCd,
                           startDate = "",
                           endDate = "",
                           verbose = TRUE){

  
  siteNumber <- paste0("USGS-", siteNumber)
  
  if(utils::packageVersion("dataRetrieval") >= "2.7.19"){
    data <- suppressMessages( dataRetrieval::read_waterdata_samples(monitoringLocationIdentifier = siteNumber,
                                usgsPCode = parameterCd,
                                activityStartDateLower = startDate, 
                                activityStartDateUpper = endDate))
  } else {
    data <- suppressMessages( dataRetrieval::read_USGS_samples(monitoringLocationIdentifier = siteNumber,
                                                                    usgsPCode = parameterCd,
                                                                    activityStartDateLower = startDate, 
                                                                    activityStartDateUpper = endDate))    
  }
  
  extra_cols <- c("ActivityStartDateTime",
                  "USGSPCode",
                  "ActivityMediaSubdivisionName",
                  "ActivityMediaName",
                  "ResultSampleFractionText",
                  "ResultStatusIdentifier",
                  "ResultValueTypeName",
                  "ActivityTypeCode")
  
  conversion_names <- data.frame(legacy_names = c("ResultDetectionConditionText",
                                                  "ResultMeasureValue",
                                                  "DetectionQuantitationLimitMeasure.MeasureValue",
                                                  "CharacteristicName",
                                                  "ActivityStartDate",
                                                  extra_cols),
                                 new_names = c("Result_ResultDetectionCondition",
                                               "Result_Measure",
                                               "DetectionLimit_MeasureA",
                                               "Result_Characteristic",
                                               "Activity_StartDate",
                                               "Activity_StartDateTime",
                                               "USGSpcode",
                                               "Activity_MediaSubdivisionName",
                                               "Activity_Media",
                                               "Result_SampleFraction",
                                               "Result_MeasureStatusIdentifier",
                                               "Result_MeasureType",
                                               "Activity_TypeCode"))
  
  for(i in seq_len(nrow(conversion_names))){
    names(data)[which(names(data) == conversion_names$new_names[i])] <- conversion_names$legacy_names[i]
  }
  
  if(nrow(data) == 0){
    warning("No data returned")
  }
  
  if(nrow(data) > 0){
    
    data <- processQWData(data)
    first_three <- c("dateTime",
                     "qualifier",
                     "value")
    compressedData <- compressData(data[, first_three],
                                   verbose = verbose)
    combined_data <- cbind(compressedData, data[, names(data)[!names(data) %in% first_three]])
    combined_data <- remove_zeros(combined_data, verbose = verbose)
    
    Sample <- populateSampleColumns(combined_data)
    orig_Sample <- c("Date", "ConcLow", "ConcHigh", "Uncen", "ConcAve",
                     "Julian", "Month", "Day", "DecYear", "waterYear", "MonthSeq",
                     "SinDY", "CosDY")
    Sample <- Sample[, c(orig_Sample, names(Sample)[!names(Sample) %in% orig_Sample])]
    Sample <- Sample[order(Sample$Date), ]
  } else {
    Sample <- data.frame(Date=as.Date(character()),
                         ConcLow=numeric(), 
                         ConcHigh=numeric(), 
                         Uncen=numeric(),
                         ConcAve=numeric(),
                         Julian=numeric(),
                         Month=numeric(),
                         Day=numeric(),
                         DecYear=numeric(),
                         MonthSeq=numeric(),
                         SinDY=numeric(),
                         CosDY=numeric(),
                         stringsAsFactors=FALSE)
  }
  
  return(Sample)
  
}


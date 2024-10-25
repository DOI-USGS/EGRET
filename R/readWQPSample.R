#' Import Sample Data from the Water Quality Portal for WRTDS
#'
#' Imports data from the Water Quality Portal, so it could be STORET, USGS, or USDA data. 
#' This function gets the data from: \url{https://www.waterqualitydata.us}
#' For raw data, use readWQPdata.  This function will retrieve the raw data, 
#' compress it (summing constituents), then converts it to the Sample dataframe structure.
#' See chapter 7 of the EGRET user guide for more details.
#' @param siteNumber character site number.  If USGS, it should be in the form :'USGS-XXXXXXXXX...'
#' @param characteristicName character. Either a valid characteristic name, or a 5 digit 
#' USGS parameter code.
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
#' @seealso \code{\link[dataRetrieval]{readWQPdata}}, \code{dataRetrieval::whatWQPsites}, 
#' \code{\link[dataRetrieval]{readWQPqw}}, \code{\link{compressData}}, \code{\link{populateSampleColumns}}
#' @examples
#' # These examples require an internet connection to run
#' \donttest{
#' # Sample_All <- readWQPSample('WIDNR_WQX-10032762','Specific conductance', '', '')
#' }
readWQPSample <- function(siteNumber,
                          characteristicName,
                          startDate = "",
                          endDate = "",
                          verbose = TRUE){
  
  extra_cols <- c("ActivityStartDateTime",
                  "USGSPCode",
                  "ActivityMediaSubdivisionName",
                  "ActivityMediaName",
                  "ResultSampleFractionText",
                  "ResultStatusIdentifier",
                  "ResultValueTypeName")
  
  if(packageVersion("dataRetrieval") >= "2.7.17"){
    data <- suppressMessages(dataRetrieval::readWQPqw(siteNumbers = siteNumber,
                                     parameterCd = characteristicName,
                                     startDate = startDate,
                                     endDate = endDate,
                                     ignore_attributes = TRUE,
                                     legacy = FALSE)  )
    
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
                                                 "Result_MeasureType"))
    
    for(i in seq_len(nrow(conversion_names))){
      names(data)[which(names(data) == conversion_names$new_names[i])] <- conversion_names$legacy_names[i]
    }

  } else {
    data <- dataRetrieval::readWQPqw(siteNumbers = siteNumber,
                                     parameterCd = characteristicName,
                                     startDate = startDate,
                                     endDate = endDate,
                                     ignore_attributes = TRUE)
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

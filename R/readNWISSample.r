#' Import NWIS Sample Data for EGRET analysis
#'
#' Imports data from NWIS web service. 
#' A list of parameter and statistic codes can be found here: \url{https://help.waterdata.usgs.gov/codes-and-parameters}
#' For raw data, use \code{\link[dataRetrieval]{readNWISqw}} from the dataRetrieval package.
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
#' \code{\link[dataRetrieval]{readNWISqw}}
#' @examples
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
  

  multi_pcodes <- length(parameterCd) > 1
  if(multi_pcodes){
    rawSample <- dataRetrieval::readNWISqw(siteNumber,
                                           parameterCd,
                                           startDate,endDate, 
                                           expanded = FALSE)
    dataColumns <- grep("p\\d{5}",names(rawSample))
    remarkColumns <- grep("r\\d{5}",names(rawSample))
    totalColumns <-c(grep("sample_dt",names(rawSample)),
                     dataColumns, remarkColumns)
    totalColumns <- totalColumns[order(totalColumns)]
    extras <- rawSample[, c("medium_cd")]

  } else {
    rawSample <- dataRetrieval::readNWISqw(siteNumber,
                                           parameterCd,
                                           startDate,endDate, 
                                           expanded = TRUE)
    totalColumns <- c("sample_dt",  "remark_cd",  "result_va")
    
    extras <- rawSample[, c("medium_cd", "hyd_cond_cd", "samp_type_cd", 
                            "hyd_event_cd", "dqi_cd", "rpt_lev_cd")]
    
  }
  

  if(nrow(rawSample) > 0){
    compressedData <- compressData(rawSample[,totalColumns],
                                   verbose = verbose)
    Sample <- populateSampleColumns(compressedData)
    if(!multi_pcodes){
      Sample <- cbind(Sample, extras)
    }
    
  } else {
    Sample <- data.frame(Date = as.Date(character()),
                         ConcLow = numeric(), 
                         ConcHigh = numeric(), 
                         Uncen = numeric(),
                         ConcAve = numeric(),
                         Julian = numeric(),
                         Month = numeric(),
                         Day = numeric(),
                         DecYear = numeric(),
                         MonthSeq = numeric(),
                         SinDY = numeric(),
                         CosDY = numeric(),
                         stringsAsFactors = FALSE)
  }

  return(Sample)
}



#' Import NWIS Daily Data for EGRET analysis
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @param convert logical Option to include a conversion from cfs to cms (35.314667). The default is TRUE, 
#' which is appropriate for using NWIS data in the EGRET package.  Set this to FALSE to not include the conversion. If the parameter code is not 00060 (NWIS discharge),
#' there is no conversion applied.
#' @keywords data import USGS WRTDS
#' @export
#' @import dataRetrieval
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
#' @seealso \code{\link[dataRetrieval]{readNWISdv}}, \code{\link{populateDaily}}
#' @examples
#' # These examples require an internet connection to run
#' 
#' Daily <- readNWISDaily('01594440','00060', '1985-01-01', '1985-03-31')
#' DailySuspSediment <- readNWISDaily('01594440','80154', '1985-01-01', '1985-03-31',convert=FALSE)
#' 
readNWISDaily <- function (siteNumber,parameterCd="00060",
                           startDate="",endDate="",interactive=TRUE,convert=TRUE){

#   data <- readNWISdv(siteNumber,parameterCd,startDate,endDate)
#   #  need to setup conversion factor because the NWIS data are in cfs but we store in cms
#   names(data) <- c('agency', 'site', 'dateTime', 'tz_cd','code', 'value')  # do a merge instead?
#   
#   data$dateTime <- as.Date(data$dateTime) 
  ##################################
  url <- dataRetrieval::constructNWISURL(siteNumber,parameterCd,startDate,endDate,"dv",statCd="00003", format = "tsv")
  
  data <- dataRetrieval::importRDB1(url, asDateTime=FALSE)
  if(nrow(data)>0){
    names(data) <- c('agency', 'site', 'dateTime', 'value', 'code')
    data$dateTime <- as.Date(data$dateTime)
  }

  #####################################
  qConvert <- ifelse("00060" == parameterCd, 35.314667, 1)
  qConvert<- ifelse(convert,qConvert,1)
  
  localDaily <- populateDaily(data,qConvert,interactive=interactive)
  return (localDaily)
}

#' Import NWIS Daily Data for EGRET analysis
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{https://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{https://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' @param verbose logical specifying whether or not to display progress message
#' @param interactive logical deprecated. Use 'verbose' instead
#' @param convert logical Option to include a conversion from cfs to cms (35.314667). The default is TRUE, 
#' which is appropriate for using NWIS data in the EGRET package.  Set this to FALSE to not include the conversion. If the parameter code is not 00060 (NWIS discharge),
#' there is no conversion applied.
#' @keywords data import USGS WRTDS
#' @export
#' @importFrom dataRetrieval constructNWISURL
#' @importFrom dataRetrieval importRDB1
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
#' \dontrun{
#' 
#' Daily <- readNWISDaily('01594440','00060', '1985-01-01', '1985-03-31')
#' DailySuspSediment <- readNWISDaily('01594440','80154', '1985-01-01', '1985-03-31',convert=FALSE)
#' }
readNWISDaily <- function (siteNumber,parameterCd="00060",
                           startDate="",endDate="",verbose = TRUE, interactive=NULL,convert=TRUE){


  if(!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  
  url <- constructNWISURL(siteNumber,parameterCd,startDate,endDate,"dv",statCd="00003", format = "tsv")
  
  data <- importRDB1(url, asDateTime=FALSE)
  if(nrow(data)>0){
    names(data) <- c('agency', 'site', 'dateTime', 'value', 'code')
    data$value <- as.numeric(data$value)
    #####################################
    qConvert <- ifelse("00060" == parameterCd, 35.314667, 1)
    qConvert<- ifelse(convert,qConvert,1)
    
    localDaily <- populateDaily(data,qConvert,verbose = verbose)
  } else {
    localDaily <- data.frame(Date=as.Date(character()),
                         Q=numeric(), 
                         Julian=numeric(),
                         Month=numeric(),
                         Day=numeric(),
                         DecYear=numeric(),
                         MonthSeq=numeric(),
                         Qualifier=character(),
                         i=integer(),
                         LogQ=numeric(),
                         Q7=numeric(),
                         Q30=numeric(),
                         stringsAsFactors=FALSE)
  }


  return (localDaily)
}

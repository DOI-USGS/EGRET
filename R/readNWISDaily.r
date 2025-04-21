#' Import NWIS Daily Data for EGRET analysis
#'
#' Imports daily data from NWIS web service. This function gets the data from here: \url{https://waterservices.usgs.gov/}
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' @param verbose logical specifying whether or not to display progress message
#' @param convert logical Option to include a conversion from cfs to cms (35.314667). The default is TRUE, 
#' which is appropriate for using NWIS data in the EGRET package.  Set this to FALSE to not include the conversion. If the parameter code is not 00060 (NWIS discharge),
#' there is no conversion applied.
#' @keywords data import USGS WRTDS
#' @export
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
#' @examplesIf interactive()
#' \donttest{
#' 
#' Daily <- readNWISDaily('01594440','00060',
#'                        '1985-01-01', '1985-03-31')
#' DailySuspSediment <- readNWISDaily('01594440','80154',
#'                                    '1985-01-01', '1985-03-31',
#'                                    convert = FALSE)
#' }
readNWISDaily <- function (siteNumber,
                           parameterCd="00060",
                           startDate = "",
                           endDate = "",
                           verbose = TRUE,
                           convert = TRUE){

  url <- dataRetrieval::constructNWISURL(siteNumbers = siteNumber,
                                         parameterCd = parameterCd,
                                         startDate = startDate,
                                         endDate = endDate,
                                         service = "dv",
                                         statCd = "00003",
                                         format = "tsv")
  
  data_rdb <- dataRetrieval::importRDB1(url, asDateTime = FALSE)
  
  if(nrow(data_rdb) > 0){
    if(length(names(data_rdb)) >= 5){
      names(data_rdb) <- c('agency', 'site', 'dateTime', 'value', 'code')
      data_rdb$dateTime <- as.Date(data_rdb$dateTime)
      data_rdb$value <- as.numeric(data_rdb$value)
      #####################################
      qConvert <- ifelse("00060" == parameterCd, 35.314667, 1)
      qConvert<- ifelse(convert,qConvert,1)
      
      localDaily <- populateDaily(data_rdb,
                                  qConvert,
                                  verbose = verbose)      
    } else {
      if("comment" %in% names(attributes(data_rdb))){
        message(attr(data_rdb, "comment"))
      }
    }

  } else {
    localDaily <- data.frame(Date = as.Date(character()),
                             Q = numeric(), 
                             Julian = numeric(),
                             Month = numeric(),
                             Day = numeric(),
                             DecYear = numeric(),
                             MonthSeq = numeric(),
                             Qualifier = character(),
                             i = integer(),
                             LogQ = numeric(),
                             Q7 = numeric(),
                             Q30 = numeric(),
                             stringsAsFactors = FALSE)
  }

  return (localDaily)
}

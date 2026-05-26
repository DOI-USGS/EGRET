#' Import NWIS Daily Data for EGRET analysis
#'
#' Imports daily data from NWIS web service. This function gets the data from here: \url{https://waterservices.usgs.gov/}
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number.
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' @param verbose logical specifying whether or not to display messages.
#' @param convert logical Option to include a conversion from cfs to cms (35.314667).
#' The default is TRUE which is appropriate for using NWIS data in the EGRET package.
#' Set this to FALSE to not include the conversion. If the parameter code is not 00060
#' (NWIS discharge), there is no conversion applied.
#' @param adjust logical specifying whether or not to add a constant to zero values to
#' allow log transformation. Defaults to TRUE.
#' @param fill logical specifying whether to fill NA values by linear interpolation.
#' Defaults to FALSE.
#' @param maxgap Maximum number of NA days allowed for interpolating gaps.
#' Default is 21. Only used if fill is set to TRUE.
#' @param fill_type character to define what process to fill missing data. Options are
#' "interpolation" - linear interpolation from the
#' `zoo::na.approx`, or "log_interp" - linear interpolation in the log space.
#' Only used if fill is set to TRUE.
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
#' @seealso \code{\link[dataRetrieval]{read_waterdata_daily}}, \code{\link{populateDaily}}
#' @examplesIf interactive()
#' \donttest{
#'
#' Daily <- readNWISDaily('01594440','00060',
#'                        '2004-01-01', '2024-03-31')
#' DailySuspSediment <- readNWISDaily('01594440','80154',
#'                                    '1985-01-01', '1985-03-31',
#'                                    convert = FALSE)
#' }
readNWISDaily <- function(
  siteNumber,
  parameterCd = "00060",
  startDate = "",
  endDate = "",
  verbose = TRUE,
  convert = TRUE,
  adjust = TRUE,
  fill = FALSE,
  maxgap = 21,
  fill_type = "interpolation"
) {
  qConvert <- ifelse("00060" == parameterCd, 35.314667, 1)
  qConvert <- ifelse(convert, qConvert, 1)

  if (!grepl("USGS-", siteNumber)) {
    siteNumber <- paste0("USGS-", siteNumber)
  }

  daily_data <- suppressMessages(dataRetrieval::read_waterdata_daily(
    monitoring_location_id = siteNumber,
    parameter_code = parameterCd,
    time = c(startDate, endDate),
    statistic_id = "00003",
    skipGeometry = TRUE
  ))

  if (nrow(daily_data) > 0) {
    daily_data <- daily_data[, c("time", "value", "qualifier")]

    localDaily <- populateDaily(
      daily_data,
      qConvert,
      verbose = verbose,
      adjust = adjust,
      fill = fill,
      maxgap = maxgap,
      fill_type = fill_type
    )
  } else {
    localDaily <- data.frame(
      Date = as.Date(character()),
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
      stringsAsFactors = FALSE
    )
  }

  return(localDaily)
}

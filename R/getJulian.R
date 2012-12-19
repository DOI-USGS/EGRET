#' A utility program to determine the Julian day for any given date
#'
#' Julian days are computed with an origin of "1850-01-01". 
#' Data input is, in quotes, something like "1949-09-30"
#'
#' @param date character string specifying a date.  In quotes as "yyyy-mm-dd"
#' @keywords water quality, streamflow, statistics
#' @export
#' @return Julian a numeric value, representing the julian day since 1850-01-01
#' @examples
#' getJulian("1949-09-30")
getJulian<-function(date) {
  # enter date in quotes for example "1949-09-30" 
  # program returns the julian date  
  dateTime <- as.Date(date)
  Julian <- as.numeric(julian(dateTime, origin = as.Date("1850-01-01")))
  return(Julian)
}
#' Populate Date Columns
#'
#' Creates various date columns for WRTDS study.
#'
#' @param rawData vector with dateTime
#' @return DateFrame dataframe
#' @export
#' @examples
#' dateTime <- c('1984-02-28 13:56',
#'               '1984-03-01 00:00',
#'               '1986-03-01 00:00',
#'               '1986-10-15 00:00')
#'               
#' expandedDateDF <- populateDateColumns(dateTime)
#' expandedDateDF
#' 
#' dateTime <- c('1984-02-28', 
#'               '1984-03-01',
#'               '1986-03-01',
#'               '1986-10-15')
#' expandedDateDF <- populateDateColumns(dateTime)
#' expandedDateDF
populateDateColumns <- function(rawData){  # rawData is a vector of dates
  DateFrame <- as.data.frame(matrix(ncol=1,nrow=length(rawData)))
  colnames(DateFrame) <- c('Date')  
  DateFrame$Date <- rawData
  dateTime <- as.POSIXlt(rawData)
  DateFrame$Julian <- as.numeric(julian(dateTime,origin=as.Date("1850-01-01")))
  DateFrame$Month <- dateTime$mon + 1
  DateFrame$Day <- dateTime$yday + 1
  year <- dateTime$year + 1900
  hour <- dateTime$hour
  minute <- dateTime$min
  
  if (sum(hour) == 0 & sum(minute) == 0){
    dateTime$hour <- rep(12,length(dateTime))
  }
  
  leapOffset <- ifelse((year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)), 0,1)
  
  DateFrame$Day[DateFrame$Day > 59] <- DateFrame$Day[DateFrame$Day > 59] + leapOffset[DateFrame$Day > 59]

  DateFrame$DecYear <- decimalDate(dateTime)
  DateFrame$MonthSeq <- ((year-1850)*12)+DateFrame$Month
  DateFrame$waterYear <- as.integer(DateFrame$DecYear)
  DateFrame$waterYear[DateFrame$Month %in% c(10:12)] <- DateFrame$waterYear[DateFrame$Month %in% c(10:12)]+1
  
  return (DateFrame)
  
}

#' decimalDate
#' 
#' Create a decimal date or date/time from a vector.
#' 
#' @export
#' @param rawData vector of dates or dateTimes.
#' @examples 
#' dateTime <- c('1984-02-28 13:56',
#'               '1984-03-01 00:00',
#'               '1986-03-01 00:00',
#'               '1986-10-15 00:00')
#' decimalDate(dateTime)
#' 
#' dateTime <- c('1984-02-28', 
#'               '1984-03-01',
#'               '1986-03-01',
#'               '1986-10-15')
#' decimalDate(dateTime)
decimalDate <- function(rawData){
  
  dateTime <- as.POSIXlt(rawData)
  year <- dateTime$year + 1900
  
  startYear <- as.POSIXct(paste0(year,"-01-01 00:00"))
  endYear <- as.POSIXct(paste0(year+1,"-01-01 00:00"))
  
  DecYear <- year + as.numeric(difftime(dateTime, startYear, units = "secs"))/as.numeric(difftime(endYear, startYear, units = "secs"))
  return(DecYear)
}

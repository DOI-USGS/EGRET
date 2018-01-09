#' startEnd
#' 
#' Returns two date variables for a combination of paStart, paLong, and year
#' 
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param year integer year
#' 
#' @return 
#' @export
#' @examples 
#' paStart <- 10
#' paLong <- 12
#' year <- 1999
#' startEnd(paStart, paLong, year)
startEnd <- function(paStart, paLong, year){
# this function returns two date variables for a combination of paStart, paLong, and year
# where year is the calendar year in which the period of analysis ends
	monthChar <- c("01","02","03","04","05","06","07","08","09","10","11","12")
	endDayChar <- c("31","28","31","30","31","30","31","30","30","31","30","31")
	endMonth <- paStart + paLong - 1
	yearChar <- if(endMonth > 12) as.character(year - 1) else as.character(year)
	startDate <- paste(yearChar,"-",monthChar[paStart],"-01",sep="")
	endMonth <- if(endMonth > 12) endMonth - 12 else endMonth
  endDate <- paste(as.character(year),"-",monthChar[endMonth],"-",endDayChar[endMonth],sep="")
  isLeap <- (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
  endDate <- if (isLeap & endMonth==2) paste(as.character(year),"-02-29",sep="") else endDate 
  startEnd <- as.Date(c(startDate,endDate))
  return(startEnd)	
}


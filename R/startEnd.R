#' startEnd
#' 
#' Returns two date variables for a combination of paStart, paLong, and year
#' 
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param year integer year
#' 
#' @return Date list 
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
  startEnd <- list(startDate=as.Date(startDate),
                   endDate=as.Date(endDate))
  return(startEnd)	
}

#' surfaceStartEnd
#' 
#' surfaceStartEnd
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param Date1 earliest date of Sample (to set surface start)
#' @param Date2 latest date of Sample (to set surface end)
#' @export
#' @examples 
#' eList <- Choptank_eList
#' Date1 <- eList$Sample$Date[1]
#' Date2 <- range(eList$Sample$Date)[2]
#' surfaceStartEnd(10, 12, Date1, Date2)
surfaceStartEnd <- function(paStart, paLong, Date1, Date2){
  
  Date1 <- as.Date(Date1)
  Date2 <- as.Date(Date2)
  year1Temp <- trunc(decimalDate(Date1)) - 2
  year2Temp <- trunc(decimalDate(Date2)) + 2
  # first we march forward to find the starting date
  for(i in year1Temp:year2Temp){
    startEndPair <- startEnd(paStart, paLong, i)
    if(Date1 <= startEndPair[[1]]) break
  }
  year <- i - 1 
  startEndPair <- startEnd(paStart, paLong, year)
  startSurface <- startEndPair[[1]]
  # now we march backward to find the ending date
  for(i in year2Temp:year1Temp){
    startEndPair <- startEnd(paStart, paLong, i)
    if(Date2 >= startEndPair[[1]]) break
  }
  year <- i   

  endSurface <- startEndPair[[2]]

  return(list(surfaceStart = as.Date(startSurface), 
              surfaceEnd = as.Date(endSurface)))

}


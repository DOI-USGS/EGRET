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
#' @param Date1
#' @param Date2
#' @export
#' @examples 
#' eList <- Choptank_eList
#' Date1 <- eList$Sample$Date[1]
#' Date2 <- range(eList$Sample$Date)[2]
#' surfaceStartEnd(10, 12, Date1, Date2)
surfaceStartEnd <- function(paStart, paLong, Date1, Date2){
  
  fractionOfYear1 <- decimalDate(Date1) - trunc(decimalDate(Date1))
  fractionOfWaterYear <- fractionWaterYear(Date1)
  
  if(fractionOfYear1 > fractionOfWaterYear){
    surfaceStart <- paste0(trunc(decimalDate(Date1)),"-10-01")
  } else {
    surfaceStart <- paste0(trunc(decimalDate(Date1))-1,"-10-01")
  }
  
  fractionOfYear2 <- decimalDate(Date2) - trunc(decimalDate(Date2))
  fractionOfWaterYear <- fractionWaterYear(Date2)
  
  if(fractionOfYear2 > fractionOfWaterYear){
    surfaceEnd <- paste0(trunc(decimalDate(Date2))+1,"-09-30")
  } else {
    surfaceEnd <- paste0(trunc(decimalDate(Date2)),"-09-30")
  }
  
  return(list(surfaceStart = surfaceStart, surfaceEnd = surfaceEnd))
}


fractionWaterYear <- function(Date){
  year <- trunc(decimalDate(Date))
  
  isLeap <- (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
  if(isLeap){
    fractionOfWaterYear <- decimalDate("1976-09-30") - trunc(decimalDate("1976-09-30"))
  } else {
    fractionOfWaterYear <- decimalDate("1977-09-30") - trunc(decimalDate("1977-09-30"))
  }
  return(fractionOfWaterYear)
}

#' Create a character variable that describes the period of analysis, when period of analysis has already been set in AnnualResults
#'
#' The period of analysis can be of any length from 1 month to 12 months. 
#' The period of analysis can have any starting month from 1 (January) through 12 (December). 
#' This function produces a character character that describes this period of analysis. 
#' For example "water year", "calendar year", "year starting with April", or 
#' "Season consisting of April, May, June". 
#' There is an alternative version of this function for the case where AnnualResults does not exist. 
#' This might arise in a call from plotConcTime or plotLogConcTime. 
#' That function is called setSeasonLabelByUser. 
#'
#' @param localAnnualResults data frame that contains the annual results, default is AnnualResults
#' @param monthLab object of monthLabel class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @keywords water quality graphics
#' @export
#' @return periodName character which describes the period of analysis
#' @examples
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' AnnualResults <- setupYears(Daily)
#' setSeasonLabel(AnnualResults)
#' 
#' AnnualResultsWinter <- setupYears(Daily, 
#'                                   paLong = 3,
#'                                   paStart = 12)
#' setSeasonLabel(AnnualResultsWinter)
setSeasonLabel <- function(localAnnualResults,
                           monthLab = 1){
  # this function sets up text variable used to label graphs and
  # tables, defining what the period of analysis is
  paStart <- localAnnualResults$PeriodStart[1]
  paLong <- localAnnualResults$PeriodLong[1]
  
  if (is.numeric(monthLab)){
    monthInfo <- monthInfo[shortCode=monthLab][[1]]    
  } else if (is.character(monthLab)){
    monthInfo <- monthInfo[monthLab][[1]]
  } else {
    monthInfo <- monthLab
  }
  
  index <- seq(paStart, paStart + paLong - 1)
  index <- ifelse(index > 12, index - 12, index)

  monthList <- monthInfo@monthAbbrev[index]
  
  monthList <- paste(monthList, collapse = " ")
  
  temp1 <- c("Year Starting With", monthInfo@monthFull[paStart])
  temp1 <- paste(temp1, collapse = " ")
  temp2 <- "Water Year"
  temp3 <- "Calendar Year"
  temp4 <- c("Season Consisting of", monthList)
  temp4 <- paste(temp4, collapse = " ")
  periodName <- temp4
  periodName<-if(paLong==12) temp1 else periodName
  periodName<-if(paLong==12 & paStart==10) temp2 else periodName
  periodName<-if(paLong==12 & paStart==1) temp3 else periodName
  return(periodName)  
}
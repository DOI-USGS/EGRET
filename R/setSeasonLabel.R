#' Create a character string that describes the period of analysis, when period of analysis has already been set in AnnualResults
#'
#' The period of analysis can be of any length from 1 month to 12 months. 
#' The period of analysis can have any starting month from 1 (January) through 12 (December). 
#' This function produces a character string that describes this period of analysis. 
#' For example "water year", "calendar year", "year starting with April", or 
#' "Season consisting of April, May, June". 
#' There is an alternative version of this function for the case where AnnualResults does not exist. 
#' This might arise in a call from plotConcTime or plotLogConcTime. 
#' That function is called setSeasonLabelByUser. 
#'
#' @param localAnnualResults data frame that contains the annual results, default is AnnualResults
#' @keywords water quality graphics
#' @export
#' @return periodName string which describes the period of analysis
#' @examples
#' Daily <- ChopDaily
#' AnnualResults <- setupYears()
#' setSeasonLabel(AnnualResults)
setSeasonLabel<-function(localAnnualResults){
  # this function sets up text variable used to label graphs and
  # tables, defining what the period of analysis is
  paStart<-localAnnualResults$PeriodStart[1]
  paLong<-localAnnualResults$PeriodLong[1]
  index<-seq(paStart,paStart+paLong-1)
  index<-ifelse(index>12,index-12,index)
  #   monthList<-c(monthAbbrev[index[1:paLong]])
  monthList <- sapply(index[1:paLong], function(x){monthInfo[[x]]@monthAbbrev})
  monthList<-paste(monthList,collapse=" ")
  #   temp1<- c("Year Starting With",monthFull[paStart])
  temp1<- c("Year Starting With",monthInfo[[paStart]]@monthFull)
  temp1<-paste(temp1,collapse=" ")
  temp2<- "Water Year"
  temp3<- "Calendar Year"
  temp4<- c("Season Consisting of",monthList)
  temp4<-paste(temp4,collapse=" ")
  periodName<-temp4
  periodName<-if(paLong==12) temp1 else periodName
  periodName<-if(paLong==12&paStart==10) temp2 else periodName
  periodName<-if(paLong==12&paStart==1) temp3 else periodName
  return(periodName)  
}
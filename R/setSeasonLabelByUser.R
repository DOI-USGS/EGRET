#' Creates a character string that describes the period of analysis, when the period of analysis is being set by the user and not from AnnualResults
#'
#' The period of analysis can be of any length from 1 month to 12 months. 
#' The period of analysis can have any starting month from 1 (January) through 12 (December). 
#' This function produces a character string that describes this period of analysis. 
#' For example "water year", "calendar year", "year starting with April", or 
#' "Season consisting of April, May, June". 
#' There is an alternative version of this function for the case where AnnualResults exists. 
#' And we want to use the period of analysis defined there. 
#' That function is called setSeasonLabel. 
#'
#' @param paStartInput numeric the month which is the start of the period of analysis, default is 10 which would be the case if the period of analysis is the water year
#' @param paLongInput numeric the length of the the period of analysis, in months, default is 12 which would be the case if the period of analysis is the water year
#' @keywords water quality graphics
#' @export
#' @return periodName string which describes the period of analysis
#' @examples
#' setSeasonLabelByUser(paStartInput=1,paLongInput=12)
#' setSeasonLabelByUser(paStartInput=4,paLongInput=3)
setSeasonLabelByUser<-function(paStartInput = 10, paLongInput = 12){
  # this function sets up text variable used to label graphs and
  # tables, defining what the period of analysis is
  paStart<-paStartInput
  paLong<-paLongInput
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
#' Creates a subset Daily data frame that only contains daily estimates for the specified period of analysis
#'
#' This function uses the user-defined 'period of analysis', and subsets the Daily data frame, it doesn't have 
#' any effect on the Sample data frame.
#' If you want to examine your data set as a time series of water years, 
#' then the period of analysis is October through September.  
#' If you want to examine the data set as calendar years then the period of analysis is January through December.  
#' You might want to examine the winter season, which you could define as December through February, 
#' then those 3 months become the period of analysis. The only constraints on the definition of a period of analysis 
#' are these: it must be defined in terms of whole months; it must be a set of contiguous months (like March-April-May), 
#' and have a length that is no less than 1 month and no more than 12 months.  
#' Define the PA by using two arguments: paLong and paStart. paLong is the length of the period of analysis,
#' and paStart is the starting month.
#'
#' @param paLong a numeric value for the length of the period of analysis, must be an integer from 1 to 12
#' @param paStart a numeric value for the starting month of the period of analysis, must be an integer from 1 to 12
#' @param df dataframe which must contain a column named Month (for month of the calendar year, typically this 
#' is a Daily data frame.
#' @keywords statistics streamflow
#' @export
#' @return localDaily a data frame containing the daily data but only for the period of analysis (not all months)
#' @examples
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' DailySubset <- selectDays(Daily, 4, 11)
selectDays<-function(df, paLong, paStart) {
  
  localDaily <- df

  goodMonth <- seq(paStart,length = paLong)
  goodMonth[goodMonth > 12] <- goodMonth[goodMonth > 12] - 12
  localDaily <- localDaily[localDaily$Month %in% goodMonth, ]

  return(localDaily)
}
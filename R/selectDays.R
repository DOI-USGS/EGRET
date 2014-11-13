#' Creates a subset of the Daily data frame that only contains data for the specified period of analysis
#'
#'  Function is not called by user, but is called by plotSDLogQ
#'
#' @param paLong a numeric value for the length of the period of Analysis, must be an integer from 1 to 12
#' @param paStart a numeric value for the starting month of the period of analysis, must be an integer from 1 to 12
#' @param df dataframe with Q
#' @keywords statistics streamflow
#' @export
#' @return localDaily a data frame containing the daily data but only for the period of analysis (not all months)
#' @examples
#' Daily <- ChopDaily
#' DailySubset <- selectDays(Daily, 4, 11)
selectDays<-function(df, paLong, paStart) {
  
  localDaily <- df

  goodMonth <- seq(paStart,length = paLong)
  goodMonth[goodMonth > 12] <- goodMonth[goodMonth > 12] - 12
  localDaily <- localDaily[localDaily$Month %in% goodMonth, ]

  return(localDaily)
}
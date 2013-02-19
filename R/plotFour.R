#' Makes four graphs of streamflow statistics on a single page
#'
#'  Part of the flowHistory system.  The four statistics are 1-day maximum, annual mean, annual 7-day minimum, and the running standard deviation of the log daily discharge values.
#'  Prior to running this code user must have run setPA and makeAnnualSeries.
#'
#' @param localINFO string specifying the name of the data frame that contains the metadata, defoult name is INFO
#' @param localAnnualSeries string specifying the name of the data frame that contains the annual series of statistics, default is annualSeries
#' @param localDaily string specifying the name of the data frame that contains the daily values, default is Daily
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' INFO <- exINFOEnd
#' annualSeries <- exannualSeries
#' Daily <- exDailyStart
#' plotFour(window=2)
plotFour<-function (localINFO = INFO, localAnnualSeries = annualSeries, localDaily = Daily, 
                    yearStart = NA, yearEnd = NA, printTitle = TRUE, runoff = FALSE, 
                    qUnit = 1, window=15) 
{
  par(mfcol = c(2, 2), cex = 0.6, oma = c(0, 1.7, 6, 1.7), 
      cex.lab = 1.4, cex.axis = 1.2)
  setYearStart <- if (is.na(yearStart)) {
    min(localAnnualSeries[1, , ], na.rm = TRUE)
  } else {
    yearStart
  } 
  setYearEnd <- if (is.na(yearEnd)) {
    max(localAnnualSeries[1, , ], na.rm = TRUE)
  } else {
    yearEnd
  }
  plotFlowSingle(istat = 8, yearStart = setYearStart, yearEnd = setYearEnd, 
                 localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE)
  plotFlowSingle(istat = 2, yearStart = setYearStart, yearEnd = setYearEnd, 
                 localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE)
  plotFlowSingle(istat = 5, yearStart = setYearStart, yearEnd = setYearEnd, 
                 localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE)
  plotSDLogQ(yearStart = setYearStart, yearEnd = setYearEnd, window = window, 
             localDaily = localDaily, localINFO = localINFO, 
             tinyPlot = TRUE, printPA = FALSE,  
             printStaName = FALSE)
  
  textPA <- setSeasonLabelByUser(paStartInput = localINFO$paStart, 
                                 paLongInput = localINFO$paLong)
  title <- if (printTitle) 
    paste(localINFO$shortName, "\n", textPA)
  mtext(title, cex = 1.2, outer = TRUE, font = 2)
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}
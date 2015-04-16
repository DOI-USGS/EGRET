#' Makes four graphs of streamflow statistics on a single page
#'
#' @description
#'  Part of the flowHistory system.  The four statistics are 1-day maximum, annual mean, annual 7-day minimum, and the running standard deviation of the log daily discharge values.
#'  
#'  Although there are a lot of optional arguments to this function, most are set to a logical default.
#'  
#' Data come from named list, which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least Daily and INFO dataframes
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{plotFlowSingle}}
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' #Water year:
#' plotFour(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList,paStart=6,paLong=3)
#' plotFour(eList)
#' } 
plotFour<-function (eList, 
                    yearStart = NA, yearEnd = NA, printTitle = TRUE, runoff = FALSE, 
                    qUnit = 1, window=15, cex = 0.8, cex.axis = 1.2,cex.main=1.2,
                    col="black", lwd=1,...) {
    
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localAnnualSeries <- makeAnnualSeries(eList)
  
  par(mfcol = c(2, 2), oma = c(0, 1.7, 6, 1.7))
  
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
  plotFlowSingle(eList, istat = 8, yearStart = setYearStart, yearEnd = setYearEnd, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE,cex=cex, cex.main=1,
                 cex.axis = cex.axis, col=col,lwd=lwd,...)
  plotFlowSingle(eList, istat = 2, yearStart = setYearStart, yearEnd = setYearEnd, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE,cex=cex, cex.main=1,
                 cex.axis = cex.axis, col=col,lwd=lwd, ...)
  plotFlowSingle(eList, istat = 5, yearStart = setYearStart, yearEnd = setYearEnd, 
                 tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
                 printIstat = TRUE, printStaName = FALSE,cex=cex, cex.main=1,
                 cex.axis = cex.axis, col=col,lwd=lwd, ...)
  plotSDLogQ(eList, yearStart = setYearStart, yearEnd = setYearEnd, window = window, 
             tinyPlot = TRUE, printPA = FALSE,  
             printStaName = FALSE, cex=cex, cex.main=1,
             cex.axis = cex.axis, col=col,lwd=lwd, ...)
  
  textPA <- setSeasonLabelByUser(paStartInput = localINFO$paStart, 
                                 paLongInput = localINFO$paLong)
  title <- if (printTitle) 
    paste(localINFO$shortName, "\n", textPA)
  mtext(title, outer = TRUE, font = 2,cex=cex.main)
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}
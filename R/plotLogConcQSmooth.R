#' Plot up to three curves representing the log concentration versus discharge relationship, each curve is a different point in time
#'
#' These plots are like a vertical slice of the estimated concentration surface that is seen in the plotContours function.  
#' These plots show how the concentration-discharge relationship is changing over time. 
#' Typically the time points selected would be in three years at the same time of year spaced out over the period of record.  But that is not necessary.  
#' Another possibility is to use this to explore seasonal differences.  In this case the three
#' dates would be in the same year but different times during the year.
#'
#' @param date1 string specifying the date for the first curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes) 
#' @param date2 string specifying the date for the second curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If only one curve is wanted this should be NA
#' @param date3 string specifying the date for the third curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If a third curve is not wanted this should be NA
#' @param qLow numeric value for the lowest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qHigh numeric value for the highest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend, in the units shown on x-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param legendTop numeric which represents the top edge of the legend, in the units shown on y-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param concMin numeric value for lower limit on concentration shown on the graph, default = NA (which causes the lower limit to be set automatically, based on the data)
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are also printed to the console (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param localSample string specifying the name of the data frame that contains the Sample data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param cex number
#' @param cex.axis number
#' @param cex.main number
#' @param lwd number
#' @param legend.cex number
#' @param tinyPlot logical
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords water-quality statistics graphics
#' @import survival
#' @export
#' @examples 
#' date1<-"2001-06-01"
#' date2<-"2009-06-01"
#' date3<-NA
#' qLow<-1
#' qHigh<-1000
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogConcQSmooth(date1,date2,date3,qLow,qHigh)
plotLogConcQSmooth<-function(date1,date2,date3,qLow,qHigh,qUnit = 2, legendLeft = .05,legendTop =0.3, 
                             concMax = NA, concMin = NA,bw = FALSE, printTitle = TRUE, printValues = FALSE, 
                             localSample = Sample, localINFO = INFO, 
                             windowY = 10, windowQ = 2, windowS = 0.5,tinyPlot=FALSE,
                             lwd=2,cex=0.8, cex.axis=1.1,cex.main=1.1, legend.cex=1,...) {
  
  plotConcQSmooth(date1=date1,date2=date2,date3=date3,qLow=qLow,qHigh=qHigh,
                            qUnit = qUnit, legendLeft = legendLeft,legendTop =legendTop, 
                            concMax = concMax, concMin=concMin, bw = bw, printTitle = printTitle, printValues = printValues, 
                            localSample = localSample, localINFO = localINFO, 
                            windowY = windowY, windowQ = windowQ, windowS = windowS,tinyPlot=tinyPlot,
                            lwd=lwd,cex=cex, cex.axis=cex.axis,cex.main=cex.main, legend.cex=legend.cex,
                            logScale=TRUE,...) 
  
  
}

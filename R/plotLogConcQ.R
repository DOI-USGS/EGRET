#' Sample data plot: concentration vs. discharge (log/log)
#'
#' Concentration data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param concMax numeric if you want to specify the maximum concentration value to display, you can do so with the argument concMax, otherwise it will be automatic
#' @param concMin numeric if you want to specify the minimum concentration value to display, you can do so with the argument concMin, otherwise it will be automatic
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param cex number
#' @param cex.axis number
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotLogConcQ(qUnit = 1)
#' plotLogConcQ(qUnit = 'thousandCfs')
plotLogConcQ<-function(localSample = Sample, localINFO = INFO, qUnit = 2, 
            tinyPlot = FALSE, concMax = NA, concMin = NA, printTitle = TRUE, cex=0.8, cex.axis=1.1,cex.main=1.1,...){

  plotConcQ(localSample = localSample, localINFO = localINFO, qUnit = qUnit, tinyPlot = tinyPlot,
            logScale=TRUE, concMax = concMax, concMin = concMin,printTitle = printTitle, 
            cex=cex, cex.axis=cex.axis,cex.main=cex.main,...)

}
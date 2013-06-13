#' Sample data plot:  observed log flux vs predicted log flux
#'
#' Concentration data come from a data frame named Sample which contains the sample data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit object of qUnit class. \code{\link{qConst}}
#' @param fluxMax numeric specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param oneToOneLine inserts 1:1 line
#' @param cex number
#' @param cex.axis number
#' @param cex.main number
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' plotLogFluxPred(fluxUnit = 1)
#' plotLogFluxPred(fluxUnit = 'kgDay')
plotLogFluxPred<-function (localSample = Sample, localINFO = INFO, fluxUnit = 3, 
                           fluxMax = NA, tinyPlot=FALSE, printTitle = TRUE, 
                           oneToOneLine=TRUE,cex=0.8, cex.axis=1.1,cex.main=1.1, ...) 
{

  plotFluxPred(localSample = localSample, localINFO = localINFO, fluxUnit = fluxUnit, fluxMax = fluxMax, 
                         printTitle = printTitle, oneToOneLine=oneToOneLine, 
                         cex=cex, cex.axis=cex.axis,cex.main=cex.main,tinyPlot=tinyPlot,logScale=TRUE,...)

}

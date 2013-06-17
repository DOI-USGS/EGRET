#' Graph of bserved versus estimated concentration as a log-log graph
#'
#' Data come from a data frame named Sample which contains the Sample Data. 
#' The metadata come from a data frame named INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param oneToOneLine inserts 1:1 line
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par should be set by user, if FALSE, EGRET chooses best graphical parameters.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' plotLogConcPred()
plotLogConcPred<-function(localSample = Sample, localINFO = INFO, concMax = NA, 
                          tinyPlot = FALSE, printTitle = TRUE,cex=0.8,col="black",lwd=1, 
                          cex.axis=1.1,cex.main=1.1, oneToOneLine=TRUE, customPar=FALSE, ...){
  # this function shows observed versus estimated concentration
  # estimated log concentration on the x-axis (these are prior to bias correction), 
  # observed log concentration on y-axis 
  # these estimates are from a "leave-one-out" cross validation application of WRTDS

  
  plotConcPred(localSample = localSample, localINFO = localINFO, concMax = concMax, logScale=TRUE,
               printTitle = printTitle,tinyPlot=tinyPlot,cex=cex, 
               cex.axis=cex.axis,cex.main=cex.main, customPar=customPar,col=col,lwd=lwd,...)


}

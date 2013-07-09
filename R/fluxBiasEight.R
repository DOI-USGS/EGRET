#' Produces 8-panel plot that is useful for determining if there is a flux bias problem
#'
#' @description
#' These plots use the jack-knife estimates from WRTDS to investigate the potential flux bias problem. 
#' It can also be used for estimates constructed by other methods (such as LOADEST) if the results are
#' stored in a data frame organized like the Sample data frame.  It allows additional label information
#' to indicate what method is used. 
#'
#' Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#' contains an INFO, Daily, and Sample dataframes, then the following R code will produce a plot:
#' \code{fluxBiasMulti()}
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param moreTitle string specifying some additional information to go in figure title, typically some information about the specific estimation method used, default is no additional information
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' pdf("fluxBiasMulti.pdf", height=9, width=8)
#' fluxBiasMulti()
#' dev.off()
fluxBiasMulti<-function (localSample = Sample, localDaily = Daily, 
                         localINFO = INFO, qUnit = 2, fluxUnit = 3, moreTitle = "WRTDS", 
                         cex = 0.7, cex.axis = 1.1,cex.main=1.1,
                         col="black", lwd=1,...){
  
  par(oma = c(0, 10, 4, 10),mfrow=c(4,2))
  plotResidPred(localSample = localSample, localINFO = localINFO, 
                stdResid = FALSE, tinyPlot=TRUE, printTitle = FALSE,cex=cex, 
                cex.axis = cex.axis, col=col,lwd=lwd,...)
  plotResidQ(localSample = localSample, localINFO = localINFO, 
             qUnit, tinyPlot = TRUE, printTitle = FALSE,cex=cex, 
             cex.axis = cex.axis, col=col,lwd=lwd,...)
  plotResidTime(localSample = localSample, localINFO = localINFO, 
                printTitle = FALSE, tinyPlot=TRUE,cex=cex, 
                cex.axis = cex.axis, col=col,lwd=lwd,...)
  boxResidMonth(localSample = localSample, localINFO = localINFO, 
                printTitle = FALSE, tinyPlot=TRUE,cex=cex, 
                cex.axis = cex.axis,lwd=lwd,...)
  boxConcThree(localSample = localSample, localDaily = localDaily, 
               localINFO = localINFO, printTitle=FALSE, tinyPlot=TRUE,cex=cex, 
               cex.axis = cex.axis, lwd=lwd,...)
  plotConcPred(localSample = localSample, localINFO=localINFO, printTitle=FALSE, 
               tinyPlot=TRUE,cex=cex, 
               cex.axis = cex.axis, col=col,lwd=lwd,...)
  boxQTwice(localSample = localSample, localDaily = localDaily, 
            localINFO = localINFO, printTitle = FALSE, qUnit = qUnit,tinyPlot=TRUE,cex=cex, 
            cex.axis = cex.axis, lwd=lwd,...)
  plotFluxPred(localSample = localSample, localINFO = localINFO, 
               fluxUnit, tinyPlot = TRUE, printTitle = FALSE,cex=cex, 
               cex.axis = cex.axis, col=col,lwd=lwd,...)
  fluxBias <- fluxBiasStat(localSample = localSample)
  fB <- as.numeric(fluxBias[3])
  fB <- format(fB, digits = 3)
  title <- paste(localINFO$shortName, " ", localINFO$paramShortName, 
                 "\nModel is",moreTitle, "  Flux Bias Statistic", fB)
  mtext(title, cex = cex.main, outer = TRUE, font = 2)
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}

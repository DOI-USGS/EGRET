#' Produces alternative 6-panel plot that is useful for determining if there is a flux bias problem
#'
#' These plots use the jack-knife estimates from WRTDS to investigate the potential flux bias problem. 
#' It can also be used for estimates constructed by other methods (such as LOADEST) if the results are
#' stored in a data frame organized like the Sample data frame.  It allows additional label information
#' to indicate what method is used. 
#' The 6 graphs are: Log Concentration versus Log Discharge, Residual verus Log Discharge, Log Concentration versus Log Estimated Concentration (estimates made prior to bias adjustment),
#' Residuals versus Estimates (in log concentration space), Observed Flux versus Estimated Flux (2 plots, one in log space and the other in real space).
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param moreTitle string specifying some additional information to go in figure title, typically some information about the specific estimation method used, default is no additional information
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' Daily <- exDaily
#' INFO <- exINFO
#' 
#' fluxBiasMultiAlt(fluxUnit = 9,moreTitle="WRTDS")
fluxBiasMultiAlt<-function (localSample = Sample, localDaily = Daily, 
                         localINFO = INFO, qUnit = 2, fluxUnit = 3, moreTitle = "",...){
  
#   layout(rbind(c(1, 2), c(3, 4), c(5, 6)), heights = c(1, 1), 
#          widths = c(1, 1), respect = rbind(c(0, 0), c(0, 0), 
#                                               c(0, 0)))
  par(oma = c(0, 6.8, 4, 6.8),mfrow=c(3,2))
  plotResidPred(localSample = localSample, localINFO = localINFO, stdResid = FALSE, tinyPlot=TRUE, printTitle = FALSE)
  plotResidQ(localSample = localSample, localINFO = localINFO, 
             qUnit, tinyPlot = TRUE, printTitle = FALSE)
  plotResidTime(localSample = localSample, localINFO = localINFO, printTitle = FALSE,tinyPlot=TRUE)
  boxResidMonth(localSample = localSample, localINFO = localINFO, printTitle = FALSE,tinyPlot=TRUE)
  boxQTwice(localSample = localSample, localDaily = localDaily, localINFO = localINFO, 
            printTitle = FALSE, qUnit = qUnit,tinyPlot=TRUE)
  
  plotFluxPred(localSample = localSample, localINFO = localINFO, 
               fluxUnit, tinyPlot = TRUE, printTitle = FALSE)
  fluxBias <- fluxBiasStat(localSample = localSample)
  fB <- as.numeric(fluxBias[3])
  fB <- format(fB, digits = 3)
  title <- paste(localINFO$shortName, " ", localINFO$paramShortName, 
                 "\nModel is",moreTitle, "  Flux Bias Statistic", fB)
  mtext(title, cex = 1.2, outer = TRUE, font = 2)
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}

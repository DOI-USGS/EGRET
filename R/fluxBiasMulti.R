#' Produces a 6-panel plot that is useful for determining if there is a flux bias problem
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
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' 
#' fluxBiasMulti(fluxUnit = 9,moreTitle="WRTDS")
fluxBiasMulti<-function (localSample = Sample, localDaily = Daily, 
                         localINFO = INFO, qUnit = 2, fluxUnit = 3, moreTitle = "",...){
  
#   layout(rbind(c(1, 2), c(3, 4), c(5, 6)), heights = c(1, 1), 
#          widths = c(1, 1), respect = rbind(c(0, 0), c(0, 0), 
#                                               c(0, 0)))
  par(oma = c(0, 6.8, 4, 6.8),mfrow=c(3,2))
  plotLogConcQ(localSample = localSample, localINFO = localINFO, 
               qUnit, tinyPlot = TRUE, printTitle = FALSE,rmSciX=TRUE,...)
  plotResidQ(localSample = localSample, localINFO = localINFO, qUnit,
             rmSciX=TRUE,tinyPlot=TRUE,printTitle=FALSE, ...)
  plotLogConcPred(localSample = localSample, localINFO = localINFO, 
                  tinyPlot = TRUE, printTitle = FALSE,...)
  plotResidPred(localSample = localSample, localINFO = localINFO, 
                tinyPlot = TRUE, printTitle = FALSE,...)  
  plotFluxPred(localSample = localSample, localINFO = localINFO, 
               fluxUnit, tinyPlot = TRUE, printTitle = FALSE,...)
  plotLogFluxPred(localSample = localSample, localINFO = localINFO, 
                  fluxUnit, tinyPlot = TRUE, printTitle = FALSE,...)
  
  fluxBias <- fluxBiasStat(localSample = localSample)
  fB <- as.numeric(fluxBias[3])
  fB <- format(fB, digits = 4)
  fB1<- as.numeric(fluxBias[1])
  fB1<- format(fB1, digits =4)
  fB2<- as.numeric(fluxBias[2])
  fB2<- format(fB2, digits =4)
  title <- paste(localINFO$shortName, " ", localINFO$paramShortName, 
                 "\nFlux Bias Statistic", fB, " (",fB1,",",fB2,") ",moreTitle)
  mtext(title, cex = 1.2, outer = TRUE, font = 2)
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}

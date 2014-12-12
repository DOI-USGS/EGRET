#' Produces 8-panel plot that is useful for determining if there is a flux bias problem
#'
#' @description
#' These plots use the jack-knife estimates from WRTDS to investigate the potential flux bias problem. 
#' It can also be used for estimates constructed by other methods (such as LOADEST) if the results are
#' stored in a data frame organized like the Sample data frame.  It allows additional label information
#' to indicate what method is used. 
#'
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata
#'
#' @param eList named list with at least Sample, Daily, and INFO dataframes
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{printFluxUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param moreTitle character specifying some additional information to go in figure title, typically some information about the specific estimation method used, default is no additional information
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' pdf("fluxBiasMulti.pdf", height=9, width=8)
#' fluxBiasMulti(eList)
#' dev.off()
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList,paStart=6,paLong=3)
#' pdf("fluxBiasMultiSummer.pdf", height=9, width=8)
#' fluxBiasMulti(eList)
#' dev.off()
fluxBiasMulti<-function (eList, qUnit = 2, fluxUnit = 3, moreTitle = "WRTDS", 
                         cex = 0.7, cex.axis = 1.1,cex.main=1.1,
                         col="black", lwd=1,...){
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  possibleGoodUnits <- c("mg/l","mg/l as N", "mg/l as NO2", 
                         "mg/l as NO3","mg/l as P","mg/l as PO3","mg/l as PO4","mg/l as CaCO3",
                         "mg/l as Na","mg/l as H","mg/l as S","mg/l NH4" )
  
  allCaps <- toupper(possibleGoodUnits)
  localUnits <- toupper(localINFO$param.units)
  
  if(!(localUnits %in% allCaps)){
    warning("Expected concentration units are mg/l, \nThe INFO dataframe indicates:",localINFO$param.units,
            "\nFlux calculations will be wrong if units are not consistent")
  }
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  par(oma = c(0, 10, 4, 10),mfrow=c(4,2))
  plotResidPred(eList, 
                stdResid = FALSE, tinyPlot=TRUE, printTitle = FALSE,cex=cex, 
                cex.axis = cex.axis, col=col,lwd=lwd,...)
  plotResidQ(eList, 
             qUnit, tinyPlot = TRUE, printTitle = FALSE,cex=cex, 
             cex.axis = cex.axis, col=col,lwd=lwd,...)
  plotResidTime(eList, 
                printTitle = FALSE, tinyPlot=TRUE,cex=cex, 
                cex.axis = cex.axis, col=col,lwd=lwd,...)
  boxResidMonth(eList, 
                printTitle = FALSE, tinyPlot=TRUE,cex=cex, 
                cex.axis = cex.axis,lwd=lwd,...)
  boxConcThree(eList, 
               localINFO = localINFO, printTitle=FALSE, tinyPlot=TRUE,cex=cex, 
               cex.axis = cex.axis, lwd=lwd,...)
  plotConcPred(eList, printTitle=FALSE, 
               tinyPlot=TRUE,cex=cex, 
               cex.axis = cex.axis, col=col,lwd=lwd,...)
  boxQTwice(eList, printTitle = FALSE, qUnit = qUnit,tinyPlot=TRUE,cex=cex, 
            cex.axis = cex.axis, lwd=lwd,...)
  plotFluxPred(eList, 
               fluxUnit, tinyPlot = TRUE, printTitle = FALSE,cex=cex, 
               cex.axis = cex.axis, col=col,lwd=lwd,...)
  fluxBias <- fluxBiasStat(localSample)
  fB <- as.numeric(fluxBias[3])
  fB <- format(fB, digits = 3)
  title <- paste(localINFO$shortName, ", ", localINFO$paramShortName, 
                 "\nModel is ",moreTitle, " Flux Bias Statistic", fB, sep="")
  if("" == title2){
    mtext(title, cex = cex.main, outer = TRUE, font = 1.8)
  } else {
    title <- paste(title, title2, sep="\n")
    mtext(title, cex = cex.main*.75, outer = TRUE, font = 1.8)    
  }
  
  par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
  
}

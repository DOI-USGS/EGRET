#' Produces a 4 panel plot that gives an overview of the data set prior to any processing
#'
#' @description
#' This function produces the 4 plots based only on the data stored in the eList.  
#' The four plots are 1) log concentration versus log discharge, 2) log concentration versus time
#' 3) a boxplot of log concentration by month, and 
#' 4) a side-by-side boxplot of the sampled discharges and all daily discharges. 
#' To save space, the graphic is labeled only at the top of the 4 graph display. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'
#' @param eList named list with at least Daily, Sample, and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or 
#' numeric represented the short code, or character representing the descriptive name.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param logScaleConc logical if TRUE y in concentration graphs plotted in log axis. Default is TRUE.
#' @param logScaleQ logical if TRUE y in streamflow graphs plotted in log axis. Default is TRUE.
#' @param randomCensored logical. Show censored values as randomized. Default is FALSE.  
#' If TRUE, makeAugmentedSample must be run first.
#' @param concLab object of concUnit class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @keywords graphics water-quality statistics
#' @seealso \code{\link{plotConcQ}}, \code{\link{boxConcMonth}}, 
#' \code{\link{plotConcTime}}, \code{\link{boxQTwice}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' multiPlotDataOverview(eList, qUnit=1)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' multiPlotDataOverview(eList, qUnit=1) 
#' 
#' Custom axes:
#' eList$INFO$param.units <- "ng"
#' qConst_precip <- new("qUnit",
#'                      qShortName = "   mm  ",
#'                      qUnitFactor = 1,
#'                      qUnitName = "Millimeter",
#'                      qUnitExpress = expression(paste("Precipitation in ",mm)),
#'                      qUnitTiny = expression(paste("Precipitation ", "(", mm, ")")),
#'                      shortCode = 1,
#'                      unitUSGS = "Precipitation, in mm",
#'                      prefix = "Precipitation")
#' 
#' deposition <- new("concUnit",
#'                   longPrefix = "Deposition",
#'                   shortPrefix = "Dep")
#' 
#' multiPlotDataOverview(eList, 
#'                       qUnit = qConst_precip, 
#'                       concLab = deposition)
#' 
#' 
multiPlotDataOverview <- function (eList, 
                                   qUnit = 2,
                                   cex.main = 1.2,
                                   randomCensored = FALSE,
                                   logScaleConc = TRUE,
                                   logScaleQ = TRUE,
                                   concLab = 1){
    
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  if(paLong == 12){
    title2 <- ""
  } else {
    title2 <- setSeasonLabelByUser(paStartInput = paStart,
                                   paLongInput = paLong)
  }
  
  par(mfcol = c(2,2),
      oma = c(0,2.4,4.5,2.4),
      tcl = 0.5)
  
  plotConcQ(eList, 
            qUnit = qUnit,
            tinyPlot = TRUE,
            printTitle = FALSE,
            rmSciX = TRUE,
            logScale = logScaleConc,
            randomCensored = randomCensored,
            concLab = concLab)
  boxConcMonth(eList,
               printTitle = FALSE,
               tinyPlot = TRUE,
               logScale = logScaleConc, 
               concLab = concLab)
  plotConcTime(eList, 
               printTitle = FALSE,
               tinyPlot = TRUE,
               logScale = logScaleConc,
               concLab = concLab,
               randomCensored = randomCensored)
  boxQTwice(eList,
            printTitle = FALSE,
            qUnit = qUnit, 
            tinyPlot = TRUE, 
            logScale = logScaleQ)
  
  title <- paste(localINFO$shortName,"\n",localINFO$paramShortName)
  
  if("" == title2){
    mtext(title,cex=cex.main,outer=TRUE,font=2)
  } else {
    title <- paste(title, title2, sep="\n")
    mtext(title, cex = cex.main*.75, outer = TRUE, font = 2)    
  }
  
  par(mfcol = c(1,1),
      oma = c(0,0,0,0))
  
}
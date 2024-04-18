#' Table of annual results for discharge, concentration and flux
#'
#' Produce an ASCII table showing: year, mean discharge, mean concentration, flow-normalized concentration, 
#' mean flux, and flow-normalized flux. Note that the flux and flow-normalized flux are rates and not a mass.  As such a value for some period shorter than a full year 
#' could be larger than the value for a full year.
#'
#' Can also procude a table for any Period of Analysis (individual months or sequence of months) using \code{\link{setPA}}. 
#'
#' @param eList named list with at least Daily and INFO dataframes
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{printFluxUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param localDaily data frame to override eList$Daily
#' @return results dataframe, if returnDataFrame=TRUE
#' @keywords water-quality statistics
#' @export
#' @return dataframe with year, discharge, concentration,
#' flow-normalized concentration, flux, and flow-normalized concentration columns. 
#' If the eList was run through WRTDSKalman, an additional column generalized flux
#' is included.
#' @examples
#' eList <- Choptank_eList
#' # Water Year:
#' \donttest{
#' tableResults(eList, fluxUnit = 8)
#' df <- tableResults(eList, fluxUnit = 1)
#' df
#' # Spring:
#' eList <- setPA(eList, paStart = 3, paLong = 3)
#' tableResults(eList, fluxUnit = 1, qUnit = "cfs")
#' }
tableResults<-function(eList, qUnit = 2, fluxUnit = 9, localDaily = NA) {
  
  localINFO <- getInfo(eList)
  
  if(all(is.na(localDaily))){
    localDaily <- eList$Daily
  }

  if(all(c("paStart","paLong") %in% names(localINFO))){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localAnnualResults <- setupYears(paStart=paStart,paLong=paLong, localDaily = localDaily)
  localAnnualResults <- localAnnualResults[rowSums(is.na(localAnnualResults[,c("Conc","Flux","FNConc","FNFlux")])) != 4,]

  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################ 
  
  fluxFactor<-fluxUnit@unitFactor
  qFactor<-qUnit@qUnitFactor
  fName<-fluxUnit@shortName
  qName<-qUnit@qShortName
  
  fNameNoSpace <- gsub(" ","", fName)
  qNameNoSpace <- gsub(" ","", qName)
  
  periodName<-setSeasonLabel(localAnnualResults = localAnnualResults)
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  if(hasFlex){
    periodName <- paste(periodName,"*")
  }
  cat("\n  ",localINFO$shortName,"\n  ",localINFO$paramShortName)
  cat("\n  ",periodName,"\n")
  
  period <- eList$INFO$paLong/12
  
  c1 <- format(trunc(localAnnualResults$DecYear + period/2), width=7)
  c2 <- format(localAnnualResults$Q*qFactor,digits=3, width=9)
  c3 <- format(localAnnualResults$Conc,digits=3, width=9)
  c4 <- format(localAnnualResults$FNConc,digits=3, width=9)
  c5 <- format(localAnnualResults$Flux*fluxFactor, digits=3, width=9)
  c6 <- format(localAnnualResults$FNFlux*fluxFactor, digits=3, width=9)
  
  if(all(c("GenFlux", "GenConc") %in% names(localAnnualResults))){
    c8 <- format(localAnnualResults$GenFlux*fluxFactor,digits=3,width=9)
    c7 <- format(localAnnualResults$GenConc, digits=3, width=9)
    cat("\n   Year   Discharge    Conc    FN_Conc   GenConc     Flux    FN_Flux   GenFlux")
    cat("\n            ", qName, "         mg/L             ", fName, "\n\n")
    results<-data.frame(c1, c2, c3, c4, c7, c5, c6, c8)
    colnames(results) <- c("Year", paste0("Discharge [", qNameNoSpace, "]"),
                           "Conc [mg/L]", 
                           "FN Conc [mg/L]", 
                           "GenConc [mg/L]",
                           paste0("Flux [", fNameNoSpace, "]"), 
                           paste0("FN Flux [", fNameNoSpace, "]"),
                           paste0("GenFlux [", fNameNoSpace,"]")) 
  } else {
    cat("\n   Year   Discharge    Conc    FN_Conc     Flux    FN_Flux")
    cat("\n         ", qName, "         mg/L         ", fName, "\n\n")

    results<-data.frame(c1,c2,c3,c4,c5,c6)
    colnames(results) <- c("Year", paste0("Discharge [", qNameNoSpace, "]"),
                           "Conc [mg/L]", "FN Conc [mg/L]", 
                           paste0("Flux [", fNameNoSpace, "]"), 
                           paste0("FN Flux [", fNameNoSpace, "]") )    
  }
  
  utils::write.table(results,file="",quote=FALSE,col.names=FALSE,row.names=FALSE)
  
  origNames <- names(results)
  results <- data.frame(apply(results, 2, function(x) as.numeric(gsub(" ","", as.character(x)))))
  names(results) <- origNames
  
  invisible(results)  
}
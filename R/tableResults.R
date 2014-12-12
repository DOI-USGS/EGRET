#' Table of annual results for discharge, concentration and flux
#'
#' Produce an ASCII table showing: year, mean discharge, mean concentration, flow-normalized concentration, 
#' mean flux, and flow-normalized flux. 
#'
#' @param eList named list with at least Daily and INFO dataframes
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{printFluxUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @return results dataframe, if returnDataFrame=TRUE
#' @keywords water-quality statistics
#' @export
#' @return dataframe with year, discharge, concentration, flow-normalized concentration, flux, and flow-normalized concentration columns. 
#' @examples
#' eList <- Choptank_eList
#' # Water Year:
#' tableResults(eList, fluxUnit = 1)
#' tableResults(eList, fluxUnit = 'kgDay', qUnit = 'cms')
#' returnedTable <- tableResults(eList, fluxUnit = 1)
#' # Winter:
#' eList <- setPA(eList, paLong=3,paStart=12)
#' tableResults(eList, fluxUnit = 1)
tableResults<-function(eList, qUnit = 2, fluxUnit = 9) {
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localAnnualResults <- setupYears(paStart=paStart,paLong=paLong, localDaily = localDaily)
  
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
  
  cat("\n  ",localINFO$shortName,"\n  ",localINFO$paramShortName)
  cat("\n  ",periodName,"\n")
  cat("\n   Year   Discharge    Conc    FN_Conc     Flux    FN_Flux")
  cat("\n         ",qName,"         mg/L         ",fName,"\n\n")
  
  c1<-format(trunc(localAnnualResults$DecYear),width=7)
  c2<-format(localAnnualResults$Q*qFactor,digits=3,width=9)
  c3<-format(localAnnualResults$Conc,digits=3,width=9)
  c4<-format(localAnnualResults$FNConc,digits=3,width=9)
  c5<-format(localAnnualResults$Flux*fluxFactor,digits=3,width=9)
  c6<-format(localAnnualResults$FNFlux*fluxFactor,digits=3,width=9)
  results<-data.frame(c1,c2,c3,c4,c5,c6)
  colnames(results) <- c("Year", paste("Discharge [", qNameNoSpace, "]", sep=""), "Conc [mg/L]", "FN Conc [mg/L]", paste("Flux [", fNameNoSpace, "]", sep=""), paste("FN Flux [", fNameNoSpace, "]", sep="") )
  
  write.table(results,file="",quote=FALSE,col.names=FALSE,row.names=FALSE)
  
  origNames <- names(results)
  results <- data.frame(apply(results, 2, function(x) as.numeric(gsub(" ","", as.character(x)))))
  names(results) <- origNames
  
  invisible(results)  
}
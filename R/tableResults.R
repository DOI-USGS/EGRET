#' Table of annual results for discharge, concentration and flux
#'
#' Produce an ASCII table showing: year, mean discharge, mean concentration, flow-normalized concentration, 
#' mean flux, and flow-normalized flux. 
#' Uses results stored in AnnualResults and INFO data frames.
#'
#' @param localDaily data frame that contains the flow data, default name is Daily
#' @param localINFO data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param returnDataFrame logical.  If a dataframe is required, set this to TRUE.  Otherwise, the default is FALSE.
#' @return results dataframe, if returnDataFrame=TRUE
#' @keywords water-quality statistics
#' @export
#' @return dataframe with year, discharge, concentration, flow-normalized concentration, flux, and flow-normalized concentration columns. 
#' @examples
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' # Water Year:
#' tableResults(fluxUnit = 1)
#' tableResults(fluxUnit = 'kgDay', qUnit = 'cms')
#' returnedTable <- tableResults(fluxUnit = 1, returnDataFrame = TRUE)
#' # Winter:
#' INFO <- setPA(paLong=3,paStart=12)
#' tableResults(fluxUnit = 1)
tableResults<-function(localDaily = Daily, localINFO = INFO, qUnit = 2, fluxUnit = 9, returnDataFrame = FALSE) {
  
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
  
  if (!returnDataFrame) {
    return()
  }
  
  origNames <- names(results)
  results <- data.frame(apply(results, 2, function(x) as.numeric(gsub(" ","", as.character(x)))))
  names(results) <- origNames
  
  return(results)  
}
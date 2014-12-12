#' Create a table of the changes in flow-normalized values between various points in time in the record
#'
#' These tables describe trends in flow-normalized concentration and in flow-normalized flux. 
#' They are described as changes in real units or in percent and als as slopes in real units per year or in percent per year.
#' They are computed over pairs of time points.  These time points can be user-defined or
#' they can be set by the program to be the final year of the record and a set of years that are multiple of 5 years prior to that.
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param fluxUnit object of fluxUnit class. \code{\link{printFluxUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param yearPoints numeric vector listing the years for which the change or slope computations are made, they need to be in chronological order.  For example yearPoints=c(1975,1985,1995,2005), default is NA (which allows the program to set yearPoints automatically)
#' @keywords water-quality statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water Year:
#' tableChange(eList, fluxUnit=6,yearPoints=c(2001,2005,2008,2009))
#' tableChange(eList, fluxUnit=9) 
#' # Winter:
#' eList <- setPA(eList, paStart=12,paLong=3)
#' tableChange(eList, fluxUnit=6,yearPoints=c(2001,2005,2008,2009))
tableChange<-function(eList, fluxUnit = 9, yearPoints = NA) {
  
  localINFO <- getInfo(eList)
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
  
  localAnnualResults <- setupYears(paStart=paStart,paLong=paLong, localDaily = localDaily)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################ 
  firstYear<-trunc(localAnnualResults$DecYear[1])
  numYears<-length(localAnnualResults$DecYear)
  lastYear<-trunc(localAnnualResults$DecYear[numYears])
  defaultYearPoints<-seq(lastYear,firstYear,-5)
  numPoints<-length(defaultYearPoints)
  defaultYearPoints[1:numPoints]<-defaultYearPoints[numPoints:1]
  yearPoints<-if(is.na(yearPoints[1])) defaultYearPoints else yearPoints
  numPoints<-length(yearPoints)
  # these last three lines check to make sure that the yearPoints are in the range of the data	
  yearPoints<-if(yearPoints[numPoints]>lastYear) defaultYearPoints else yearPoints
  yearPoints<-if(yearPoints[1]<firstYear) defaultYearPoints else yearPoints
  numPoints<-length(yearPoints)
  fluxFactor<-fluxUnit@unitFactor
  fName<-fluxUnit@shortName
  cat("\n  ",localINFO$shortName,"\n  ",localINFO$paramShortName)
  periodName<-setSeasonLabel(localAnnualResults = localAnnualResults)
  cat("\n  ",periodName,"\n")
  header1<-"\n           Concentration trends\n   time span       change     slope    change     slope\n                     mg/L   mg/L/yr        %       %/yr"
  header2<-"\n\n\n                 Flux Trends\n   time span          change        slope       change        slope"
  blankHolder<-"      ---"
  results<-rep(NA,4)
  indexPoints<-yearPoints-firstYear+1
  numPointsMinusOne<-numPoints-1
  write(header1,file="")
  
  for(iFirst in 1:numPointsMinusOne) {
    xFirst<-indexPoints[iFirst]
    yFirst<-localAnnualResults$FNConc[indexPoints[iFirst]]
    iFirstPlusOne<-iFirst+1
    for(iLast in iFirstPlusOne:numPoints) {
      xLast<-indexPoints[iLast]
      yLast<-localAnnualResults$FNConc[indexPoints[iLast]]
      xDif<-xLast - xFirst
      yDif<-yLast - yFirst
      results[1]<-if(is.na(yDif)) blankHolder else format(yDif,digits=2,width=9)
      results[2]<-if(is.na(yDif)) blankHolder else format(yDif/xDif,digits=2,width=9)
      results[3]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst,digits=2,width=9)
      results[4]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst/xDif,digits=2,width=9)
      cat("\n",yearPoints[iFirst]," to ",yearPoints[iLast],results)
    }}
  write(header2,file="")
  cat("              ",fName,fName,"/yr      %         %/yr")
  for(iFirst in 1:numPointsMinusOne) {
    xFirst<-indexPoints[iFirst]
    yFirst<-localAnnualResults$FNFlux[indexPoints[iFirst]]*fluxFactor
    iFirstPlusOne<-iFirst+1
    for(iLast in iFirstPlusOne:numPoints) {
      xLast<-indexPoints[iLast]
      yLast<-localAnnualResults$FNFlux[indexPoints[iLast]]*fluxFactor
      xDif<-xLast - xFirst
      yDif<-yLast - yFirst
      results[1]<-if(is.na(yDif)) blankHolder else format(yDif,digits=2,width=12)
      results[2]<-if(is.na(yDif)) blankHolder else format(yDif/xDif,digits=2,width=12)
      results[3]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst,digits=2,width=12)
      results[4]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst/xDif,digits=2,width=12)
      cat("\n",yearPoints[iFirst]," to ",yearPoints[iLast],results)
    }
  }
}
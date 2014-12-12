#' Prints table of change metrics for a given streamflow statistic
#'
#' Part of the flowHistory system.
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum. 
#' A dataframe is returned, as well as a printout in the R console.
#'
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param eList named list with at least Daily and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param yearPoints A vector of numeric values, specifying the years at which change metrics are to be calculated, default is NA (which allows the function to set these automatically), yearPoints must be in ascending order
#' @keywords streamflow statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' tableFlowChange(eList, istat=5,yearPoints=c(2001,2005,2009))
#' df <- tableFlowChange(eList, istat=5,yearPoints=c(2001,2005,2009))
tableFlowChange<-function(eList, istat, qUnit = 1, runoff = FALSE, 
                          yearPoints = NA) {
  
  localAnnualSeries <- makeAnnualSeries(eList)
  localINFO <- getInfo(eList)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################ 
  
  firstYear<-min(localAnnualSeries[1,istat,],na.rm=TRUE)
  firstYear<-trunc(firstYear)
  lastYear<-max(localAnnualSeries[1,istat,],na.rm=TRUE)
  lastYear<-trunc(lastYear)
  numYears<-lastYear - firstYear + 1
  defaultYearPoints<-seq(lastYear,firstYear,-5)
  numPoints<-length(defaultYearPoints)
  defaultYearPoints[1:numPoints]<-defaultYearPoints[numPoints:1]
  yearPoints<-if(is.na(yearPoints[1])) defaultYearPoints else yearPoints
  numPoints<-length(yearPoints)
  # these last three lines check to make sure that the yearPoints are in the range of the data	
  yearPoints<-if(yearPoints[numPoints]>lastYear) defaultYearPoints else yearPoints
  yearPoints<-if(yearPoints[1]<firstYear) defaultYearPoints else yearPoints
  numPoints<-length(yearPoints)
  qFactor<-qUnit@qUnitFactor
  qName<-qUnit@qShortName
  qSmooth<-localAnnualSeries[3,istat,]
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  cat("\n  ",localINFO$shortName)
  periodName<-setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)
  cat("\n  ",periodName)
  nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
  cat("\n   ",nameIstat[istat],"\n")
  header2<-"\n             Streamflow Trends\n   time span          change        slope       change        slope"
  blankHolder<-"      ---"
  results<-rep(NA,4)
  indexPoints<-yearPoints-firstYear+1
  numPointsMinusOne<-numPoints-1
  write(header2,file="")
  unitsText<-if(runoff) "mm/day" else qUnit@qShortName
  
  if(runoff){
    cat("                      ",unitsText,"      ",unitsText,"/yr       %           %/yr",sep="")
  } else {
    formatSpacing <- if (3 == nchar(gsub(" ", "",unitsText))) "       " else "   "
    cat("                     ",unitsText,formatSpacing,gsub(" ", "",unitsText),"/yr        %            %/yr",sep="")
  }
  header<-c("year1","year2",paste("change[",gsub(" ", "",unitsText),"]",sep=""),paste("slope[",gsub(" ", "",unitsText),"/yr]",sep=""),"change[%]","slope[%/yr]")
  resultDF <- as.data.frame(sapply(1:6, function(x) data.frame(x)))
  colnames(resultDF) <- header
  for(iFirst in 1:numPointsMinusOne) {
    xFirst<-indexPoints[iFirst]
    yFirst<-qSmooth[indexPoints[iFirst]]
    iFirstPlusOne<-iFirst+1
    for(iLast in iFirstPlusOne:numPoints) {
      xLast<-indexPoints[iLast]
      yLast<-qSmooth[indexPoints[iLast]]
      xDif<-xLast - xFirst
      yDif<-yLast - yFirst
      results[1]<-if(is.na(yDif)) blankHolder else format(yDif,digits=2,width=12)
      results[2]<-if(is.na(yDif)) blankHolder else format(yDif/xDif,digits=2,width=12)
      results[3]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst,digits=2,width=12)
      results[4]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst/xDif,digits=2,width=12)
      cat("\n",yearPoints[iFirst]," to ",yearPoints[iLast],results)
      resultDF <- rbind(resultDF, c(yearPoints[iFirst], yearPoints[iLast],results))
    }
  }
  cat("\n")
  resultDF <- resultDF[-1,]
  row.names(resultDF) <- NULL
  resultDF <- as.data.frame(lapply(resultDF,as.numeric))
  colnames(resultDF) <- header

  
  invisible(resultDF)
}

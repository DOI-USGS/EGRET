#####################################################
# 07June2012  EGRET.R
# RMH  17Jan2012  various functions modified and added for flux bias analysis
# RMH  21Jan2012  Added plotConcHist
# RMH  23Jan2012  Added plotContours
# RMH  23Jan2012  Enhancements of graphics, and added scientific notation to axes
# RMH  24Jan2012  Starting to document for packaging
# LDC  27Jan2012  Including S4 objects for flux and qUnits and some packaging docs
# RMH  31Jan2012  More documentation for packaging and adding a few more functions
# RMH  01Feb2012  Small correction to plotLogConcTime also added plotConcQSmooth and put plotContours back in
# RMH  01Feb2012  found that some of the roxygen2 documentation was missing from plotResidPred so this was added in
# RMH  09Feb2012  added another function plotLogConcQSmooth, still needs roxygen2 documentation also deleted un-necessary code about units
# RMH  09Feb2012  added the surfaces data set and finished of some roxygen2 code
# RMH  13Feb2012  merging in Smooths.R and estimateAndAggregate.R
# RMH  23Feb2012  minor adjustments to some of the plotting routines
# RMH  09Mar2012  small changes to flowDuration, plotLogFluxQ, fluxBiasStat, fluxBiasMulti
# RMH  13Mar2012  added in all the code that had been in allOther.R
# RMH  16Mar2012  changed all of the example data sets (also changed cex=0.4 to cex=0.7 in plots)
# RMH  21Mar2012  fixed small bugs in plotConcHist, plotFluxHist, and saveResults
# RMH  23Mar2012  cosmetic improvements in plotContours and plotDiffContours
# RMH  23Apr2012  fixed typos in estDailyWithoutNormalization and changes to vertical axes in 3 plots
# RMH  25Apr2012  Edited the text in descriptions to add periods at the end of sentences, also fixed the example in plotConcHist
# RMH  05Jun2012  Added several new functions to increase the capabilities of flowHistory
# RMH  07Jun2012  three bug fixes (in plotFlowSingle, plotSDLogQ, and estDailyFromSurfaces)
################################################################
#' EGRET package (except for dataRetrieval) includes WRTDS and flowHistory
#'
#' \tabular{ll}{
#' Package: \tab EGRET\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2012-04-23\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do WRTDS and flowHistory analysis,
#'  and produce graphs and tables of data and results from these analyses.
#'
#' @name EGRET-package
#' @docType package
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}, Laura De Cicco \email{ldecicco@@usgs.gov}
#' @references Hirsch, R. M., Moyer, D. L. and Archfield, S. A. (2010), Weighted Regressions on Time, Discharge, and Season (WRTDS), with an Application to Chesapeake Bay River Inputs. JAWRA Journal of the American Water Resources Association, 46: 857-880. doi: 10.1111/j.1752-1688.2010.00482.x
#' @keywords water-quality graphics streamflow statistics 
NULL

#' Data included in EGRET
#'
#' Example data representing Streamflow and Nitrate from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exDailyStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing Streamflow and Nitrate from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name exDailyEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exSampleStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name exSampleEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exINFOStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#' as augmented by the functions setupYears (for WRTDS) and setPA (for flowHistory)
#'
#' @name exINFOEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing annual WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function setupYears
#'
#' @name exAnnualResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing annual Series of streamflow statistics from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function makeAnnualSeries
#'
#' @name exannualSeries
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing monthly WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function calculateMonthlyResults
#'
#' @name exMonthlyResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing surfaces, from the fitted model
#' of Nitrate data for the Choptank River at Greensboro, MD,  USGS data
#' output from the function estSurfaces
#'
#' @name exsurfaces
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Flux units included in EGRET
#'
#' Flux units
#'
#' @name fluxConst
#' @docType data
NULL

#' Flow units included in EGRET
#'
#' Flow units
#'
#' @name qConst
#' @docType data
NULL

#' Month labels included in EGRET
#'
#' Month labels
#'
#' @name monthInfo
#' @docType data
NULL
##############################################################################
#' Creates a character string that describes the period of analysis, when the period of analysis is being set by the user and not from AnnualResults
#'
#' The period of analysis can be of any length from 1 month to 12 months. 
#' The period of analysis can have any starting month from 1 (January) through 12 (December). 
#' This function produces a character string that describes this period of analysis. 
#' For example "water year", "calendar year", "year starting with April", or 
#' "Season consisting of April, May, June". 
#' There is an alternative version of this function for the case where AnnualResults exists. 
#' And we want to use the period of analysis defined there. 
#' That function is called setSeasonLabel. 
#'
#' @param paStartInput numeric the month which is the start of the period of analysis, default is 10 which would be the case if the period of analysis is the water year
#' @param paLongInput numeric the length of the the period of analysis, in months, default is 12 which would be the case if the period of analysis is the water year
#' @keywords water quality graphics
#' @export
#' @return periodName string which describes the period of analysis
#' @examples
#' setSeasonLabelByUser(paStartInput=1,paLongInput=12)
#' setSeasonLabelByUser(paStartInput=4,paLongInput=3)
setSeasonLabelByUser<-function(paStartInput = 10, paLongInput = 12){
# this function sets up text variable used to label graphs and
# tables, defining what the period of analysis is
  paStart<-paStartInput
  paLong<-paLongInput
  index<-seq(paStart,paStart+paLong-1)
  index<-ifelse(index>12,index-12,index)
#   monthList<-c(monthAbbrev[index[1:paLong]])
  monthList <- sapply(index[1:paLong], function(x){monthInfo[[x]]@monthAbbrev})
  monthList<-paste(monthList,collapse=" ")
#   temp1<- c("Year Starting With",monthFull[paStart])
  temp1<- c("Year Starting With",monthInfo[[paStart]]@monthFull)
  temp1<-paste(temp1,collapse=" ")
  temp2<- "Water Year"
  temp3<- "Calendar Year"
  temp4<- c("Season Consisting of",monthList)
  temp4<-paste(temp4,collapse=" ")
  periodName<-temp4
  periodName<-if(paLong==12) temp1 else periodName
  periodName<-if(paLong==12&paStart==10) temp2 else periodName
  periodName<-if(paLong==12&paStart==1) temp3 else periodName
  return(periodName)	
}
#
#
#' Table of annual results for discharge, concentration and flux
#'
#' Produce an ASCII table showing: year, mean discharge, mean concentration, flow-normalized concentration, 
#' mean flux, and flow-normalized flux. 
#' Uses results stored in AnnualResults and INFO data frames.
#'
#' @param localAnnualResults string specifying the name of the data frame that contains the results, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param returnDataFrame logical.  If a dataframe is required, set this to TRUE.  Otherwise, the default is FALSE.
#' @keywords water-quality statistics
#' @export
#' @return dataframe with year, discharge, concentration, flow-normalized concentration, flux, and flow-normalized concentration columns. 
#' @examples
#' AnnualResults <- exAnnualResults
#' INFO <- exINFOEnd
#' tableResults(fluxUnit = 1)
#' tableResults(fluxUnit = 'kgDay', qUnit = 'cms')
#' returnedTable <- tableResults(fluxUnit = 1, returnDataFrame = TRUE)
tableResults<-function(localAnnualResults = AnnualResults, localINFO = INFO, qUnit = 2, fluxUnit = 9, returnDataFrame = FALSE) {
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
  colnames(results) <- c("Year", paste("Discharge [", qName, "]", sep=""), "Conc [mg/L]", "FN_Conc [mg/L]", paste("Flux [", fNameNoSpace, "]", sep=""), paste("FN_Flux [", fNameNoSpace, "]", sep="") )
  
  write.table(results,file="",quote=FALSE,col.names=FALSE,row.names=FALSE)
  
  if (!returnDataFrame) {
    return()
  }
  
  return(results)  
}
#
#
#' Sets up tick marks for an axis for a graph with an arithmetic scale which starts at zero
#'
#' Axis tick marks that run from zero to some specified maximum, creates about 4 to 8 ticks marks.
#' 
#' @param yMax A numeric value for the maximum value to be plotted, it must be >0
#' @keywords statistics graphics
#' @export
#' @return yTicks A numeric vector representing the values for each of the tick marks
#' @examples
#' yPretty(7.8)
#' yPretty(125)
yPretty<-function(yMax) {
  #This function sets up the ticks on the y axis
  #To run from zero, to some reasonable maximum	
	yPair<-c(0,yMax)
	yTicks<-pretty(yPair,n=5)
	return(yTicks)
}
#
#
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
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' fluxBiasMulti(localSample = exSampleEnd, localDaily = exDailyEnd, localINFO = exINFOEnd, fluxUnit = 9,moreTitle="WRTDS")
fluxBiasMulti<-function (localSample = Sample, localDaily = Daily, 
    localINFO = INFO, qUnit = 2, fluxUnit = 3, moreTitle = "") 
{
    layout(rbind(c(1, 2), c(3, 4), c(5, 6)), heights = c(1, 1), 
        width = c(1.5, 1.5), respect = rbind(c(0, 0), c(0, 0), 
            c(0, 0)))
    par(oma = c(0, 6.8, 4, 6.8))
    plotLogConcQ(localSample = localSample, localINFO = localINFO, 
        qUnit, tinyPlot = TRUE, printTitle = FALSE)
    plotResidQ(localSample = localSample, localINFO = localINFO, qUnit,tinyPlot=TRUE,printTitle=FALSE)
    plotLogConcPred(localSample = localSample, localINFO = localINFO, 
        tinyPlot = TRUE, printTitle = FALSE)
    plotResidPred(localSample = localSample, localINFO = localINFO, 
        tinyPlot = TRUE, printTitle = FALSE)
    
    plotFluxPred(localSample = localSample, localINFO = localINFO, 
        fluxUnit, tinyPlot = TRUE, printTitle = FALSE)
    plotLogFluxPred(localSample = localSample, localINFO = localINFO, 
        fluxUnit, tinyPlot = TRUE, printTitle = FALSE)

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
#
#
#'  Compute the flux bias statistic: (mean of estimated flux - mean of observed flux)  / mean of observed flux
#'
#'  Computes three versions of the flux bias: 
#'   The first where all censored values are set to their miniumum. 
#'   The second where all censored values are set to their maximum. 
#'   The third which is the average of the other two. 
#'      In practice there is rarely a noticable difference among them.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @keywords water-quality statistics, bias
#' @export
#' @return fluxBias a vector of three numerical values, a lower bound, upper bound and an average estimate of the ratio of (mean estimated flux - mean observed flux) / mean estimated flux.  Typically one should use fluxBias[3]
#' @examples
#' fluxBiasStat(localSample = exSampleEnd) 
fluxBiasStat<-function(localSample = Sample) {
	sumLow<-sum(localSample$ConcLow*localSample$Q,na.rm=TRUE)
	sumHigh<-sum(localSample$ConcHigh*localSample$Q)
	sumEst<-sum(localSample$ConcHat*localSample$Q)
	bias1<-(sumEst-sumHigh)/sumEst
	bias2<-(sumEst-sumLow)/sumEst
	bias3<-(bias1+bias2)/2
	fluxBias<-data.frame(bias1,bias2,bias3)
	return(fluxBias)
}
#
#
#' Plot of the time series of daily concentration estimates and the sample values for the days that were sampled
#'
#' This plot is useful for visual examination of the ability of the WRTDS, or other model, to fit the 
#' data, seen in a time-series perspective. 
#' The graph is most useful when it covers a period of just a few years and not the complete record
#' but a complete record can be done by repeated use over a series of segments.
#'
#' @param startYear numeric specifying the starting date (expressed as decimal years, for example 1989.0) for the plot
#' @param endYear numeric specifiying the ending date for the plot 
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' plotConcTimeDaily(2001,2010, localSample = exSampleEnd, localDaily = exDailyEnd, localINFO = exINFOEnd)
plotConcTimeDaily<-function(startYear, endYear, localSample = Sample, localDaily = Daily, localINFO = INFO, tinyPlot = FALSE, concMax = NA, printTitle = TRUE){
  if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
  subSample<-subset(localSample,DecYear>=startYear)
  subSample<-subset(subSample,DecYear<=endYear)
  subDaily<-subset(localDaily,DecYear>=startYear)
  subDaily<-subset(subDaily,DecYear<=endYear)
  xSample<-subSample$DecYear
  xDaily<-subDaily$DecYear
  xLimits<-c(startYear,endYear)
  xTicks<-pretty(xLimits,n=5)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yLow<-subSample$ConcLow
  yHigh<-subSample$ConcHigh
  Uncen<-subSample$Uncen
  yAll<-c(subDaily$ConcDay,subSample$ConcHigh)
  maxYHigh<-if(is.na(concMax)) 1.05*max(yAll) else concMax
  yTicks<-yPretty(maxYHigh)
  yTop<-yTicks[length(yTicks)]
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed and Estimated Concentration versus Time") else ""
  plot(xSample,yHigh,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="Concentration in mg/L",main=plotTitle,pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2)
  axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  axis(2,tcl=0.5,las=1,at=yTicks)
  axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  par(new=TRUE)
  plot(xDaily,subDaily$ConcDay,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="",main="",type="l",cex.main=1.3,font.main=2,cex.lab=1.2)
  box()
  yLowVal<-ifelse(is.na(yLow),0,yLow)
  numSamples<-length(xSample)
  uncensoredIndex <- 1:numSamples
  uncensoredIndex <- uncensoredIndex[Uncen==0]
  segments(xSample[uncensoredIndex],yLowVal[uncensoredIndex],xSample[uncensoredIndex],yHigh[uncensoredIndex])
  par(mar=c(5,4,4,2)+0.1)
}
#
#
#
#
#' Graph of annual concentration and flow normalized concentration versus year
#'
#' Data come from a data frame named AnnualResults. 
#' The metadata come from a data frame named INFO.
#' The annual concentrations are "time-weighted" mean concentrations (as opposed to "flow-weighted"). 
#' The annual results reported are for a specified "period of analysis" which can be 
#' an entire water year, a calendar, a season or even an individual month.  
#' User specifies this period of analysis in the call to setupYears.
#'
#' @param yearStart numeric is the calendar year containing the first estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param yearEnd numeric is the calendar year just after the last estimated annual value to be plotted, default is NA (which allows it to be set automatically by the data)
#' @param localAnnualResults string specifying the name of the data frame that contains the annual results, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param plotFlowNorm logical variable if TRUE flow normalized line is plotted, if FALSE not plotted 
#' @keywords graphics water-quality statistics
#' @export
#' @seealso \code{\link{setupYears}}
#' @examples
#' yearStart <- 2001
#' yearEnd <- 2010
#' plotConcHist(yearStart, yearEnd, localAnnualResults = exAnnualResults, localINFO = exINFOEnd)
plotConcHist<-function(yearStart = NA, yearEnd = NA, localAnnualResults = AnnualResults, localINFO = INFO, concMax = NA, printTitle = TRUE, plotFlowNorm = TRUE){
# produces a graph of annual mean concentration and flow normalized concentration versus year
# AnnualResults contains the set of results
# typically yearStart and yearEnd should be integers, 
# yearStart is the start of the calendar year of the first estimated annual value
# yearEnd is the start of the calendar year after the last estimated annual value
# if you want to specify the maximum value, you can do so with the argument concMax, otherwise it will be automatic
	par(oma=c(3,0,3,0))
	par(mar=c(5,6,5,2))
	numYears <- length(localAnnualResults$DecYear)
	yearStart <- if(is.na(yearStart)) trunc(localAnnualResults$DecYear[1]) else yearStart
	yearEnd <- if(is.na(yearEnd)) trunc(localAnnualResults$DecYear[numYears])+1 else yearEnd 
	subAnnualResults<-subset(localAnnualResults,DecYear>=yearStart)
	subAnnualResults<-subset(subAnnualResults,DecYear<=yearEnd)
	annConc<-subAnnualResults$Conc
	fnConc<-subAnnualResults$FNConc
	concMax<-if(is.na(concMax)) 1.05*max(annConc,na.rm=TRUE) else concMax
	xVals<-subAnnualResults$DecYear
	xMin<-yearStart
	xMax<-yearEnd
	yearSpan<-c(xMin,xMax)
	xTicks<-pretty(yearSpan,n=9)
	numXTicks<-length(xTicks)
	xLeft<-xTicks[1]
	xRight<-xTicks[numXTicks]
	yTicks<-yPretty(concMax)
	yTop<-yTicks[length(yTicks)]
	periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
	title3<-if(plotFlowNorm) "\nMean Concentration (dots) & Flow Normalized Concentration (line)" else "\nAnnual Mean Concentration"
	title<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\n",periodName,title3) else ""
	plot(subAnnualResults$DecYear,annConc,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="Concentration in mg/L",main=title,pch=20,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2)
	if(plotFlowNorm) par(new=TRUE)
	if(plotFlowNorm) plot(subAnnualResults$DecYear,fnConc,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="",main="",type="l",col="green",lwd=3,cex=0.8,cex.main=1.0,cex.lab=1.2,font=2)
	axis(1,tcl=0.5,at=xTicks,labels=xTicks)
	axis(2,tcl=0.5,las=1,at=yTicks,cex.axis=1.1)
	axis(3,tcl=0.5,at=xTicks,labels=FALSE)
	axis(4,tcl=0.5,at=yTicks,labels=FALSE)
	box()
	par(oma=c(0,0,0,0))
	par(mar=c(5,4,4,2)+0.1)	
}
#
#
#' Create a table of the changes in flow-normalized values between various points in time in the record
#'
#' These tables describe trends in flow-normalized concentration and in flow-normalized flux. 
#' They are described as changes in real units or in percent and als as slopes in real units per year or in percent per year.
#' They are computed over pairs of time points.  These time points can be user-defined or
#' they can be set by the program to be the final year of the record and a set of years that are multiple of 5 years prior to that.
#'
#' @param localAnnualResults string specifying the name of the data frame that contains the concentration and discharge data, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param yearPoints numeric vector listing the years for which the change or slope computations are made, they need to be in chronological order.  For example yearPoints=c(1975,1985,1995,2005), default is NA (which allows the program to set yearPoints automatically)
#' @keywords water-quality statistics
#' @export
#' @examples
#' AnnualResults <- exAnnualResults
#' INFO <- exINFOEnd
#' tableChange(fluxUnit=6,yearPoints=c(2001,2005,2008,2009))
#' tableChange(fluxUnit=9) 
tableChange<-function(localAnnualResults = AnnualResults, localINFO = INFO, fluxUnit = 9, yearPoints = NA) {
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

#' Create a table of the changes in flow-normalized concentration or flux values between various points in time in the record
#'
#' This table describe trends in flow-normalized concentration or flux depending on if flux is defined as TRUE or FALSE. 
#' The results are described as changes in real units or in percent and als as slopes in real units per year or in percent per year.
#' They are computed over pairs of time points (Year1 to Year2).  These time points can be user-defined or
#' they can be set by the program to be the final year of the record and a set of years that are multiple of 5 years prior to that.
#'
#' @param localAnnualResults string specifying the name of the data frame that contains the concentration and discharge data, default name is AnnualResults
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param fluxUnit object of fluxUnit class. \code{\link{fluxConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param yearPoints numeric vector listing the years for which the change or slope computations are made, they need to be in chronological order.  For example yearPoints=c(1975,1985,1995,2005), default is NA (which allows the program to set yearPoints automatically)
#' @param flux logical if TRUE results are returned in flux, if FALSE concentration. Default is set to FALSE.
#' @param returnDataFrame logical, if a dataframe is required to be returned set this to TRUE.  Otherwise, the default is FALSE
#' @keywords water-quality statistics
#' @export
#' @return dataframe with Year1, Year2, change[mg/L], slope[mg/L], change[percent], slope[percent] columns. The data in each row is the change or slope calculated from Year1 to Year2
#' @examples
#' AnnualResults <- exAnnualResults
#' INFO <- exINFOEnd
#' tableChangeSingle(fluxUnit=6,yearPoints=c(2001,2005,2008,2009), flux=FALSE)  #This returns concentration ASCII table in the console 
#' tableChangeSingle(fluxUnit=6,yearPoints=c(2001,2005,2008,2009), flux=TRUE)  #This returns flux values ASCII table in the console
#' tableChangeConc <-tableChangeSingle(fluxUnit=9, returnDataFrame = TRUE, flux=FALSE)    #This returns concentration values in a dataframe
#' tableChangeFlux <-tableChangeSingle(fluxUnit=9, returnDataFrame = TRUE, flux=TRUE)  #This returns flux values in a dataframe
tableChangeSingle<-function(localAnnualResults = AnnualResults, localINFO = INFO, fluxUnit = 9, yearPoints = NA, returnDataFrame = FALSE, flux = FALSE) {
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
  
  fNameNoSpace <- gsub(" ","", fName)
  
  if (flux) header1 <- paste(header2, "\n              ",fName,fName,"/yr      %         %/yr", sep="")
  
  blankHolder<-"      ---"
  results<-rep(NA,4)
  indexPoints<-yearPoints-firstYear+1
  numPointsMinusOne<-numPoints-1
  write(header1,file="")
  
  if (flux){
    header <- c("Year1", "Year2", paste("change [", fNameNoSpace, "]", sep=""), paste("slope [", fNameNoSpace, "]", sep=""),"change[%]", "slope [%/yr]" )
  } else {
    header <- c("Year1", "Year2", "change [mg/L]","slope [mg/L/yr]","change[%]", "slope [%/yr]")    
  }
  
  resultDF <- as.data.frame(sapply(1:6, function(x) data.frame(x)))
  colnames(resultDF) <- header  
  
  for(iFirst in 1:numPointsMinusOne) {
    xFirst<-indexPoints[iFirst]
    yFirst<-localAnnualResults$FNConc[indexPoints[iFirst]]
    iFirstPlusOne<-iFirst+1
    for(iLast in iFirstPlusOne:numPoints) {
      xLast<-indexPoints[iLast]
      
      if (flux) {
        yLast<-localAnnualResults$FNFlux[indexPoints[iLast]]
        widthLength <- 12
      } else {
        yLast<-localAnnualResults$FNConc[indexPoints[iLast]]
        widthLength <- 9
      }      
      
      xDif<-xLast - xFirst
      yDif<-yLast - yFirst
      
      
      results[1]<-if(is.na(yDif)) blankHolder else format(yDif,digits=2,width=widthLength)
      results[2]<-if(is.na(yDif)) blankHolder else format(yDif/xDif,digits=2,width=widthLength)
      results[3]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst,digits=2,width=widthLength)
      results[4]<-if(is.na(yDif)) blankHolder else format(100*yDif/yFirst/xDif,digits=2,width=widthLength)
      cat("\n",yearPoints[iFirst]," to ",yearPoints[iLast],results)
      resultDF <- rbind(resultDF, c(yearPoints[iFirst], yearPoints[iLast],results))
    }
  }
  cat("\n")
  resultDF <- resultDF[-1,]
  row.names(resultDF) <- NULL
  resultDF <- as.data.frame(lapply(resultDF,as.numeric))
  colnames(resultDF) <- header
  
  if (!returnDataFrame) {
    return()
  }
  
  return(resultDF)
}

#
#
#
#
#' Color contour plot of the estimated surfaces as a function of discharge and time (surfaces include log concentration, standard error, and concentration)
#'
#' These plots are normally used for plotting the estimated concentration surface (whatSurface=3) but can be used to explore the 
#' estimated surfaces for the log of concentration or for the standard error (in log space) which is what determines the bias correction. 
#' The plots are often more interpretable when the time limits are only about 4 years apart.
#' To explore changes over a long time period it is best to do this multiple times, for various time slices of 4 years (for example) or to use the function plotDiffContours.
#'
#' @param yearStart numeric value for the starting date for the graph, expressed as decimal year (typically whole number such as 1989.0)
#' @param yearEnd numeric value for the ending date for the graph, expressed as decimal year, (for example 1993.0)
#' @param qBottom numeric value for the bottom edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qTop numeric value for the top edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param whatSurface numeric value, can only accept 1, 2, or 3;  whatSurface=1 is yHat (log concentration), whatSurface=2 is SE (standard error of log concentration), and whatSurface=3 is ConcHat (unbiased estimate of concentration), default = 3
#' @param localsurfaces string specifying the name of the matrix that contains the estimated surfaces, default is surfaces
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param localDaily string specifying the name of the data frame that contains the daily data, default name is Daily
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param contourLevels numeric vector containing the contour levels for the contour plot, arranged in ascending order, default is NA (which causes the contour levels to be set automatically, based on the data)
#' @param span numeric, it is the half-width (in days) of the smoothing window for computing the flow duration information, default = 60
#' @param pval numeric, the probability value for the lower flow frequency line on the graph
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param vert1 numeric, the location in time for a black vertical line on the figure, yearStart<vert1<yearEnd, default is NA (vertical line is not drawn) 
#' @param vert2 numeric, the location in time for a black vertical line on the figure, yearStart<vert2<yearEnd, default is NA (vertical line is not drawn)
#' @param horiz numeric, the location in discharge for a black horizontal line on the figure, qBottom<vert1<qTop, default is NA (no horizontal line is drawn)
#' @param flowDuration logical variable if TRUE plot the flow duration lines, if FALSE do not plot them, default = TRUE
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' yearStart<-2001
#' yearEnd<-2010
#' qBottom<-0.2
#' qTop<-20
#' clevel<-seq(0,2,0.5)
#' plotContours(yearStart,yearEnd,qBottom,qTop, localsurfaces = exsurfaces, localINFO = exINFOEnd, localDaily = exDailyEnd, contourLevels = clevel)  
plotContours<-function(yearStart, yearEnd, qBottom, qTop, whatSurface = 3, localsurfaces = surfaces, localINFO = INFO, localDaily = Daily, qUnit = 2, contourLevels = NA, span = 60, pval = 0.05, printTitle = TRUE, vert1 = NA, vert2 = NA, horiz = NA, flowDuration = TRUE) {
#  This funtion makes a contour plot 
#  x-axis is bounded by yearStart and yearEnd
#  y-axis is bounded by qBottom and qTop (in whatever discharge units are specified by qUnit)
#  whatSurface specifies which of three surfaces to plot
#     1 is the yHat surface (log concentration)
#     2 is the SE surface (standard error of log concentration)
#     3 is the ConcHat (unbiased estimate of Concentration)
#  surf is the name of the data frame that contains the three surfaces, typically it is surfaces
#  info is the name of the data frame that contains the indexing parameters for the surfaces, typically it is INFO
#  qUnit is the units of discharge to be used in the graphic: 1 is cfs, 2 is cms, 3 is 10^3 cfs, and 4 is 10^3 cms
#  contourLevels the default is NA (which lets the program set up the contour intervals)
#    if you want to set the contourLevels then you need to define a vector containing the limits of the contour intervals
#    for example, to have 4 levels we could say > contourLevels<-c(0,0.5,1,1.5,2)  or > contourLevels<-seq(0,2,0.5)
#    note that the length of contourLevels is the number of intervals plus 1
#  span is a smoothing parameter for the flow duration information, if it is too jagged you can increase it to smooth it out
#      note that it doesn't influence any calculations, just the appearance of the figure
#  pval is the lower flow frequency for the flow duration information
#     pval = 0.05 means that the lines will be at the 5 and 95%tiles of the flow distribution
#       other options could be pval=0.01 (for 1 and 99) or pval=10 (for 10 and 90)
#     pval must be greater than zero and less than 0.5
#   printTitle is a logical variable to indicate if a title is desired
#   vert1 defines the location of a vertical black line that represents a given date
#         this is often useful in explaining the meaning of the contour plot
#       vert1 must be between yearStart and yearEnd and it is expressed as a decimal year, 
#       for example 1July1998 would be vert1=1998.5
#       the default is no vertical line
#   vert2 is the location of a second vertical line (used so that two points in time can be compared)
#   horiz is the location of a horizontal line at some specified discharge (helpful in discussing changes over time)
#        it must be between qBottom and qTop and is defined in the units specified by qUnit
#        the default is no vertical line
################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
	par(oma=c(6,1,6,0))
	par(mar=c(5,5,4,2)+0.1)
	surfaceName<-c("log of Concentration","Standard Error of log(C)","Concentration")
	j<-3
	j<-if(whatSurface==1) 1 else j
	j<-if(whatSurface==2) 2 else j
	surf<-localsurfaces
	surfaceMin<-min(surf[,,j])
	surfaceMax<-max(surf[,,j])
	surfaceSpan<-c(surfaceMin,surfaceMax)
	contourLevels<-if(is.na(contourLevels[1])) pretty(surfaceSpan,n=5) else contourLevels
# computing the indexing of the surface, the whole thing, not just the part being plotted
	bottomLogQ<-localINFO$bottomLogQ
	stepLogQ<-localINFO$stepLogQ
	nVectorLogQ<-localINFO$nVectorLogQ
	bottomYear<-localINFO$bottomYear
	stepYear<-localINFO$stepYear
	nVectorYear<-localINFO$nVectorYear
	x<-((1:nVectorYear)*stepYear) + (bottomYear - stepYear)
	y<-((1:nVectorLogQ)*stepLogQ) + (bottomLogQ - stepLogQ)
	yLQ<-y
	qFactor<-qUnit@qUnitFactor
	y<-exp(y)*qFactor
	numX<-length(x)
	numY<-length(y)
	qBottom<-max(0.9*y[1],qBottom)
	qTop<-min(1.1*y[numY],qTop)
	xSpan<-c(yearStart,yearEnd)
	xTicks<-pretty(xSpan,n=5)
	yTicks<-logPretty3(qBottom,qTop)
	nYTicks<-length(yTicks)
	surfj<-surf[,,j]
	surft<-t(surfj)
# the next section does the flow duration information, using the whole period of record in Daily, not just the graph period
		plotTitle<-if(printTitle) paste(localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color") else ""
	if(flowDuration) {
	numDays<-length(localDaily$Day)
	freq<-rep(0,nVectorLogQ)
	durSurf<-rep(0,nVectorYear*nVectorLogQ)
	dim(durSurf)<-c(nVectorYear,nVectorLogQ)
	centerDays<-seq(1,365,22.9)
	centerDays<-floor(centerDays)
	for (ix in 1:16) {
		startDay<-centerDays[ix]-span
		endDay<-centerDays[ix]+span
		goodDays<-seq(startDay,endDay,1)
		goodDays<-ifelse(goodDays>0,goodDays,goodDays+365)
		goodDays<-ifelse(goodDays<366,goodDays,goodDays-365)
		numDays<-length(localDaily$Day)
		isGood<-rep(FALSE,numDays)
		for(i in 1:numDays) {
			count<-ifelse(localDaily$Day[i]==goodDays,1,0)
			isGood[i]<-if(sum(count)>0) TRUE else FALSE
		}
		spanDaily<-data.frame(localDaily,isGood)
		spanDaily<-subset(spanDaily,isGood)
		n<-length(spanDaily$Day)
		LogQ<-spanDaily$LogQ
		for(jQ in 1:nVectorLogQ) {
			ind<-ifelse(LogQ<yLQ[jQ],1,0)
			freq[jQ]<-sum(ind)/n
		}
		xInd<-seq(ix,numX,16)
		numXind<-length(xInd)
		for(ii in 1:numXind) {
			iX<-xInd[ii]
			durSurf[iX,]<-freq
		}
	}
	plevels<-c(pval,1-pval)
	pct1<-format(plevels[1]*100,digits=2)
	pct2<-format(plevels[2]*100,digits=2)
	plotTitle<-if(printTitle) paste(localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color\nBlack lines are",pct1,"and",pct2,"flow percentiles") else ""
	}
# setting up for the possible 3 straight lines to go on the graph
# if the lines aren't being plotted they are just located outside the plot area
	vectorNone<-c(yearStart,log(yTicks[1],10)-1,yearEnd,log(yTicks[1],10)-1)
	v1<-if(is.na(vert1)) vectorNone else c(vert1,log(yTicks[1],10),vert1,log(yTicks[nYTicks],10))
	v2<-if(is.na(vert2)) vectorNone else c(vert2,log(yTicks[1],10),vert2,log(yTicks[nYTicks],10))
	h1<-if(is.na(horiz)) vectorNone else c(yearStart,log(horiz,10),yearEnd,log(horiz,10))
	
	yLab<-qUnit@qUnitExpress
	filled.contour(x,log(y,10),surft,levels=contourLevels,xlim=c(yearStart,yearEnd),ylim=c(log(yTicks[1],10),log(yTicks[nYTicks],10)),main=plotTitle,xlab="",ylab=yLab,xaxs="i",yaxs="i",cex.main=0.95,
	plot.axes={
		axis(1,tcl=0.5,at=xTicks,labels=xTicks)
		axis(2,tcl=0.5,las=1,at=log(yTicks,10),labels=yTicks)
		axis(3, tcl = 0.5, at = xTicks, labels =FALSE)
        axis(4, tcl = 0.5, at = log(yTicks, 10), labels=FALSE)
        if(flowDuration) contour(x,log(y,10),durSurf,add=TRUE,drawlabels=FALSE,levels=plevels)
        segments(v1[1],v1[2],v1[3],v1[4])
        segments(v2[1],v2[2],v2[3],v2[4])
        segments(h1[1],h1[2],h1[3],h1[4])
    })
	par(oma=c(0,0,0,0))
	par(mar=c(5,4,4,2)+0.1)
}
#
#
#
#' Plot up to three curves representing the concentration versus discharge relationship. Each curve is a different point in time.  
#'
#' These plots are like a vertical slice of the estimated concentration surface that is seen in the plotContours function.  
#' These plots show how the concentration-discharge relationship is changing over time. 
#' Typically the time points selected would be in three years at the same time of year spaced out over the period of record.  But that is not necessary.  
#' Another possibility is to use this to explore seasonal differences.  In this case the three
#' dates would be in the same year but different times during the year.
#'
#' @param date1 string specifying the date for the first curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes) 
#' @param date2 string specifying the date for the second curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If only one curve is wanted this should be NA
#' @param date3 string specifying the date for the third curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If a third curve is not wanted this should be NA
#' @param qLow numeric value for the lowest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qHigh numeric value for the highest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend, in the units shown on x-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param legendTop numeric which represents the top edge of the legend, in the units shown on y-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are also printed to the console (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param localSample string specifying the name of the data frame that contains the Sample data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @keywords water-quality statistics graphics
#' @import survival
#' @export
#' @examples 
#' date1<-"2001-06-01"
#' date2<-"2010-06-01"
#' date3<-NA
#' qLow<-1
#' qHigh<-1000
#' plotConcQSmooth(date1,date2,date3,qLow,qHigh, localSample=exSampleEnd, localINFO = exINFOEnd)
plotConcQSmooth<-function(date1,date2,date3,qLow,qHigh,qUnit = 2, legendLeft = 0,legendTop = 0, concMax = NA,bw = FALSE, printTitle = TRUE, printValues = FALSE, localSample = Sample, localINFO = INFO, windowY = 10, windowQ = 2, windowS = 0.5) {
#########################################################
       if (is.numeric(qUnit)) {
        qUnit <- qConst[shortCode = qUnit][[1]]
    }
    else if (is.character(qUnit)) {
        qUnit <- qConst[qUnit][[1]]
    }
#############################################################
        numDates<-3
        numDates<-if(is.na(date2)) 1 else 3
        numDates<-if(is.na(date3)) 2 else 3
        dates<-rep(as.POSIXlt(date1),3)
        dates[1]<-as.POSIXlt(date1)
        dates[2]<-as.POSIXlt(date2)
        dates[3]<-as.POSIXlt(date3)
        LogQLow<-log(qLow)
        LogQHigh<-log(qHigh)
        step<-(LogQHigh-LogQLow)/47
        x<-exp(seq(LogQLow,LogQHigh,step))
        qFactor<-qUnit@qUnitFactor
        LQ<-log(x/qFactor)
# note the vector x is the set of 48 discharge values used to construct the curve, expressed in the selected units (such as cfs or 1000 cfs)
# and the vector LQ is the same set of 48 discharge values but expressed in units of natural log of cubic meters per second
        y<-rep(NA,3*48)
        dim(y)<-c(3,48)
        day<-dates$yday + 1
        year<-dates$year + 1900
        decYear<-year+((day-0.5)/366)
        for(iCurve in 1:numDates) {
        	yrs<-rep(decYear[iCurve],48)
        	result<-runSurvReg(localSample,yrs,LQ,windowY = windowY, windowQ = windowQ, windowS = windowS)
        	y[iCurve,]<-result[,3]
                }
       title<-if(printTitle) paste (localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated Concentration Versus Discharge Relationship\nat",numDates,"specific dates") else ""
       xLab=qUnit@qUnitExpress
       yLab="Concentration in mg/L"
       xTicks<-logPretty3(qLow,qHigh)
       numXTicks<-length(xTicks)
       xLeft<-xTicks[1]
       xRight<-xTicks[numXTicks]
       yMax<-max(y,na.rm=TRUE)
       yTop<-if(is.na(concMax)) yMax else concMax
       yTicks<-yPretty(yTop)
       numYTicks<-length(yTicks)
       yTop<-yTicks[numYTicks]
       colorVal<-if(bw) c("black","black","black") else c("black","red","green")
       lineVal<-if(bw) c(1,2,3) else c(1,1,1)
       plot(log(x,10),y[1,],axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab=xLab,ylim=c(0,yTop),yaxs="i",ylab=yLab,main=title,type="l",lwd=2,col=colorVal[1],lty=lineVal[1],cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
       axis(1, tcl = 0.5, at = log(xTicks, 10), labels = xTicks)
       axis(2, tcl = 0.5, las = 1, at = yTicks, labels = yTicks)
       axis(3, tcl = 0.5, at = log(xTicks, 10), labels = FALSE)
       axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
       box()
		par(new=TRUE)
		plot(log(x,10),y[2,],axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="",main="",type="l",lwd=2,col=colorVal[2],lty=lineVal[2],cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
		par(new=TRUE)
		plot(log(x,10),y[3,],axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab="",ylim=c(0,yTop),yaxs="i",ylab="",main="",type="l",lwd=2,col=colorVal[3],lty=lineVal[3],cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
		legendLeft<-if(legendLeft==0) qLow*2 else legendLeft
		legendLeft<-log(legendLeft,10)
		legendTop<-if(legendTop==0) 0.3*yTop else legendTop 
		words<-as.character(dates[1:numDates])
		ltys<-lineVal[1:numDates]
		cols<-colorVal[1:numDates]
		legend(legendLeft,legendTop,legend=words,lty=ltys,col=cols,lwd=2,cex=1.5)
		printResults<-rep(NA,48*4)
		dim(printResults)<-c(48,4)
		for(j in 1:48) {printResults[j,1]<-format(x[j],width=9)
			printResults[j,2:4]<-format(y[1:3,j],width=10)}
			topLine<-c("discharge",as.character(dates[1:numDates]))
		if(printValues) write(topLine,file="",ncolumns=4)
		if(printValues) write.table(printResults,file="",quote=FALSE,row.names=FALSE,col.names=FALSE)      
}
#
#' plot up to three curves representing the log concentration versus discharge relationship, each curve is a different point in time
#'
#' These plots are like a vertical slice of the estimated concentration surface that is seen in the plotContours function.  
#' These plots show how the concentration-discharge relationship is changing over time. 
#' Typically the time points selected would be in three years at the same time of year spaced out over the period of record.  But that is not necessary.  
#' Another possibility is to use this to explore seasonal differences.  In this case the three
#' dates would be in the same year but different times during the year.
#'
#' @param date1 string specifying the date for the first curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes) 
#' @param date2 string specifying the date for the second curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If only one curve is wanted this should be NA
#' @param date3 string specifying the date for the third curve on the graph, it is in the form "yyyy-mm-dd" (must be in quotes).  If a third curve is not wanted this should be NA
#' @param qLow numeric value for the lowest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qHigh numeric value for the highest discharge to be considered, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param legendLeft numeric which represents the left edge of the legend, in the units shown on x-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param legendTop numeric which represents the top edge of the legend, in the units shown on y-axis of graph, default is 0, will be placed within the graph but may overprint data
#' @param concMax numeric value for upper limit on concentration shown on the graph, default = NA (which causes the upper limit to be set automatically, based on the data)
#' @param concMin numeric value for lower limit on concentration shown on the graph, default = NA (which causes the lower limit to be set automatically, based on the data)
#' @param bw logical if TRUE graph is produced in black and white, default is FALSE (which means it will use color)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param printValues logical variable if TRUE the results shown on the graph are also printed to the console (this can be useful for quantifying the changes seen visually in the graph), default is FALSE (not printed)
#' @param localSample string specifying the name of the data frame that contains the Sample data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @keywords water-quality statistics graphics
#' @import survival
#' @export
#' @examples 
#' date1<-"2001-06-01"
#' date2<-"2009-06-01"
#' date3<-NA
#' qLow<-1
#' qHigh<-1000
#' plotLogConcQSmooth(date1,date2,date3,qLow,qHigh, localSample=exSampleEnd, localINFO = exINFOEnd)
plotLogConcQSmooth<-function(date1,date2,date3,qLow,qHigh,qUnit = 2, legendLeft = 0,legendTop = 0, concMax = NA, concMin = NA, bw = FALSE, printTitle = TRUE, printValues = FALSE, localSample = Sample, localINFO = INFO, windowY = 10, windowQ = 2, windowS = 0.5) {
##################################################
       if (is.numeric(qUnit)) {
        qUnit <- qConst[shortCode = qUnit][[1]]
    }
    else if (is.character(qUnit)) {
        qUnit <- qConst[qUnit][[1]]
    }
################################################# 
        numDates<-3
        numDates<-if(is.na(date2)) 1 else 3
        numDates<-if(is.na(date3)) 2 else 3
        dates<-rep(as.POSIXlt(date1),3)
        dates[1]<-as.POSIXlt(date1)
        dates[2]<-as.POSIXlt(date2)
        dates[3]<-as.POSIXlt(date3)
        LogQLow<-log(qLow)
        LogQHigh<-log(qHigh)
        step<-(LogQHigh-LogQLow)/47
        x<-exp(seq(LogQLow,LogQHigh,step))
        qFactor<-qUnit@qUnitFactor
        LQ<-log(x/qFactor)
# note the vector x is the set of 48 discharge values used to construct the curve, expressed in the selected units (such as cfs or 1000 cfs)
# and the vector LQ is the same set of 48 discharge values but expressed in units of natural log of cubic meters per second
        y<-rep(NA,3*48)
        dim(y)<-c(3,48)
        day<-dates$yday + 1
        year<-dates$year + 1900
        decYear<-year+((day-0.5)/366)
        for(iCurve in 1:numDates) {
        	yrs<-rep(decYear[iCurve],48)
        	result<-runSurvReg(localSample,yrs,LQ,windowY = windowY, windowQ = windowQ, windowS = windowS)
        	y[iCurve,]<-result[,3]
                }
       title<-if(printTitle) paste (localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated Concentration Versus Discharge Relationship\nat",numDates,"specific dates") else ""
       xLab=qUnit@qUnitExpress
       yLab="Concentration in mg/L"
       xTicks<-logPretty3(qLow,qHigh)
       numXTicks<-length(xTicks)
       xLeft<-xTicks[1]
       xRight<-xTicks[numXTicks]
       yMax<-max(y,na.rm=TRUE)
       yTop<-if(is.na(concMax)) yMax else concMax
       yMin<-min(y,na.rm=TRUE)
       yBottom<-if(is.na(concMin)) yMin else concMin
       yTicks<-logPretty3(yBottom,yTop)
       numYTicks<-length(yTicks)
       yTop<-yTicks[numYTicks]
       colorVal<-if(bw) c("black","black","black") else c("black","red","green")
       lineVal<-if(bw) c(1,2,3) else c(1,1,1)
       plot(log(x,10),log(y[1,],10),axes=FALSE, xlim=c(log(xLeft,10),log(xRight,10)), xaxs="i",xlab=xLab,ylim=c(log(yBottom,10),log(yTop,10)), yaxs="i",ylab=yLab,main=title,type="l",lwd=2,col=colorVal[1],lty=lineVal[1], cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
       axis(1, tcl = 0.5, at = log(xTicks, 10), labels = xTicks)
       axis(2, tcl = 0.5, las = 1, at = log(yTicks,10), labels = yTicks)
       axis(3, tcl = 0.5, at = log(xTicks, 10), labels = FALSE)
       axis(4, tcl = 0.5, at = log(yTicks,10), labels = FALSE)
       box()
		par(new=TRUE)
		plot(log(x,10),log(y[2,],10),axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab="",ylim=c(log(yBottom,10),log(yTop,10)), yaxs="i",ylab="",main="",type="l",lwd=2,col=colorVal[2],lty=lineVal[2],cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
		par(new=TRUE)
		plot(log(x,10),log(y[3,],10),axes=FALSE,xlim=c(log(xLeft,10),log(xRight,10)),xaxs="i",xlab="", ylim=c(log(yBottom,10), log(yTop,10)) ,yaxs="i",ylab="",main="",type="l",lwd=2,col=colorVal[3],lty=lineVal[3],cex=0.7,cex.main=1.1,font.main=2,cex.lab=1.2)
		legendLeft<-if(legendLeft==0) qLow*2 else legendLeft
		legendLeft<-log(legendLeft,10)
		legendTop<-if(legendTop==0) 0.3*yTop else legendTop
		legendTop<-log(legendTop,10) 
		words<-as.character(dates[1:numDates])
		ltys<-lineVal[1:numDates]
		cols<-colorVal[1:numDates]
		legend(legendLeft,legendTop,legend=words,lty=ltys,col=cols,lwd=2,cex=1.5)
		printResults<-rep(NA,48*4)
		dim(printResults)<-c(48,4)
		for(j in 1:48) {printResults[j,1]<-format(x[j],width=9)
			printResults[j,2:4]<-format(y[1:3,j],width=10)}
			topLine<-c("discharge",as.character(dates[1:numDates]))
		if(printValues) write(topLine,file="",ncolumns=4)
		if(printValues) write.table(printResults,file="",quote=FALSE,row.names=FALSE,col.names=FALSE) 
	}
#
#' Jack-Knife cross validation of the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This function fits the WRTDS model n times (where n is the number of observations).  
#' For each fit, the data value being estimated is eliminated from the record. 
#' This gives predictions that do not depend on knowing the actual result for that day. 
#' Thus it provides for a more "honest" estimate of model performance than a traditional 
#' error analysis that uses all the data. 
#'
#' @param SampleCrossV string specifying the name of the data frame containing the sample values, default is Sample
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @keywords water-quality statistics
#' @import survival
#' @return SampleCrossV data frame containing the sample data augmented by the results of the cross-validation exercise
#' @export
#' @examples
#' estCrossVal(SampleCrossV = exSampleStart)
estCrossVal<-function(SampleCrossV = Sample, windowY = 10, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen = 50){
  #  this function fits the WRTDS model making an estimate of concentration for every day
  #    But, it uses leave-one-out-cross-validation
  #    That is, for the day it is estimating, it leaves that observation out of the data set
  #      It returns a Sample data frame with three added columns
  #      yHat, SE, and ConcHat
  numObs<-length(SampleCrossV$DecYear)
  yHat<-rep(0,numObs)
  SE<-rep(0,numObs)
  ConcHat<-rep(0,numObs)
  iCounter<-seq(1,numObs)
  cat("\n estCrossVal prints out the sample numbers to indicate progress\n")
  SampleCV<-data.frame(SampleCrossV,iCounter,yHat,SE,ConcHat)
  for(i in 1:numObs) {
  	cat(" ",i)
  	SampleCV$keep<-ifelse(SampleCV$iCounter==i,FALSE,TRUE)
  	SampleMinusOne<-subset(SampleCV,keep)
  	result<-runSurvReg(SampleMinusOne,SampleCrossV$DecYear[i],SampleCrossV$LogQ[i],windowY,windowQ,windowS,minNumObs,minNumUncen)
  	SampleCrossV$yHat[i]<-result[1]
  	SampleCrossV$SE[i]<-result[2]
  	SampleCrossV$ConcHat[i]<-result[3]
  }	
  return(SampleCrossV)
}
#
#
#' Estimate the three surfaces (for yHat, SE and ConcHat) as a function of DecYear and logQ and store in the three-dimensional object called surfaces
#'
#' This function uses weighted survival regression to estimate three surfaces that cover the complete range
#' of DecYear and log(Q) values in the Daily data set. 
#' These surfaces are:
#'   (1) is the estimated log concentration (yHat), 
#'   (2) is the estimated standard error (SE), 
#'   (3) is the estimated concentration (ConcHat). 
#' They are mapped as an array that covers the complete space of daily discharge and time. 
#' The first index is discharge, layed out in 14 equally spaced levels of log(Q).
#' The second index is time, layed out as 16 increments of the calendar year, starting January 1.
#'  It returns the 3 dimensional array called surfaces.
#'  This array will be used to estimate these 3 quantities for any given day in the daily values record. 
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localSample string specifying the name of the data frame containing the sample values, default is Sample
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @keywords water-quality statistics
#' @import survival
#' @return surfaces array containing the three surfaces estimated, array is 3 dimensional
#' @export
#' @examples
#' estSurfaces(localDaily = exDailyStart, localSample = exSampleStart)
estSurfaces<-function(localDaily = Daily, localSample = Sample, windowY=10,windowQ=2,windowS=0.5,minNumObs=100,minNumUncen=50){
# this function estimates the 3 surfaces based on the Sample data
# one is the estimated log concentration (yHat)
# the second is the estimated standard error (SE)
# the third is the estimated concentration (ConcHat)
# they are mapped as an array that covers the complete space of daily discharge and time
# the first index is discharge, layed out in 14 equally spaced levels of log(Q)
# the second index is time, layed out as 16 increments of the calendar year, starting January 1.
# it returns the data frame called surfaces 
#
	bottomLogQ<-min(localDaily$LogQ) - 0.05
	topLogQ<-max(localDaily$LogQ) + 0.05
	stepLogQ<-(topLogQ-bottomLogQ)/13
	vectorLogQ<-seq(bottomLogQ,topLogQ,stepLogQ)
	stepYear<-1/16
	bottomYear<-floor(min(localDaily$DecYear))
	topYear<-ceiling(max(localDaily$DecYear))
	vectorYear<-seq(bottomYear,topYear,stepYear)
	nVectorYear<-length(vectorYear)
	estPtLogQ<-rep(vectorLogQ,nVectorYear)
	estPtYear<-rep(vectorYear,each=14)
	resultSurvReg<-runSurvReg(localSample,estPtYear,estPtLogQ,windowY,windowQ,windowS,minNumObs,minNumUncen)
	surfaces<-array(0,dim=c(14,nVectorYear,3))
	for(iQ in 1:14) {
    for(iY in 1:nVectorYear){ 
			k<-(iY-1)*14+iQ
			surfaces[iQ,iY,]<-resultSurvReg[k,]
    }
  }
	return(surfaces)
}
#
#
#'  Compute the 6 parameters needed to lay out the grid for the surfaces computed in estSurfaces
#'
#'     The code here is a repetition of the first part of the code for estSurfaces
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return surfaceIndexParameters a numeric vector of length 6, defining the grid for the surfaces
#' @export
#' @examples
#' surfaceIndex(localDaily = exDailyStart)
surfaceIndex<-function(localDaily = Daily){
# this function contains the same code that comes at the start of
# estSurfaces, it just computes the parameters of the grid 
# used for the surfaces so that they can be stored for future use
# the first index is discharge, layed out in 14 equally spaced levels of log(Q)
# the second index is time, layed out as 16 increments of the calendar year, starting January 1.
#  Note: I don't think this is the smartest way to do this, but I'm not sure what to do here
#  I don't like trying to have the same code twice
#
	bottomLogQ<-min(localDaily$LogQ) - 0.05
	topLogQ<-max(localDaily$LogQ) + 0.05
	stepLogQ<-(topLogQ-bottomLogQ)/13
	vectorLogQ<-seq(bottomLogQ,topLogQ,stepLogQ)
	stepYear<-1/16
	bottomYear<-floor(min(localDaily$DecYear))
	topYear<-ceiling(max(localDaily$DecYear))
	vectorYear<-seq(bottomYear,topYear,stepYear)
	nVectorYear<-length(vectorYear)
	surfaceIndexParameters<-c(bottomLogQ,stepLogQ,14,bottomYear,stepYear,nVectorYear)
	return(surfaceIndexParameters)
}			
#
#
#' Run the weighted survival regression for a set of estimation points (defined by DecYear and Log(Q))
#'
#'   This function runs the survival regression which is the concentration estimation method of WRTDS. 
#'    It uses sample data from the data frame Sample. 
#'    It does the estimation for a set of data points defined by two vectors: estPtYear and estPtLQ. 
#'    It returns an array of results for the estimation points.  
#'    The array returned contains yHat, SE and ConcHat (in that order). 
#'
#' @param localSample string specifying the name of the data frame containing the sample values, default is Sample
#' @param estPtYear numeric vector of Decimal Year values at the estimation points
#' @param estPtLQ numeric vector of ln(Q) values at the estimation points, must be the same length as estPtYear 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @keywords water-quality statistics
#' @import survival
#' @return resultSurvReg numeric array containing the yHat, SE, and ConcHat values array dimensions are (numEstPts,3)
#' @export
#' @examples
#' estPtYear<-c(2001.0,2005.0,2009.0)
#' estPtLQ<-c(5,5,5)
#' runSurvReg(localSample=exSampleStart,estPtYear,estPtLQ)
runSurvReg<-function(localSample = Sample,estPtYear,estPtLQ,windowY=10,windowQ=2,windowS=0.5,minNumObs=100,minNumUncen=50) {
# runs survival regression model
# Sample is the Sample data frame being used
# estPtYear is a vector of DecYear values where the model will be estimated
# estPtLQ is a vector of LogQ values where the model will be estimated (must be same as estPtYear length)
# windows are the half window widths for DecYear, LogQ and, Season (respectively)
# minNumObs is the minimum number of observations that must have non-zero weights
#        if the survival regression is going to be run
# minNumUncen is the minimum number of uncensored observations that must have non-zero weights
#        if the survival regression is going to be run
#   function returns of dimensions (3 by the number of estimation points)
#        first column is predicted concentration in log space (called yHat)
#        second column is the standard error (which is used to compute the bias correction)
#        third column is the predicted concentration in real space (called ConcHat)
  library(survival)
  numEstPt<-length(estPtYear)
  resultSurvReg<-array(0,c(numEstPt,3))
  for (i in 1:numEstPt) {
# This loop takes us through all the estimation points
# We always reset the window widths to their starting values, because they may
#   have been widened in the process
    tempWindowY<-windowY
    tempWindowQ<-windowQ
    tempWindowS<-windowS
    estY<-estPtYear[i]
    estLQ<-estPtLQ[i]
    repeat{
#  We subset the sample frame by time, to narrow the set of data to run through in the following steps
    	Sam<-subset(localSample,abs(DecYear-estY)<=tempWindowY)
    	diffY<-abs(Sam$DecYear-estY)
    	weightY<-triCube(diffY,tempWindowY)
    	weightQ<-triCube(Sam$LogQ-estLQ,tempWindowQ)
    	diffUpper<-ceiling(diffY)
    	diffLower<-floor(diffY)
    	diffSeason<-pmin(abs(diffUpper-diffY),abs(diffY-diffLower))
    	weightS<-triCube(diffSeason,tempWindowS)
    	Sam$weight<-weightY*weightQ*weightS
    	Sam<-subset(Sam,weight>0)
    	numPosWt<-length(Sam$weight)
    	numUncen<-sum(Sam$Uncen)
    	tempWindowY<-tempWindowY*1.1
    	tempWindowQ<-tempWindowQ*1.1
    	tempWindowS<-min(tempWindowS*1.1,0.5)
    	if(numPosWt>=minNumObs&numUncen>=minNumUncen) break
    }
# now we are ready to run Survival Regression
    weight<-Sam$weight
    aveWeight<-sum(weight)/numPosWt
    weight<-weight/aveWeight
    survModel<-survreg(Surv(log(ConcLow),log(ConcHigh),type="interval2")~DecYear+LogQ+SinDY+CosDY,data=Sam,weights=weight,dist="gaus")
    new<-data.frame(DecYear=estY,LogQ=estLQ,SinDY=sin(2*pi*estY),CosDY=cos(2*pi*estY))
    #   extract results at estimation point
    yHat<-predict(survModel,new)
    SE<-survModel$scale
    bias<-exp((SE^2)/2)
    resultSurvReg[i,1]<-yHat
    resultSurvReg[i,2]<-SE
    resultSurvReg[i,3]<-bias*exp(yHat)
  }
  return(resultSurvReg)
}
#
#
#'   Tukey's Tricube weight function
#'
#'      Computes the tricube weight function on a vector of distance values (d),
#'      based on a half-window width of h,
#'      and returns a vector of weights that range from zero to 1.
#'
#' @param d numeric vector of distances from the point of estimation to the given sample value
#' @param h numeric value, the half-window width, measured in the same units as d
#' @keywords statistics weighting
#' @return w numeric vector of weights, all 0<=w<=1
#' @export
#' @examples
#'  h<-10
#'  d<-c(-11,-10,-5,-1,-0.01,0,5,9.9,10,20)
#'  triCube(d,h)
triCube<-function(d,h) {
#  triCube is Tukey tricubed weight function
#    first argument, d, is a vector of the distances between the observations and the estimation point
#    second argument, h, is the half window width
#    it returns a vector of weights (w) for the observations in the vector, d
	n<-length(d)
	zero<-rep(0,n)
	ad<-abs(d)
	w<-(1-(ad/h)^3)^3
	w<-pmax(zero,w)
	return(w)
}
#
#
#'    Calculates monthly values of Discharge, Concentration, Flux, Flow Normalized Concentration and Flow Normalized Flux for the entire record
#'
#'     Computes the monthly average values of these five quantities (Q, Conc, Flux, FNConc, and FNFlux).
#'     It also saves (for each month) the month sequence number (months starting with January, 1850) and the average value of DecYear.
#'     It returns a data frame containing MonthSeq and average values of DecYear, Q, Conc, Flux, FNConc, and FNFlux.
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return MonthlyResults data frame of numeric values describing the monthly average values
#' @export
#' @examples
#' calculateMonthlyResults(localDaily = exDailyEnd)
calculateMonthlyResults<-function(localDaily = Daily){
# this creates a data frame of monthly results from Daily
# it requires that there be at least 15 valid values in the month
# to compute those results
# it returns the data frame called MonthlyResults
#
	firstMonthSeq<-localDaily$MonthSeq[1]
	numDays<-length(localDaily$MonthSeq)
	lastMonthSeq<-localDaily$MonthSeq[numDays]
	numMonths<-lastMonthSeq-firstMonthSeq+1
	DecYear<-rep(NA,numMonths)
	Q<-rep(NA,numMonths)
	MonthSeq<-seq(firstMonthSeq,lastMonthSeq)
	Conc<-rep(NA,numMonths)
	Flux<-rep(NA,numMonths)
	FNConc<-rep(NA,numMonths)
	FNFlux<-rep(NA,numMonths)
	for(i in 1:numMonths) {
		thisMonthSeq<-MonthSeq[i]
		DailyM<-subset(localDaily,MonthSeq==thisMonthSeq)
		UseIt<-ifelse(is.na(DailyM$ConcDay),0,1)
		Keep<-if(sum(UseIt)>15) TRUE else FALSE
		Q[i]<-if(Keep) mean(DailyM$Q) else NA
		DecYear[i]<-if(Keep) mean(DailyM$DecYear) else NA
		Conc[i]<-if(Keep) mean(DailyM$ConcDay) else NA
		Flux[i]<-if(Keep) mean(DailyM$FluxDay) else NA
		FNConc[i]<-if(Keep) mean(DailyM$FNConc) else NA
		FNFlux[i]<-if(Keep) mean(DailyM$FNFlux) else NA
	}
	MonthlyResults<-data.frame(MonthSeq,DecYear,Q,Conc,Flux,FNConc,FNFlux)
	return(MonthlyResults)	
}
#
#
#'    Deletes the computed values during periods of time when there is no sample data
#'
#'  This function is used when the data analyst believes that a gap in the sample data record
#'  is so long that estimates during that period are not reliable. 
#'  This is only unsed for periods of several years in duration. 
#'  For this period, the values of Conc, Flux, FNConc and FNFlux are all converted to NA. 
#'
#' @param startBlank string specifying starting date of blank period, input in quotes in yyyy-mm-dd format
#' @param endBlank string specifying the ending date of blank period, input in quotes in yyyy-mm-dd format
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return localDaily data frame 
#' @export
#' @examples
#' startBlank = "2004-10-01"
#' endBlank = "2006-09-30"
#' blankTime(startBlank, endBlank, localDaily = exDailyEnd)
blankTime<-function(startBlank, endBlank, localDaily = Daily) {
# this function is used after the model estimation is done
# it can be used more than once, for multiple blank periods
# the startBlank and endBlank variables must be in quotes and in yyyy-mm-dd format
# it is a good idea for startBlank to be the first day of some month
# it is a good idea for endBlank to be the last day of some month
# it is also a good idea for these to cover entire water years
#
#  code needs to include error handling for inputs
	startBlank<-as.Date(startBlank)
	endBlank<-as.Date(endBlank)
	startJulian<-as.numeric(julian(startBlank,origin=as.Date("1850-01-01")))
	endJulian<-as.numeric(julian(endBlank,origin=as.Date("1850-01-01")))
	bad<-ifelse(localDaily$Julian>=startJulian&localDaily$Julian<=endJulian,TRUE,FALSE)
	localDaily$ConcDay<-ifelse(bad,NA,localDaily$ConcDay)
	localDaily$FluxDay<-ifelse(bad,NA,localDaily$FluxDay)
	localDaily$FNConc<-ifelse(bad,NA,localDaily$FNConc)
	localDaily$FNFlux<-ifelse(bad,NA,localDaily$FNFlux)
	return(localDaily)		
}
#
#
#'  Creates the AnnualResults data frame from the Daily data frame
#'
#'   This function aggregates the results stored on a daily basis in the Daily data frame
#'   and stores the average values of these in the new data frame called AnnualResults.
#'      The "annual values" can be a full 12 months, or they can be shorter. 
#'      See manual to understand paLong and paStart arguments. 
#'      The simplest case, a Water Year (October through September), would have
#'      paLong=12, and paStart=10. 
#'      A calendar year would be paLong=12 and paStart=1. 
#'      A winter season of Dec, Jan, Feb would be paLong=3 and paStart=12
#'
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12, default is 12
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12, default is 10 
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return AnnualResults data frame with one row per year
#' @export
#' @examples 
#' setupYears(localDaily = exDailyEnd)
setupYears<-function(paLong = 12, paStart = 10, localDaily = Daily){
# this function aggregates the results in the data frame Daily into annual values
# but it gives the user flexibility as to the period of analysis
# The "annual values" can be a full 12 months, or they can be shorter
# See manual to understand paLong and paStart arguments
#   But, the simplest case, a Water Year would have
#   paLong=12, and paStart=10
# it is designed to handle NA values
#
#	
	numDays<-length(localDaily$MonthSeq)
	firstMonthSeq<-localDaily$MonthSeq[1]
	lastMonthSeq<-localDaily$MonthSeq[numDays]
#   creating a data frame of starting and ending months for each year
	Starts<-seq(paStart,lastMonthSeq,12)
	Ends<-Starts+paLong-1
	StartEndSeq<-data.frame(Starts,Ends)
#   need to trim off the front and back, those years that aren't in the Daily data set
	StartEndSeq<-subset(StartEndSeq,Starts>=firstMonthSeq)
	StartEndSeq<-subset(StartEndSeq,Ends<=lastMonthSeq)
	numYears<-length(StartEndSeq$Starts)
	DecYear<-rep(NA,numYears)
	Q<-rep(NA,numYears)
	Conc<-rep(NA,numYears)
	Flux<-rep(NA,numYears)
	FNConc<-rep(NA,numYears)
	FNFlux<-rep(NA,numYears)
	for(i in 1:numYears) {
		startSeq<-StartEndSeq$Starts[i]
		endSeq<-StartEndSeq$Ends[i]
		DailyYear<-subset(localDaily,MonthSeq>=startSeq)
		DailyYear<-subset(DailyYear,MonthSeq<=endSeq)
#     need to see if the data frame for the year has enough good data
		counter<-ifelse(is.na(DailyYear$ConcDay),1,0)
#     if we have NA values on more than 10% of the days, then don't use the year
		good<-if((sum(counter)/length(DailyYear$ConcDay))>0.1) FALSE else TRUE
		DecYear[i]<-mean(DailyYear$DecYear)
		Q[i]<-mean(DailyYear$Q)
		Conc[i]<-if(good) mean(DailyYear$ConcDay,na.rm=TRUE) else NA
		Flux[i]<-if(good) mean(DailyYear$FluxDay,na.rm=TRUE) else NA
		FNConc[i]<-if(good) mean(DailyYear$FNConc,na.rm=TRUE) else NA
		FNFlux[i]<-if(good) mean(DailyYear$FNFlux,na.rm=TRUE) else NA
	}
#  create two more variables that just report paStart and paLong
#    needed later to verify the period of analysis used in the Annual Results summary
	PeriodStart<-rep(paStart,numYears)
	PeriodLong<-rep(paLong,numYears)
  AnnualResults<-data.frame(DecYear,Q,Conc,Flux,FNConc,FNFlux,PeriodLong,PeriodStart)
  return(AnnualResults)		
}

#
#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does a jack-knife cross-validation of a WRTDS model, fits the surface
#' (concentration as a function of discharge and time), 
#' estimates daily values of concentration and flux, and flow normalized values.  
#' It combines these results to compute monthly values. 
#' It returns several data frames or matrices (Daily, INFO, Sample, and surfaces).
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localSample string specifying the name of the data frame containing the sample values, default is Sample
#' @param localINFO string specifying the name of the data frame containing the metadata, default is INFO
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param env environment to set variables in
#' @keywords water-quality statistics
#' @import survival
#' @export
#' @examples
#' \dontrun{modelEstimation(localDaily = exDailyStart, localSample = exSampleStart, localINFO = exINFOStart)}
modelEstimation<-function(localDaily = Daily,localSample = Sample, localINFO = INFO, windowY=10, windowQ=2, windowS=0.5,minNumObs=100,minNumUncen=50, env=parent.frame()){
  # this code is a wrapper for several different functions that test the model, fit a surface,
  #  estimate daily values and flow normalized daily values
  #  and organize these into monthly results
  #  it returns several data frames
  #  all of the data frames are given their "standard" names
  #
  library(survival)
  cat("\n first step running estCrossVal may take about 1 minute")
  Sample1<-estCrossVal(SampleCrossV = localSample, windowY, windowQ, windowS, minNumObs, minNumUncen)
  cat("\n done with estCrossVal")
  surfaceIndexParameters<-surfaceIndex(localDaily = localDaily)
  localINFO$bottomLogQ<-surfaceIndexParameters[1]
  localINFO$stepLogQ<-surfaceIndexParameters[2]
  localINFO$nVectorLogQ<-surfaceIndexParameters[3]
  localINFO$bottomYear<-surfaceIndexParameters[4]
  localINFO$stepYear<-surfaceIndexParameters[5]
  localINFO$nVectorYear<-surfaceIndexParameters[6]
  localINFO$windowY<-windowY
  localINFO$windowQ<-windowQ
  localINFO$windowS<-windowS
  localINFO$minNumObs<-minNumObs
  localINFO$minNumUncen<-minNumUncen
  cat("\nNext step running  estSurfaces this can take about another minute")
  surfaces1<-estSurfaces(localDaily = localDaily, localSample = localSample, windowY, windowQ, windowS, minNumObs, minNumUncen)
  cat("\nDone with estSurfaces and starting estDailyFromSurface")
  cat("\nThis can take several minutes but you will see updates")
  Daily1<-estDailyFromSurfaces(localDaily = localDaily, localINFO = localINFO, localsurfaces = surfaces1)
#   cat("\nDone with estDailyFromSurfaces moving on to calculateMonthlyResults")
#   MonthlyResults1<-calculateMonthlyResults(localDaily = Daily1)
#   cat("\nDone with calculateMonthlyResults")
  env$Daily<-Daily1
  env$INFO<-localINFO
  env$Sample<-Sample1
  env$surfaces<-surfaces1
#  env$MonthlyResults<-MonthlyResults1
	cat("\nDone with modelEstimation now do AnnualResults<-setupYears()\nor if using a period of analysis other than Water Year specify the arguments paStart and paLong in call to setupYears ")
}
#
#
#' Estimates all daily values of Concentration, Flux, Flow Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these four time series
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augmented version of the Daily data frame, which is returned.
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localINFO string specifying the name of the data frame containing the meta-data, default is INFO
#' @param localsurfaces string specifying the name of the array containing the three surfaces, default is surfaces
#' @keywords water-quality statistics
#' @return localDaily string specifying the name of the data frame containing the daily values and these esimates
#' @export
#' @examples
#' \dontrun{estDailyFromSurfaces(localDaily = exDailyStart, localINFO = exINFOStart)}
estDailyFromSurfaces<-function(localDaily = Daily, localINFO = INFO, localsurfaces = surfaces) {
# this function uses the surfaces that have been calulated based on the sample data
# and fills in the individual estimates using bilinear interpolation off these surfaces
# it produces estimates of ConcDay, FluxDay, FNConc, and FNFlux
# these are appended to the data frame Daily, which is returned
#
  numDays<-length(localDaily$LogQ)
# need to make sure that localDaily$i starts with 1 (in case user subsetted the data frame)   
  localDaily$i<-seq(1,numDays)
# we need to add an extra column to Daily to handle leap year for flow normalization
# the 366'th day of the year is treated as an extra day 365
  localDaily$Leap<-ifelse(localDaily$Day<365,localDaily$Day,365)
# set up the grid values
  bottomLogQ<-localINFO$bottomLogQ
  stepLogQ<-localINFO$stepLogQ
  bottomYear<-localINFO$bottomYear
  stepYear<-localINFO$stepYear
  t0<-trunc((localDaily$DecYear-bottomYear)/stepYear)+1
  t1<-t0+1
  tat0<-((t0-1)*stepYear)+bottomYear
  tf<-(localDaily$DecYear - tat0)/stepYear
  q0<-trunc((localDaily$LogQ - bottomLogQ)/stepLogQ)+1
  q1<-q0+1
  qat0<-((q0-1)*stepLogQ)+bottomLogQ
  qf<-(localDaily$LogQ - qat0)/stepLogQ
  result<-array(0,dim=c(numDays,3))
  for(i in 1:numDays) {
    for(j in 1:3){
    	f00<-localsurfaces[q0[i],t0[i],j]
    	f01<-localsurfaces[q1[i],t0[i],j]
    	f10<-localsurfaces[q0[i],t1[i],j]
    	f11<-localsurfaces[q1[i],t1[i],j]
    	b1<-f00
    	b2<-f01-f00
    	b3<-f10-f00
    	b4<-f00-f10-f01+f11
    	result[i,j]<-b1+b2*qf[i]+b3*tf[i]+b4*qf[i]*tf[i]
    }
  }
	localDaily$yHat<-result[,1]
	localDaily$SE<-result[,2]
	localDaily$ConcDay<-result[,3]
	localDaily$FluxDay<-result[,3]*localDaily$Q*86.40
# next is flow normalization
  cat("\n flow normalization starting, when the numbers reach 365 it is done")
# first we loop through the first 365 days of they year, organizing a data frame of all days
# that are on that day of the year, the day of the iyear index is iday and the data frame is Daily1
#
# set up the columns for flow normalized results
	localDaily$FNConc<-array(0,numDays)
	localDaily$FNFlux<-array(0,numDays)
	for(iday in 1:365) {
    cat(" ",iday)
		Daily1<-subset(localDaily,Leap==iday)
		numDDay<-length(Daily1$Day)
		q0<-trunc((Daily1$LogQ - bottomLogQ)/stepLogQ)+1
		q1<-q0+1
		qat0<-((q0-1)*stepLogQ)+bottomLogQ
		qf<-(Daily1$LogQ - qat0)/stepLogQ
		vectorInd<-Daily1$i
# vectorInd is a vector of indices for each of the rows in Daily1, pointing back to the index value (row number)
# in the orginial data frame, Daily
#   jday is the day for which we are making the flow normalized estimate		
		for(jday in 1:numDDay) {
# note that for any date, the t variables are now all scalars and not vectors
# time is fixed but the discharges cover the range of historical discharges that happened on
#    this day in all years
			Year<-Daily1$DecYear[jday]
			t0<-trunc((Year-bottomYear)/stepYear)+1
			t1<-t0+1
			tat0<-((t0-1)*stepYear)+bottomYear
			tf<-(Year-tat0)/stepYear
#   start summing for Flow Normalized Concentration and Flux
			sumConc<-0
			sumFlux<-0
# we sum over all of the discharge values that have been observed on this day
#   ii is the counter of these historical days we are using to form the flow normalized value
#   for the day jday (which is also day number vectorInd[jday] in the original Daily data frame)			
			for(ii in 1:numDDay){
				f00<-localsurfaces[q0[ii],t0,3]
				f01<-localsurfaces[q1[ii],t0,3]
				f10<-localsurfaces[q0[ii],t1,3]
				f11<-localsurfaces[q1[ii],t1,3]
				b1<-f00
				b2<-f01-f00
				b3<-f10-f00
				b4<-f00-f10-f01+f11
				conc<-b1+b2*qf[ii]+b3*tf+b4*qf[ii]*tf
				sumConc<-sumConc+conc
				sumFlux<-sumFlux+conc*Daily1$Q[ii]*86.4
			}
			localDaily$FNConc[vectorInd[jday]]<-sumConc/numDDay
			localDaily$FNFlux[vectorInd[jday]]<-sumFlux/numDDay
		}
  }	
	return(localDaily)}
#
#
#' Estimates all daily values of Concentration, and Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these two time series 
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augemented version of the Daily data frame, which is returned.
#'    This code is identical to estDailyFromSurfaces but it lacks the flow normalization process.
#'    The exclusion of the flow-normalization process saves a large amount of computer time.
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localINFO string specifying the name of the data frame containing the meta-data, default is INFO
#' @param localsurfaces string specifying the name of the array containing the three surfaces, default is surfaces
#' @keywords water-quality statistics
#' @return localDaily string specifying the name of the data frame containing the daily values and these esimates
#' @export
#' @examples
#' \dontrun{estDailyWithoutNormalization(localDaily = exDailyStart, localINFO = exINFOStart, localsurfaces = exsurfaces)}
estDailyWithoutNormalization<-function(localDaily = Daily, localINFO = INFO, localsurfaces = surfaces) {
# this function uses the surfaces that have been calulated based on the sample data
# and fills in the individual estimates using bilinear interpolation off these surfaces
# it produces estimates of ConcDay, and FluxDay,
# it does not do Flow Normalization
# Because it doesn't do Flow Normalization it is much faster than
# estDailyFromSurfaces - The code is identical to the first part of estDailyFromSurfaces 
# these are appended to the data frame Daily, which is returned
#
  numDays<-length(localDaily$LogQ)
# we need to add an extra column to Daily to handle leap year for flow normalization
# the 366'th day of the year is treated as an extra day 365
  localDaily$Leap<-ifelse(localDaily$Day<365,localDaily$Day,365)
# set up the grid values
  bottomLogQ<-localINFO$bottomLogQ
  stepLogQ<-localINFO$stepLogQ
  bottomYear<-localINFO$bottomYear
  stepYear<-localINFO$stepYear
  t0<-trunc((localDaily$DecYear-bottomYear)/stepYear)+1
  t1<-t0+1
  tat0<-((t0-1)*stepYear)+bottomYear
  tf<-(localDaily$DecYear - tat0)/stepYear
  q0<-trunc((localDaily$LogQ - bottomLogQ)/stepLogQ)+1
  q1<-q0+1
  qat0<-((q0-1)*stepLogQ)+bottomLogQ
  qf<-(localDaily$LogQ - qat0)/stepLogQ
  result<-array(0,dim=c(numDays,3))
  for(i in 1:numDays) {
    for(j in 1:3){
    	f00<-localsurfaces[q0[i],t0[i],j]
    	f01<-localsurfaces[q1[i],t0[i],j]
    	f10<-localsurfaces[q0[i],t1[i],j]
    	f11<-localsurfaces[q1[i],t1[i],j]
    	b1<-f00
    	b2<-f01-f00
    	b3<-f10-f00
    	b4<-f00-f10-f01+f11
    	result[i,j]<-b1+b2*qf[i]+b3*tf[i]+b4*qf[i]*tf[i]
    }
  }
	localDaily$yHat<-result[,1]
	localDaily$SE<-result[,2]
	localDaily$ConcDay<-result[,3]
	localDaily$FluxDay<-result[,3]*localDaily$Q*86.40
#     These two lines are here just in case there were values already in FNConc and FNFlux
	localDaily$FNConc<-rep(NA,numDays)
	localDaily$FNFlux<-rep(NA,numDays)
	return(localDaily)
}
#
#
#
#
#'  A utility program for saving the contents of the workspace
#'   
#'  This function saves the workspace. 
#'  It assigns the file a name using the abbreviations for station and constituent.
#'
#' @param savePath string specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param localINFO string specifying the name of the INFO database
#' @keywords water-quality statistics
#' @export
#' @examples
#' \dontrun{saveResults("",localINFO = exINFOEnd)}
saveResults<-function(savePath, localINFO =INFO){
	saveName <- paste(savePath, INFO$staAbbrev, ".", INFO$constitAbbrev, 
        ".RData", sep = "")
    save.image(file=saveName)}
#
#
#
#' Produces annual series of 8 streamflow statistics (and a lowess smooth of them) from daily streamflow data
#'
#' Part of the flowHistory system.  The data come from Daily and INFO data frames. 
#' Note that the function setPA must be run before this to establish the period of analysis (e.g. water year).
#'
#' @param localDaily string specifying the name of a data frame that contains the daily streamflow data
#' @param localINFO string specifying the name of a data frame that contains the metadata
#' @keywords statistics streamflow trends
#' @export
#' @return annualSeries data frame that contains the annual series of streamflow statistics
#' @examples 
#' makeAnnualSeries(localDaily=exDailyStart,localINFO=exINFOEnd)
makeAnnualSeries<-function(localDaily = Daily, localINFO = INFO) {
	paStart<-localINFO$paStart
	paLong<-localINFO$paLong
	window<-localINFO$window
	numDays<-length(localDaily$DecYear)
	yearFirst<-trunc(localDaily$DecYear[1])
	yearLast<-trunc(localDaily$DecYear[numDays])
	monthSeqFirst<-localDaily$MonthSeq[1]
	monthSeqLast<-localDaily$MonthSeq[numDays]
	numYears<-yearLast-yearFirst+2
#     it is ok if numYears is too large, but not ok if it is too small
	annualSeries<-rep(NA,3*8*numYears)
	dim(annualSeries)<-c(3,8,numYears)
#  annualSeries is the matrix where results are stored
#  these are annual time series, but may not be based on the whole year
#     they are based on the period of analysis, which is a period as short as a month
#     or a consecutive group of months, or an entire year.
#     the period of analysis is defined by paStart and paLong
#  the annualSeries matrix has 3 dimensions
#   first dimension takes on three possible indexes
#           	1 is the mean date for the values for that year as a decimal year
#				2 is the actual value of the given flow statistic for the given year
#				3 is the smoothed value of the given flow statistic for the given year
#   second dimension is the flow statistic, these run from 1 to 8 (min day, 7-day min, 30-day min, median,mean,30-day max, 7-day max, and max day)  
#   third dimension is the year index 
#  first we will do the low flow statistics
	paStartLow<-if(paLong==12&paStart==10) 4 else paStart
#     this means that if we are doing water years, then the low flow statistics year starts April 1
#     in all other situations it starts with the month paStart, just like the average and high flow statistics
	Starts<-seq(paStartLow,monthSeqLast,12)
	Ends<-Starts + paLong - 1
#     Starts is a vector of the sequence number of the months that start each of the years
#     Ends is a vector of the sequence number of the months that end each of the years
	startEndSeq<-data.frame(Starts,Ends)
	startEndSeq<-subset(startEndSeq,Ends>=monthSeqFirst)
	startEndSeq<-subset(startEndSeq,Starts<=monthSeqLast)
	numYSeq<-length(startEndSeq$Ends)
	for(i in 1:numYSeq) {
		startSeq<-startEndSeq$Starts[i]
		endSeq<-startEndSeq$Ends[i]
		yearDaily<-subset(localDaily,MonthSeq>=startSeq)
		yearDaily<-subset(yearDaily,MonthSeq<=endSeq)
# count up the missing days
		counter<-ifelse(is.na(yearDaily$Q),1,0)
		goodDay<-length(yearDaily$Q)-sum(counter)
#  goodDay is how many good days
#  we want to have at least about 85% of the possible days with data
	good<-if(goodDay>26*paLong) TRUE else FALSE
	meanDecYear<-if(good) mean(yearDaily$DecYear) else NA
	annualSeries[1,1:3,i]<-meanDecYear
	annualSeries[2,1,i]<-if(good) min(yearDaily$Q,na.rm=TRUE) else NA
	annualSeries[2,2,i]<-if(good) min(yearDaily$Q7,na.rm=TRUE) else NA
	annualSeries[2,3,i]<-if(good) min(yearDaily$Q30,na.rm=TRUE) else NA
	}		
# now we do the median,mean and high flow statistics
	Starts<-seq(paStart,monthSeqLast,12)
	Ends<-Starts + paLong - 1
#     Starts is a vector of the sequence number of the months that start each of the years
#     Ends is a vector of the sequence number of the months that end each of the years
	startEndSeq<-data.frame(Starts,Ends)
	startEndSeq<-subset(startEndSeq,Ends>=monthSeqFirst)
	startEndSeq<-subset(startEndSeq,Starts<=monthSeqLast)
	numYSeq<-length(startEndSeq$Ends)
	for(i in 1:numYSeq) {
		startSeq<-startEndSeq$Starts[i]
		endSeq<-startEndSeq$Ends[i]
		yearDaily<-subset(localDaily,MonthSeq>=startSeq)
		yearDaily<-subset(yearDaily,MonthSeq<=endSeq)
# count up the missing days
		counter<-ifelse(is.na(yearDaily$Q),1,0)
		goodDay<-length(yearDaily$Q)-sum(counter)
#  goodDay is how many good days
#  we want to have at least about 85% of the possible days with data
	good<-if(goodDay>26*paLong) TRUE else FALSE
		meanDecYear<-if(good) mean(yearDaily$DecYear) else NA
	annualSeries[1,4:8,i]<-meanDecYear
	annualSeries[2,4,i]<-if(good) median(yearDaily$Q,na.rm=TRUE) else NA
	annualSeries[2,5,i]<-if(good) mean(yearDaily$Q,na.rm=TRUE) else NA
	annualSeries[2,6,i]<-if(good) max(yearDaily$Q30,na.rm=TRUE) else NA
	annualSeries[2,7,i]<-if(good) max(yearDaily$Q7,na.rm=TRUE) else NA
	annualSeries[2,8,i]<-if(good) max(yearDaily$Q,na.rm=TRUE) else NA
	}
# now we do the lowess smooth	
		for(istat in 1:8) {
		x<-annualSeries[1,istat,]
		y<-log(annualSeries[2,istat,])
		originalYear<-x
		xy<-data.frame(x,y)
		xy<-na.omit(xy)
		numXY<-length(xy$x)
		x<-xy$x
		newYear<-x
		numNewYear<-length(newYear)
		orYear<-originalYear[1:numNewYear]
		diff<-newYear-orYear
		meanDiff<-mean(diff,na.rm=TRUE)
		offset<-round(meanDiff)
		for (i in 1:numXY) {
			w<-triCube(x-x[i],window)
			mod<-lm(xy$y~x,weights=w)
			new<-data.frame(x=x[i])
			z<-exp(predict(mod,new))
			ioffset<-i+offset
			annualSeries[3,istat,ioffset]<-z}}
			return(annualSeries)}
#
#
#' Sets up the period of analysis to be used in makeAnnualSeries
#'
#' Part of the flowHistory system. 
#' Period of analysis is defined by the starting month (paStart) and length in months (paLong). 
#' paStart and paLong are constrained to be integers from 1 to 12. 
#' for example, a water year would be paStart = 10 and paLong = 12. 
#' for example, the winter season, defined by Dec,Jan,Feb would be paStart = 12 and paLong =3.
#'
#' @param paStart A numeric value for the starting month of the Period of Analysis, default is 10
#' @param paLong A numeric value for the length of the Period of Analysis in months, default is 12
#' @param window A numeric value for the half-width of a smoothing window for annual streamflow values, default is 30
#' @param localINFO A character string specifying the name of the metadata data frame
#' @keywords statistics streamflow
#' @export
#' @return localInfo A data frame containing the metadata
#' @examples
#' setPA(paStart=12, paLong=3, localINFO=exINFOStart)
setPA<-function(paStart=10, paLong=12, window = 30,localINFO = INFO) {
# The purpose of setPA is just to get the paStart, paLong, and window into the INFO data frame, 
# so they can be used to run the function makeAnnualSeries
	if(exists("annualSeries"))
    rm(annualSeries,envir=sys.frame(-1))
	localINFO$paStart <- paStart
	localINFO$paLong <- paLong
	localINFO$window <- window
	return(localINFO)
}
#
#
#
#' Creates a plot of a time series of a particular flow statistic and a lowess smooth of that flow statistic
#'
#' A part of the flowHistory system.
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum
#'
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param localINFO A character string specifying the name of the metadata data frame
#' @param localAnnualSeries A character string specifying the name of a data frame containing the annual series
#' @param qMax A numeric value for the maximum value to be used for y-axis of graph, default is NA means that graph is self-scaling
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param printStaName logical variable, if TRUE station name is printed in title, if FALSE not printed, default is TRUE
#' @param printPA logical variable, if TRUE Period of Analysis information is printed in title, if FALSE not printed, default is TRUE
#' @param printIstat logical variable, if TRUE print the statistic name is printed in title, if FALSE not printed, default is TRUE
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' plotFlowSingle(8,localINFO=exINFOEnd,localAnnualSeries=exannualSeries)
plotFlowSingle<-function(istat,yearStart=NA, yearEnd = NA, localINFO = INFO, localAnnualSeries = annualSeries, qMax = NA, printTitle = TRUE, tinyPlot = FALSE, runoff = FALSE, qUnit = 1, printStaName = TRUE, printPA = TRUE, printIstat = TRUE) {
	qActual<-localAnnualSeries[2,istat,]
	qSmooth<-localAnnualSeries[3,istat,]
	years<-localAnnualSeries[1,istat,]
	par(mar =  c(5,6,5,2))
################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  qFactor<-qUnit@qUnitFactor
  qActual<-if(runoff) qActual*86.4/localINFO$drainSqKm else qActual*qFactor
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  localSeries<-data.frame(years,qActual,qSmooth)
  localSeries<-if(is.na(yearStart)) localSeries else subset(localSeries,years>=yearStart)
  localSeries<-if(is.na(yearEnd)) localSeries else subset(localSeries,years<=yearEnd)
  minYear<-min(localSeries$years,na.rm=TRUE)
  maxYear<-max(localSeries$years,na.rm=TRUE)
  xLeft<-if(is.na(yearStart)) minYear else yearStart
  numYears<-length(localSeries$years)
  xRight<-if(is.na(yearEnd)) maxYear else yearEnd
  nTicks<-if(tinyPlot) 5 else 8
  xSpan<-c(xLeft,xRight)
  xTicks<-pretty(xSpan,n=nTicks)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  yTop<-if(is.na(qMax)) 1.1*max(qActual,na.rm=TRUE) else qMax
  ySpan<-c(0,yTop)
  yTicks<-pretty(ySpan,n=nTicks)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  yLab<-if(runoff) "mm/day" else qUnit@qUnitExpress
  line1<-if(printStaName) localINFO$shortName else ""	
  line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)) else ""
  nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
  line3<-if(printIstat) paste("\n",nameIstat[istat]) else ""
  	title<-if(printTitle) paste(line1,line2,line3) else ""
  	plot(localSeries$years,localSeries$qActual,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(yBottom,yTop),yaxs="i",ylab=yLab,main=title,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2,pch=20)
  	axis(1,tcl=0.5,at=xTicks,labels=xTicks)
  	axis(2,tcl=0.5,las=1,at=yTicks,labels=yTicks,cex.axis=1.1)
  	axis(3,tcl=0.5,at=xTicks,labels=FALSE)
  	axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  	box()
  	par(new=TRUE)
  	plot(localSeries$years,localSeries$qSmooth,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",xlab="",ylim=c(yBottom,yTop),yaxs="i",ylab="",main="",cex=0.8,cex.main=1.1,cex.lab=1.2,font=2,type="l",lwd=2)
  	par(mar=c(5,4,4,2)+0.1)
	}
#
#
#' Makes four graphs of annual streamflow statistics on a single page
#'
#'  Part of the flowHistory system.  The four statistics are 1-day maximum, annual mean, annual median, and annual 7-day minimum.
#'  Prior to running this code user must have run setPA and makeAnnualSeries.
#'
#' @param localINFO string specifying the name of the data frame that contains the metadata, defoult name is INFO
#' @param localAnnualSeries string specifying the name of the data frame that contains the annual series of statistics, default is annualSeries
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' plotFourStats(localINFO=exINFOEnd,localAnnualSeries=exannualSeries)
plotFourStats<-function(localINFO = INFO, localAnnualSeries = annualSeries, yearStart = NA, yearEnd = NA, printTitle = TRUE, runoff = FALSE, qUnit =1) {
# prior to running this user must do these two commands
# INFO<-setPA(pastart,paLong,window) 
# annualSeries<-makeAnnualSeries()
#
	par(mfcol=c(2,2),cex=0.6,oma=c(0,1.7,6,1.7),cex.lab=1.4,cex.axis=1.2)
	setYearStart<-if(is.na(yearStart)) min(localAnnualSeries[1,,],na.rm=TRUE) else yearStart
	setYearEnd<-if(is.na(yearEnd)) max(localAnnualSeries[1,,],na.rm=TRUE) else yearEnd
	plotFlowSingle(istat=8, yearStart=setYearStart, yearEnd=setYearEnd, localAnnualSeries=localAnnualSeries, localINFO=localINFO, tinyPlot=TRUE, runoff=runoff, qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE)
	plotFlowSingle(istat=4, yearStart=setYearStart, yearEnd=setYearEnd, localAnnualSeries=localAnnualSeries, localINFO=localINFO, tinyPlot=TRUE, runoff=runoff, qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE)
	plotFlowSingle(istat=5, yearStart=setYearStart, yearEnd=setYearEnd, localAnnualSeries=localAnnualSeries, localINFO=localINFO, tinyPlot=TRUE, runoff=runoff, qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE)
	plotFlowSingle(istat=2, yearStart=setYearStart, yearEnd=setYearEnd, localAnnualSeries=localAnnualSeries, localINFO=localINFO, tinyPlot=TRUE, runoff=runoff, qUnit=qUnit, printPA=FALSE, printIstat=TRUE, printStaName=FALSE)
	textPA<-setSeasonLabelByUser(paStartInput=localINFO$paStart, paLongInput=localINFO$paLong)
	title<-if(printTitle) paste(localINFO$shortName,"\n",textPA)
	mtext(title,cex=1.2,outer=TRUE,font=2)
	par(mfcol=c(1,1),oma=c(0,0,0,0))	}
#
#
########################################
# placeholder for plot15 code, it is in the flowHistory code in the MainRCodes folder but it still needs work
#########################################
#
#' Print annual results for a given streamflow statistic
#'
#' Part of the flowHistory system.  
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum. 
#'  User must have run setPA and makeAnnualSeries before this function.
#'
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param localINFO A character string specifying the name of the metadata data frame
#' @param localAnnualSeries A character string specifying the name of a data frame containing the annual series
#' @keywords streamflow statistics
#' @export
#' @examples
#' printSeries(5,localINFO=exINFOEnd, localAnnualSeries=exannualSeries)
printSeries<-function(istat, qUnit = 1, runoff = FALSE, localINFO = INFO, localAnnualSeries = annualSeries) {
################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
###############################################################################
cat("\n",localINFO$shortName)
seasonText<-setSeasonLabelByUser(paStart=localINFO$paStart,paLong=localINFO$paLong)
cat("\n",seasonText)
nameIstat<-c("minimum day","7-day minimum","30-day minimum","median daily","mean daily","30-day maximum","7-day maximum",'maximum day')
cat("\n   ",nameIstat[istat])
unitsText<-if(runoff) "runoff in mm/day" else qUnit@qUnitName
cat("\n   ",unitsText)
cat("\n   year   annual   smoothed\n           value    value\n\n")
	qActual<-localAnnualSeries[2,istat,]
	qSmooth<-localAnnualSeries[3,istat,]
	years<-localAnnualSeries[1,istat,]
	qFactor<-qUnit@qUnitFactor
  qActual<-if(runoff) qActual*86.4/localINFO$drainSqKm else qActual*qFactor
  qSmooth<-if(runoff) qSmooth*86.4/localINFO$drainSqKm else qSmooth*qFactor
  toPrint<-data.frame(years,qActual,qSmooth)
  toPrint<-subset(toPrint,!is.na(years))
  toPrint$years<-format(toPrint$years,digits=4,width=7)
  toPrint$qActual<-format(toPrint$qActual,digits=3,width=8)
  toPrint$qSmooth<-format(toPrint$qSmooth,digits=3,width=8)
  write.table(toPrint,file="",col.names=FALSE,row.names=FALSE,quote=FALSE)
}
#
#
#
#' Prints table of change metrics for a given streamflow statistic
#'
#' Part of the flowHistory system.
#' The index of the flow statistics is istat.  These statistics are: 
#' (1) 1-day minimum, (2) 7-day minimum, (3) 30-day minimum, (4) median
#' (5) mean, (6) 30-day maximum, (7) 7-day maximum, and (8) 1-day maximum. 
#'  User must have run setPA and makeAnnualSeries before this function.
#'
#' @param istat A numeric value for the flow statistic to be graphed (possible values are 1 through 8)
#' @param localINFO A character string specifying the name of the metadata data frame
#' @param localAnnualSeries A character string specifying the name of a data frame containing the annual series
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param yearPoints A vector of numeric values, specifying the years at which change metrics are to be calculated, default is NA (which allows the function to set these automatically), yearPoints must be in ascending order
#' @keywords streamflow statistics
#' @export
#' @examples
#' tableFlowChange(istat=5,localAnnualSeries=exannualSeries,localINFO=exINFOEnd,yearPoints=c(2001,2005,2009))
tableFlowChange<-function(istat, localAnnualSeries = annualSeries, localINFO = INFO, qUnit = 1, runoff = FALSE, yearPoints = NA) {
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
	periodName<-setSeasonLabelByUser(paStart = localINFO$paStart, paLong = localINFO$paLong)
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
	cat("                     ",unitsText,unitsText,"/yr         %         %/yr")
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
			}}
		}
#
#
#' Plots the difference between two years from a contour plot created by plotContours
#'
#' These plots are normally used for plotting changes in the estimated concentration surface (whatSurface=3) but can be used to explore the 
#' changes in estimated surfaces for the log of concentration or for the standard error (in log space) which is what determines the bias correction.
#'
#' @param year0 numeric value for the calendar year that is the first year of the pair of years for the analysis, should be a whole number
#' @param year1 numeric value for the calendar year that is the second year of the pair of years for the analysis, should be a whole number
#' @param qBottom numeric value for the bottom edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param qTop numeric value for the top edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit)
#' @param maxDiff numeric value which is the absolute value of the largest change in concentration that will be shown on the figure
#' @param whatSurface numeric value, can only accept 1, 2, or 3;  whatSurface=1 is yHat (log concentration), whatSurface=2 is SE (standard error of log concentration), and whatSurface=3 is ConcHat (unbiased estimate of concentration), default = 3
#' @param localsurfaces string specifying the name of the matrix that contains the estimated surfaces, default is surfaces
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param localDaily string specifying the name of the data frame that contains the daily data, default name is Daily
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param span numeric, it is the half-width (in days) of the smoothing window for computing the flow duration information, default = 60
#' @param pval numeric, the probability value for the lower flow frequency line on the graph
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param vert1 numeric, the location in time for a black vertical line on the figure, yearStart<vert1<yearEnd, default is NA (vertical line is not drawn) 
#' @param vert2 numeric, the location in time for a black vertical line on the figure, yearStart<vert2<yearEnd, default is NA (vertical line is not drawn)
#' @param horiz numeric, the location in discharge for a black horizontal line on the figure, qBottom<vert1<qTop, default is NA (no horizontal line is drawn)
#' @param flowDuration logical variable if TRUE plot the flow duration lines, if FALSE do not plot them, default = TRUE
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' year0<-2001
#' year1<-2009
#' qBottom<-0.2
#' qTop<-20
#' maxDiff<-0.5
#' plotDiffContours(year0,year1,qBottom,qTop,maxDiff, localsurfaces = exsurfaces, localINFO = exINFOEnd, localDaily = exDailyEnd)
plotDiffContours<-function (year0, year1, qBottom, qTop, maxDiff, whatSurface = 3, 
    localsurfaces = surfaces, localINFO = INFO, localDaily = Daily, 
    qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, 
    vert1 = NA, vert2 = NA, horiz = NA, flowDuration = TRUE) 
{
    if (is.numeric(qUnit)) {
        qUnit <- qConst[shortCode = qUnit][[1]]
    }
    else if (is.character(qUnit)) {
        qUnit <- qConst[qUnit][[1]]
    }
    par(oma = c(6, 1, 6, 0))
    par(mar = c(5, 5, 4, 2) + 0.1)
    surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
        "Concentration")
    j <- 3
    j <- if (whatSurface == 1) 
        1
    else j
    j <- if (whatSurface == 2) 
        2
    else j
    surf <- localsurfaces
 
    bottomLogQ <- localINFO$bottomLogQ
    stepLogQ <- localINFO$stepLogQ
    nVectorLogQ <- localINFO$nVectorLogQ
    bottomYear <- localINFO$bottomYear
    stepYear <- localINFO$stepYear
    nVectorYear <- localINFO$nVectorYear
        start0<-((year0-bottomYear)*16)+1
        end0<-start0+16
        start1<-((year1-bottomYear)*16)+1
        end1<-start1+16
        diff<-surf[,start1:end1,j] - surf[,start0:end0,j]
        difft<-t(diff)
    surfaceSpan <- c(-maxDiff,maxDiff)
    contourLevels <- pretty(surfaceSpan, n = 15)
    x <- seq(0,1,stepYear)
    y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
    yLQ <- y
    qFactor <- qUnit@qUnitFactor
    y <- exp(y) * qFactor
    numX <- length(x)
    numY <- length(y)
    qBottom <- max(0.9 * y[1], qBottom)
    qTop <- min(1.1 * y[numY], qTop)
    xTicks <- c(0,0.0848,0.1642,0.249,0.331,0.416,0.498,0.583,0.668,0.750,0.835,0.917,1)
    xLabels <- c("Jan1","Feb1","Mar1","Apr1","May1","Jun1","Jul1","Aug1","Sep1","Oct1","Nov1","Dec1","Jan1")
    nxTicks<-length(xTicks)
    yTicks <- logPretty3(qBottom, qTop)
    nYTicks <- length(yTicks)
    numDays <- length(localDaily$Day)
    freq <- rep(0, nVectorLogQ)
    plotTitle <- if (printTitle) 
        paste(localINFO$shortName, "  ", localINFO$paramShortName, 
            "\nEstimated", surfaceName[j], "change from",year0,"to",year1)
    else ""

    if(flowDuration) {
    durSurf <- rep(0, 17 * nVectorLogQ)
    dim(durSurf) <- c(17, nVectorLogQ)
    centerDays <- seq(1, 388, 22.9)
    centerDays <- floor(centerDays)
    for (ix in 1:17) {
        startDay <- centerDays[ix] - span
        endDay <- centerDays[ix] + span
        goodDays <- seq(startDay, endDay, 1)
        goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
            365)
        goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
            365)
        numDays <- length(localDaily$Day)
        isGood <- rep(FALSE, numDays)
        for (i in 1:numDays) {
            count <- ifelse(localDaily$Day[i] == goodDays, 1, 
                0)
            isGood[i] <- if (sum(count) > 0) 
                TRUE
            else FALSE
        }
        spanDaily <- data.frame(localDaily, isGood)
        spanDaily <- subset(spanDaily, isGood)
        n <- length(spanDaily$Day)
        LogQ <- spanDaily$LogQ
        for (jQ in 1:nVectorLogQ) {
            ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
            freq[jQ] <- sum(ind)/n
        }
        
            durSurf[ix, ] <- freq
        }
    plevels <- c(pval, 1 - pval)
    pct1 <- format(plevels[1] * 100, digits = 2)
    pct2 <- format(plevels[2] * 100, digits = 2)
    plotTitle <- if (printTitle) 
        paste(localINFO$shortName, "  ", localINFO$paramShortName, 
            "\nEstimated", surfaceName[j], "change from",year0,"to",year1,"\nBlack lines are", 
            pct1, "and", pct2, "flow percentiles")
    else ""
    }
    vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, 
        log(yTicks[1], 10) - 1)
    v1 <- if (is.na(vert1)) 
        vectorNone
    else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 
        10))
    v2 <- if (is.na(vert2)) 
        vectorNone
    else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 
        10))
    h1 <- if (is.na(horiz)) 
        vectorNone
    else c(year0, log(horiz, 10), year1, log(horiz, 10))
    
    yLab <- qUnit@qUnitExpress
    filled.contour(x, log(y, 10), difft, levels = contourLevels, 
        xlim = c(0,1), ylim = c(log(yTicks[1], 
            10), log(yTicks[nYTicks], 10)), main = plotTitle, 
        xlab = "", ylab = yLab, xaxs = "i", yaxs = "i", cex.main = 0.95, 
        plot.axes = {
            axis(1, tcl = 0.5, at = xTicks, labels = xLabels, cex.axis=0.9)
            axis(2, tcl = 0.5, las = 1, at = log(yTicks, 10), 
                labels = yTicks, cex.axis=1.0)
            axis(3, tcl = 0.5, at = xTicks, labels =FALSE)
            axis(4, tcl = 0.5, at = log(yTicks, 10), labels=FALSE)
            if(flowDuration) contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
                levels = plevels)
            segments(v1[1], v1[2], v1[3], v1[4])
            segments(v2[1], v2[2], v2[3], v2[4])
            segments(h1[1], h1[2], h1[3], h1[4])
        })
    par(oma = c(0, 0, 0, 0))
    par(mar = c(5, 4, 4, 2) + 0.1)
}
#
#
#' Plot of the time series of daily flux estimates and the sample values for the days that were sampled
#'
#' This plot is useful for visual examination of the ability of the WRTDS, or other model, to fit the 
#' data, as seen in a time-series perspective. 
#'
#' @param startYear numeric specifying the starting date (expressed as decimal years, for example 1989.0) for the plot
#' @param endYear numeric specifiying the ending date for the plot 
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param fluxUnit number representing in pre-defined fluxUnit class array. \code{\link{fluxConst}}
#' @param fluxMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' \dontrun{plotFluxTimeDaily(2001,2009,localSample=exSampleEnd,localDaily=exDailyEnd,localINFO=exINFOEnd)}
plotFluxTimeDaily<-function (startYear, endYear, localSample = Sample, localDaily = Daily, 
    localINFO = INFO, tinyPlot = FALSE, fluxUnit = 3, fluxMax = NA, printTitle = TRUE) {
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(fluxUnit)){
    fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
  } else if (is.character(fluxUnit)){
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  ################################################################################    
  
    if (tinyPlot) 
        par(mar = c(5, 4, 1, 1))
    else par(mar = c(5, 4, 4, 2) + 0.1)
    fluxFactor <- fluxUnit@unitFactor*86.40
    subSample <- subset(localSample, DecYear >= startYear)
    subSample <- subset(subSample, DecYear <= endYear)
    subDaily <- subset(localDaily, DecYear >= startYear)
    subDaily <- subset(subDaily, DecYear <= endYear)
    xSample <- subSample$DecYear
    xDaily <- subDaily$DecYear
    xLimits <- c(startYear, endYear)
    xTicks <- pretty(xLimits, n = 5)
    numXTicks <- length(xTicks)
    xLeft <- xTicks[1]
    xRight <- xTicks[numXTicks]
    yLow <- subSample$ConcLow*subSample$Q*fluxFactor
    yHigh <- subSample$ConcHigh*subSample$Q*fluxFactor
    Uncen <- subSample$Uncen
    yAll <- c(subDaily$ConcDay*subDaily$Q*fluxFactor, subSample$ConcHigh*subSample$Q*fluxFactor)
    maxYHigh <- if (is.na(fluxMax)) 
        1.05 * max(yAll)
    else fluxMax
    yTicks <- yPretty(maxYHigh)
    yTop <- yTicks[length(yTicks)]
    plotTitle <- if (printTitle) 
        paste(localINFO$shortName, "\n", localINFO$paramShortName, 
            "\n", "Observed and Estimated Flux versus Time")
    else ""
    plot(xSample, yHigh, axes = FALSE, xlim = c(xLeft, xRight), 
        xaxs = "i", xlab = "", ylim = c(0, yTop), yaxs = "i", 
        ylab = fluxUnit@unitExpress, main = plotTitle, pch = 20, 
        cex = 0.7, cex.main = 1.3, font.main = 2, cex.lab = 1.2)
    axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
    axis(2, tcl = 0.5, las = 1, at = yTicks)
    axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
    axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
    par(new = TRUE)
    plot(xDaily, subDaily$ConcDay*subDaily$Q*fluxFactor, axes = FALSE, xlim = c(xLeft, 
        xRight), xaxs = "i", xlab = "", ylim = c(0, yTop), yaxs = "i", 
        ylab = "", main = "", type = "l", cex.main = 1.3, font.main = 2, 
        cex.lab = 1.2)
    box()
    yLowVal <- ifelse(is.na(yLow), 0, yLow)
    numSamples <- length(xSample)
    uncensoredIndex <- 1:numSamples
    uncensoredIndex <- uncensoredIndex[Uncen == 0]
    segments(xSample[uncensoredIndex], yLowVal[uncensoredIndex], 
        xSample[uncensoredIndex], yHigh[uncensoredIndex])
    par(mar = c(5, 4, 4, 2) + 0.1)
}
#
#
#
#' A utility program to determine the Julian day for any given date
#'
#' Julian days are computed with an origin of "1850-01-01". 
#' Data input is, in quotes, something like "1949-09-30"
#'
#' @param date character string specifying a date.  In quotes as "yyyy-mm-dd"
#' @keywords water quality, streamflow, statistics
#' @export
#' @return Julian a numeric value, representing the julian day since 1850-01-01
#' @examples
#' getJulian("1949-09-30")
getJulian<-function(date) {
# enter date in quotes for example "1949-09-30" 
# program returns the julian date	
	dateTime <- as.Date(date)
    Julian <- as.numeric(julian(dateTime, origin = as.Date("1850-01-01")))
    return(Julian)
}
######################################
#	
#  New material on 05June2012 below here
#
######################################
#
#' Graph of the standard deviation of the log of daily discharge versus year
#'
#' Data come from data frame named Daily.
#' The metadata come from a data frame named INFO.
#' User must have already run the function, INFO<-setPA()
#' Can be used to analyze a specific period of analysis by
#' Running INFO<-setPA(paStart,paLong)
#'
#' @param yearStart numeric is the calendar year of the first value to be included in graph, default is NA, which plots from the start of the period of record
#' @param yearEnd numeric is the calendar year of the last value to be included in graph, default is NA, which plots to the end of the period of record
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @param localDaily string specifying the name of the data frame that contains the daily streamflow data, default name is Daily
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param sdMax numeric is the maximum value to be used on the vertical axis of the graph, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure), default is TRUE
#' @param tinyPlot logical variable if TRUE plot is designed to be small, if FALSE it is designed for page size, default is FALSE (not fully implemented yet)
#' @param printStaName logical variable, if TRUE print the station name, if FALSE do not, default is TRUE
#' @param printPA logical variable, if TRUE print the period of analysis information in the plot title, if FALSE leave it out, default is TRUE
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' plotSDLogQ(window=3,localDaily=exDailyStart,localINFO=exINFOEnd,printTitle=FALSE)  
plotSDLogQ<-function(yearStart=NA,yearEnd=NA,window=15,localDaily=Daily,localINFO=INFO,sdMax=NA,printTitle = TRUE, tinyPlot = FALSE, printStaName = TRUE, printPA = TRUE){
	par(mar = c(5,6,5,2))
	numDays<-length(localDaily$LogQ)
	paLong <- localINFO$paLong
	paStart <- localINFO$paStart
	localDaily <- if(paLong == 12) localDaily else selectDays(paLong,paStart,localDaily)
	numDays<-length(localDaily$LogQ)
	startDec<-localDaily$DecYear[1]
	endDec<-localDaily$DecYear[numDays]
	startDays<-seq(startDec,endDec-window,0.1)
	numResults<-length(startDays)
	y<-rep(NA,numResults)
	xmid<-startDays+(window/2)
	for (i in 1:numResults){
	firstDay<-startDays[i]
	lastDay<-startDays[i]+window
	smallDaily<-subset(localDaily,DecYear>=firstDay&DecYear<=lastDay)
	y[i]<-sd(smallDaily$LogQ,na.rm=TRUE)
	}
	yTop<-if(is.na(sdMax)) 1.05*max(y)
	yTicks<-yPretty(yTop)
	numYTicks<-length(yTicks)
	yTop<-yTicks[numYTicks]
	xMin<-if(is.na(yearStart)) startDec else yearStart
	xMax<-if(is.na(yearEnd)) endDec else yearEnd
	nTicks <- if (tinyPlot) 5 else 8
	yearSpan<-c(xMin,xMax)
	xTicks<- pretty(yearSpan,n = nTicks)
	numXTicks<-length(xTicks)
	xLeft<-xTicks[1]
	xRight<-xTicks[numXTicks]
	line1<-if(printStaName) localINFO$shortName else ""
	line2<-if(printPA) paste("\n",setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong)) else ""
	line3<-"\nDischarge variability: Standard Deviation of Log(Q)" 
	title<-if(printTitle) paste(line1,line2,line3) else ""
plot(xmid,y,type="l",ylim=c(0,yTop),yaxs="i",lwd=2,xlim=c(xLeft,xRight),xaxs="i",main=title,xlab="",ylab="Dimensionless",axes=FALSE,cex=0.8,cex.main=1.1,cex.lab=1.2,font=2)
	axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
    axis(2, tcl = 0.5, las = 1, at = yTicks, cex.axis = 1.1)
    axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
    axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
    box()
    par(mar = c(5, 4, 4, 2) + 0.1)
}
#
#
#' Creates a subset of the Daily data frame that only contains data for the specified period of analysis
#'
#'  Function is not called by user, but is called by plotSDLogQ
#'
#' @param paLong a numeric value for the length of the period of Analysis, must be an integer from 1 to 12
#' @param paStart a numeric value for the starting month of the period of analysis, must be an integer from 1 to 12
#' @param localDaily a character string specifying the name of the daily data frame to be used
#' @keywords statistics streamflow
#' @export
#' @return localDaily a data frame containing the daily data but only for the period of analysis (not all months)
#' @examples
#' selectDays(1,3,localDaily=exDailyStart)
selectDays<-function(paLong,paStart,localDaily) {
	numDays<-length(localDaily$Q)
	goodMonth<-rep(FALSE,12)
	for (iMonth in 1:paLong) {
		monthInd <- paStart + iMonth -1
		monthInd <- if(monthInd > 12) monthInd-12 else monthInd
		goodMonth[monthInd] = TRUE
	}
	keep<-rep(TRUE,numDays)
	for(i in 1:numDays) {
		keep[i]<-ifelse(goodMonth[localDaily$Month[i]],TRUE,FALSE)
	}
	localDaily<-data.frame(localDaily,keep)
	localDaily<-subset(localDaily,keep==TRUE)
	return(localDaily)
}
#
#
#' Makes four graphs of streamflow statistics on a single page
#'
#'  Part of the flowHistory system.  The four statistics are 1-day maximum, annual mean, annual 7-day minimum, and the running standard deviation of the log daily discharge values.
#'  Prior to running this code user must have run setPA and makeAnnualSeries.
#'
#' @param localINFO string specifying the name of the data frame that contains the metadata, defoult name is INFO
#' @param localAnnualSeries string specifying the name of the data frame that contains the annual series of statistics, default is annualSeries
#' @param localDaily string specifying the name of the data frame that contains the daily values, default is Daily
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param printTitle logical variable, if TRUE title is printed, if FALSE title is not printed, default is TRUE
#' @param runoff logical variable, if TRUE the streamflow data are converted to runoff values in mm/day
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' \dontrun{plotFour(localINFO=exINFOEnd,localAnnualSeries=exannualSeries,localDaily=exDailyStart)}
plotFour<-function (localINFO = INFO, localAnnualSeries = annualSeries, localDaily = Daily, 
    yearStart = NA, yearEnd = NA, printTitle = TRUE, runoff = FALSE, 
    qUnit = 1) 
{
    par(mfcol = c(2, 2), cex = 0.6, oma = c(0, 1.7, 6, 1.7), 
        cex.lab = 1.4, cex.axis = 1.2)
    setYearStart <- if (is.na(yearStart)) 
        min(localAnnualSeries[1, , ], na.rm = TRUE)
    else yearStart
    setYearEnd <- if (is.na(yearEnd)) 
        max(localAnnualSeries[1, , ], na.rm = TRUE)
    else yearEnd
    plotFlowSingle(istat = 8, yearStart = setYearStart, yearEnd = setYearEnd, 
        localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
        tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
        printIstat = TRUE, printStaName = FALSE)
    plotFlowSingle(istat = 2, yearStart = setYearStart, yearEnd = setYearEnd, 
        localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
        tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
        printIstat = TRUE, printStaName = FALSE)
    plotFlowSingle(istat = 5, yearStart = setYearStart, yearEnd = setYearEnd, 
        localAnnualSeries = localAnnualSeries, localINFO = localINFO, 
        tinyPlot = TRUE, runoff = runoff, qUnit = qUnit, printPA = FALSE, 
        printIstat = TRUE, printStaName = FALSE)
    plotSDLogQ(yearStart = setYearStart, yearEnd = setYearEnd, window = 15, 
        localDaily = localDaily, localINFO = localINFO, 
        tinyPlot = TRUE, printPA = FALSE,  
        printStaName = FALSE)
    
    textPA <- setSeasonLabelByUser(paStartInput = localINFO$paStart, 
        paLongInput = localINFO$paLong)
    title <- if (printTitle) 
        paste(localINFO$shortName, "\n", textPA)
    mtext(title, cex = 1.2, outer = TRUE, font = 2)
    par(mfcol = c(1, 1), oma = c(0, 0, 0, 0))
}
#
#' plots 1 of the 15 graphs of streamflow statistics on a single page
#'
#' Part of the flowHistory system.  The 15 graphs include annual and four seasonal graphs
#' for each of 3 flow statistics: 1-day maximum, mean, and 7-day minimum
#' @param yearStart A numeric value for the year in which the graph should start
#' @param yearEnd A numeric value for the year in which the graph should end
#' @param qf a scale factor to convert discharge in cubic feet per second to mm/day
#' @param istat A numeric value selecting the flow statistic to be plotted, must be an integer from 1 to 8
#' @param localAnnualSeries string indicating the name of the data frame that contains the annual series data for all 8 statistics for the given period of analysis
#' @param localINFO string indicating the name of the data frame that contains the meta data
#' @param isBottom logical, if TRUE the graph is from the bottom row and thus needs x axis labels, if FALSE it does not need labels
#' @keywords graphics streamflow
#' @export
#' @examples
#' plot1of15(1990,2000,0.2938476,5,localAnnualSeries=exannualSeries,localINFO=exINFOEnd)
plot1of15<-function(yearStart,yearEnd,qf,istat,localAnnualSeries,localINFO,isBottom=FALSE) {
	xSpan<-c(yearStart,yearEnd)
	xTicks<-pretty(xSpan,n=6)
	numXTicks<-length(xTicks)
	xLeft<-xTicks[1]
	xRight<-xTicks[numXTicks]
	x<-localAnnualSeries[1,istat,]
	y<-qf*localAnnualSeries[2,istat,]
	yTop<-1.05*max(y,na.rm=TRUE)
	yTicks<-yPretty(yTop)
	numYTicks<-length(yTicks)
	yTop<-yTicks[numYTicks]
plot(x,y,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",ylim=c(0,yTop),yaxs="i",ylab="",xlab="",main="",type="p")
	if(isBottom) axis(1,tcl=0.5,at=xTicks,labels=xTicks) else axis(1,tcl=0.5,at=xTicks,labels=FALSE)
	axis(2,tcl=0.5,at=yTicks,labels=TRUE)
	axis(3,tcl=0.5,at=xTicks,labels=FALSE)
	axis(4,tcl=0.5,at=yTicks,labels=FALSE)
	y<-qf*localAnnualSeries[3,istat,]
	par(new=TRUE)
plot(x,y,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",ylim=c(0,yTop),yaxs="i",ylab="",xlab="",main="",type="l")
	axis(1,tcl=0.5,at=xTicks,labels=FALSE)
	axis(2,tcl=0.5,at=yTicks,labels=FALSE)
	axis(3,tcl=0.5,at=xTicks,labels=FALSE)
	axis(4,tcl=0.5,at=yTicks,labels=FALSE)
	box()
}
#
#
#' A special version of setPA for use inside the plot15 function
#'
#' Part of the flowHistory system
#' Users should not need to call this function
#'
#' @param paStart A numeric value for the starting month of the Period of Analysis, default is 10
#' @param paLong A numeric value for the length of the Period of Analysis in months, default is 12
#' @param window A numeric value for the half-width of a smoothing window for annual streamflow values, default is 30
#' @param localINFO A character string specifying the name of the metadata data frame
#' @keywords statistics streamflow
#' @export
#' @return localInfo A data frame containing the metadata
#' @examples
#' setPAx(paStart=12, paLong=3, localINFO=exINFOStart)
setPAx<-function (paStart = 10, paLong = 12, window = 30, localINFO = INFO) 
{
    localINFO$paStart <- paStart
    localINFO$paLong <- paLong
    localINFO$window <- window
    return(localINFO)
}
#
#
#' Makes 15 graphs of streamflow statistics on a single page
#'
#' Part of flowHistory system.
#'  This function saves a graphic as a postscript file. 
#'  It assigns the file a name using the abbreviations for station.
#'
#' @param savePath string specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param localDaily string specifying the name of the data frame that contains the daily data, default is Daily
#' @param localINFO string specifying the name of the data frame that contains the metadata, default is INFO
#' @keywords graphics streamflow statistics
#' @export
#' @examples
#' \dontrun{plot15("",yearStart=1990,yearEnd=2000,localDaily=exDailyStart,localINFO=exINFOStart)}
plot15<-function(savePath,yearStart,yearEnd,localDaily=Daily,localINFO=INFO){
plotName<-paste(savePath,"plot15.",localINFO$staAbbrev,".ps",sep="")
postscript(file=plotName,width=8,height=10,horizontal=FALSE,family="Helvetica")
par(mfrow=c(5,3),cex=0.6,oma=c(10,8,10,4),mar=c(1,4,1,1))
qf<-86/localINFO$drainSqKm
newINFO<-setPAx(10,12,localINFO=localINFO)
newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
mtext("7-day minimum",cex=0.8,font=1,side=3,line=1)
mtext("Annual values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
mtext("Mean",cex=0.8,font=1,side=3,line=1)
mtext(INFO$shortName,cex=1.0,font=1,side=3,line=4)
plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
mtext("1-day maximum",cex=0.8,font=1,side=3,line=1)
# fall season
newINFO<-setPAx(9,3,localINFO=localINFO)
newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
mtext("Fall season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
# winter season
newINFO<-setPAx(12,3,localINFO=localINFO)
newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
mtext("Winter season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
# spring season
newINFO<-setPAx(3,3,localINFO=localINFO)
newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries)
mtext("Spring season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries)
plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries)
# summer season
newINFO<-setPAx(6,3,localINFO=localINFO)
newAnnualSeries<-makeAnnualSeries(localINFO=newINFO)
plot1of15(yearStart,yearEnd,qf,istat=2,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
mtext("Summer season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
plot1of15(yearStart,yearEnd,qf,istat=5,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
plot1of15(yearStart,yearEnd,qf,istat=8,localAnnualSeries=newAnnualSeries,isBottom=TRUE)
caption<-paste("Streamflow statistics (circles) in units of millimeters per day, annual values and seasonal values\nFall (Sept., Oct., and Nov.), Winter (Dec., Jan., and Feb.), Spring (Mar., Apr., and May), and Summer (June, July, and Aug.)\nand locally weighted scatterplot smooth (solid curve) for ",localINFO$shortName," for ",yearStart," - ",yearEnd,".",sep="")
mtext(caption,side=1,outer=TRUE,line=7,adj=0,font=1,cex=0.7)
dev.off()
}
#
#
#' Plot of the discharge time series
#'
#' Part of flowHistory component.
#' Allows discharge record to only show those discharges above a given threshold
#'
#' @param startYear numeric indicating the starting year for the graph
#' @param endYear numeric indicating the ending year for the graph (should be a time in decimal years that is after the last observations to be plotted)
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qLower numeric specifying the lower bound on discharges that are to be plotted, must be in the units specified by qUnit, default is NA (lower bound is zero)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.  Default is qUnit=1 (cubic feet per second)
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @keywords graphics streamflow
#' @export
#' @examples
#' plotQTimeDaily(1990,2000,localDaily=exDailyStart,localINFO=exINFOStart,qLower=10)
plotQTimeDaily<-function (startYear, endYear, localDaily = Daily, 
    localINFO = INFO, qLower = NA, qUnit = 1, tinyPlot = FALSE, printTitle = TRUE)    
{
#########################################################
       if (is.numeric(qUnit)) {
        qUnit <- qConst[shortCode = qUnit][[1]]
    }
    else if (is.character(qUnit)) {
        qUnit <- qConst[qUnit][[1]]
    }
#############################################################
	qFactor<-qUnit@qUnitFactor
    if (tinyPlot) 
        par(mar = c(5, 4, 1, 1))
    else par(mar = c(5, 4, 4, 2) + 0.1)
    subDaily <- subset(localDaily, DecYear >= startYear)
    subDaily <- subset(subDaily, DecYear <= endYear)
    xDaily <- subDaily$DecYear
    xLimits <- c(startYear, endYear)
    xTicks <- pretty(xLimits, n = 9)
    numXTicks <- length(xTicks)
    xLeft <- xTicks[1]
    xRight <- xTicks[numXTicks]
    yDaily <- qFactor * subDaily$Q
    yMin <- if(is.na(qLower)) 0 else qLower
    yMax <- 1.05*max(yDaily)
    ySpan <- c(yMin,yMax)
    yTicks <- pretty(ySpan,8)
    nYTicks <- length(yTicks)
    yTop <- yTicks[nYTicks]
    yBottom <- yTicks[1]
    line2 <- if(is.na(qLower)) "Daily Discharge" else paste("Daily discharge above a threshold of\n",qLower," ",qUnit@qUnitName,sep="")
    line1 <- localINFO$shortName
    plotTitle <- if (printTitle) 
        paste(line1, "\n", line2)
    else ""
    yLab <- qUnit@qUnitName
    qBottom <- if(is.na(qLower)) 0 else qLower
    plot(xDaily, yDaily, axes = FALSE, xlim = c(startYear, endYear), 
        xaxs = "i", xlab = "", ylim = c(qBottom, yTop), yaxs = "i", 
        ylab = yLab, main = plotTitle, type = "l", lwd = 3, col="red", 
        cex = 0.7, cex.main = 1.3, font.main = 2, cex.lab = 1.2)
    axis(1, tcl = 0.5, at = xTicks, labels = xTicks)
    axis(2, tcl = 0.5, las = 1, at = yTicks)
    axis(3, tcl = 0.5, at = xTicks, labels = FALSE)
    axis(4, tcl = 0.5, at = yTicks, labels = FALSE)
    box()
    par(mar = c(5, 4, 4, 2) + 0.1)
}





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





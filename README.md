`EGRET`
=============

Linix: [![travis](https://travis-ci.org/USGS-R/EGRET.svg?branch=master)](https://travis-ci.org/USGS-R/EGRET)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/a2kogyfplo3valdg?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/egret)

Exploration and Graphics for RivEr Trends (`EGRET`): 
An R-package for the analysis of long-term changes in water quality and streamflow, 
including the water-quality method Weighted Regressions on Time, Discharge, and Season (WRTDS)

Evaluating long-term changes in river conditions (water quality and discharge) is an important use of hydrologic data. To carry out such evaluations, the hydrologist needs tools to facilitate several key steps in the process: acquiring the data records from a variety of sources, structuring it in ways that facilitate the analysis, routines that will process the data to extract information about changes that may be happening, and graphical techniques that can display findings about change. A pair of tightly linked R packages, called `EGRET` and `EGRET` (Exploration and Graphics for RivEr Trends), have been developed for carrying out each of these steps in an integrated manner. They are designed to accept easily data from three sources: U.S. Geological Survey hydrologic data, Water Quality Portal Data (currently including U.S. Environmental Protection Agency (EPA) STORET data, and USDA STEWARDS data), and user-supplied flat files. The `EGRET` package not only serves as a "front end" to the `EGRET` package, it can also be used to easily download many types of hydrologic data and organize it in ways that facilitate many other hydrologic applications. The `EGRET` package has components oriented towards the description of long-term changes in streamflow statistics (high flow, average flow, and low flow) as well as changes in water quality. For the water-quality analysis, it uses Weighted Regressions on Time, Discharge and Season (WRTDS) to describe long-term trends in both concentration and flux. `EGRET` also creates a wide range of graphical presentations of the water-quality data and of the WRTDS results. This report serves as a user guide to these two R packages, providing detailed guidance on installation and use of the software, documentation of the analysis methods used, as well as guidance on some of the kinds of questions and approaches that the software can facilitate.

The link for the official USGS publication user guide is here:

[http://pubs.usgs.gov/tm/04/a10/](http://pubs.usgs.gov/tm/04/a10/)

Please visit the wiki for more information:
[EGRET Wiki](https://github.com/USGS-R/EGRET/wiki)

An recent presentation on an overview of `EGRET` and `EGRET` can be found here:
[EGRET and EGRET 2014-10-07.pdf](https://github.com/USGS-R/EGRET/blob/gh-pages/images/EGRET%20and%20EGRET%202014-10-07.pdf?raw=true)

Subscribe
---------
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.

Package Installation
---------------------------------

To install the EGRET and EGRET packages you need to be using R 3.0 or greater. Then use the following command:

	install.packages(c("EGRET","EGRET"), 
	     repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
	     dependencies=TRUE)



Background Information
----------------------

WRTDS is a method of analysis for long-term surface water quality data to evaluate trends and average concentrations and fluxes. It is intended for use with data sets of more than about 200 observations of water quality over a time span of about 20 years or more, but can be used with somewhat shorter data sets. There also needs to be a daily time series of streamflow data covering the entire period of the water quality data collection. The method can be used with smaller data sets, but some adjustments by the user are needed to make it work with less than 100 water quality observations. The best way to learn about the WRTDS approach and to see examples of its application to multiple large data sets is to read two journal articles. Both are available, for free, from the journals in which they were published.

The first relates to nitrate and total phosphorus data for 9 rivers draining to Chesapeake Bay:

[Chesapeake Bay](http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.2010.00482.x/full)

The second is an application to nitrate data for 8 monitoring sites on the Mississippi River or its major tributaries:

[Mississippi River](http://pubs.acs.org/doi/abs/10.1021/es201221s)

The manual assumes that the user understands the concepts underlying WRTDS.  Thus, reading at least the first of these papers is necessary to understanding the manual.


Sample Workflow
---------------

Load data from web services:

	library(EGRET)
	Daily <- readNWISDaily("06934500","00060","1979-10-01","2010-09-30")
	Sample <-readNWISSample("06934500","00631","1970-10-01","2011-09-30")
	INFO <-readNWISInfo("06934500","00631", interactive=FALSE)
	eList <-mergeReport(Info, Daily, Sample)

This is a sample workflow for using WRTDS on the Choptank River at Greensboro MD, for Nitrate:

	library(EGRET)
	
	############################
	# Gather discharge data:
	siteID <- "01491000" #Choptank River at Greensboro, MD
	startDate <- "" #Gets earliest date
	endDate <- "2011-09-30"
	# Gather sample data:
	parameter_cd<-"00631" #5 digit USGS code
	Sample <- readNWISSample(siteID,parameter_cd,startDate,endDate)
	#Gets earliest date from Sample record:
	#This is just one of many ways to assure the Daily record
	#spans the Sample record
	startDate <- min(as.character(Sample$Date)) 
	# Gather discharge data:
	Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
	# Gather site and parameter information:
	
	# Here user must input some values for
	# the default (interactive=TRUE)
	INFO<- readNWISInfo(siteID,parameter_cd)
	INFO$shortName <- "Choptank River at Greensboro, MD"
	
	# Merge discharge with sample data:
	eList <- mergeReport(INFO, Daily, Sample)
	############################
	
	############################
	# Check sample data:
	boxConcMonth(eList)
	boxQTwice(eList)
	plotConcTime(eList)
	plotConcQ(eList)
	multiPlotDataOverview(eList)
	############################
	
	############################
	# Run WRTDS model:
	eList <- modelEstimation(eList)
	############################
	
	############################
	#Check model results:
	
	#eList:
	plotConcTimeDaily(eList)
	plotFluxTimeDaily(eList)
	plotConcPred(eList)
	plotFluxPred(eList)
	plotResidPred(eList)
	plotResidQ(eList)
	plotResidTime(eList)
	boxResidMonth(eList)
	boxConcThree(eList)
	
	#Require Daily + INFO:
	plotConcHist(eList)
	plotFluxHist(eList)
	
	# Multi-line plots:
	date1 <- "2000-09-01"
	date2 <- "2005-09-01"
	date3 <- "2009-09-01"
	qBottom<-100
	qTop<-5000
	plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop, 
	                   concMax=2,qUnit=1)
	q1 <- 10
	q2 <- 25
	q3 <- 75
	centerDate <- "07-01"
	yearEnd <- 2009
	yearStart <- 2000
	plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd)
	
	# Multi-plots:
	fluxBiasMulti(eList)
	
	#Contour plots:
	clevel<-seq(0,2,0.5)
	maxDiff<-0.8
	yearStart <- 2000
	yearEnd <- 2010
	
	plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
	             contourLevels = clevel,qUnit=1)
	plotDiffContours(eList, yearStart,yearEnd,
	                 qBottom,qTop,maxDiff,qUnit=1)
	# modify this for your own computer file structure
	savePath<-"/Users/rhirsch/Desktop/" 
	saveResults(savePath)

This is a sample workflow for a flowHistory application for the entire record.

	library(EGRET)
	
	# Flow history analysis
	############################
	# Gather discharge data:
	siteID <- "01491000" #Choptank River at Greensboro, MD
	startDate <- "" # Get earliest date
	endDate <- "" # Get latest date
	Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
	# Gather site and parameter information:
	# Here user must input some values for
	# the default (interactive=TRUE)
	INFO<- readNWISInfo(siteID,"00060")
	INFO$shortName <- "Choptank River at Greensboro, MD"
	eList <- as.egret(INFO, Daily, NA, NA)
	############################
	
	############################
	# Check flow history data:
	annualSeries <- makeAnnualSeries()
	plotFlowSingle(eList, istat=7,qUnit="thousandCfs")
	plotSDLogQ(eList)
	plotQTimeDaily(eList, qLower=1,qUnit=3)
	plotFour(eList, qUnit=3)
	plotFourStats(eList, qUnit=3)
	############################

	# modify this for your own computer file structure:
	savePath<-"/Users/rhirsch/Desktop/" 
	saveResults(savePath, eList)



Version updates
---------------

###EGRET 1.4.0
* EGRET specific data retrieval functions moved from dataRetrieval to EGRET
* eList - a named list of INFO, Daily, Sample, and surfaces is now used as the input to functions.

###EGRET 1.3.0

* July 31, 2014
* Modified the smoothing algorithm in flowHistory and WRTDS to reduce "edge effects".  New smoother is the default, but original method is still available (see User Guide for details). The modified method tends to reduce curvature near the start and end of record.  It has no effect during the middle years of the data set.


###EGRET 1.2.5 
* March 14, 2014
* Added period of analysis control to all graphs and tables (with exception of plotContours, plotDiffContours, plotConcQSmooth, and plotConcTimeSmooth).  This allows these outputs to be limited to specific months of the year as determined by the user.
* Changed default color palette for contour plots
* Updated vignette.
* Added setupYears call within functions to generally eliminate the need for AnnualResults.
* Updated calculateMonthlyResults to give month, year, and decimal year.
* Continued to improve documentation.

* Version 1.2.4 July 10, 2013

* Version 1.2.3 February 21, 2013

* Version 1.2.1 June 8, 2012

* Version 1.1.3	April 26, 2012

* Version 1.0.0	March 16, 2012

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


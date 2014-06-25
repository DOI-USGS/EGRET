Exploration and Graphics for RivEr Trends (EGRET)
=============

Exploration and Graphics for RivEr Trends (EGRET): 
An R-package for the analysis of long-term changes in water quality and streamflow, 
including the water-quality method Weighted Regressions on Time, Discharge, and Season (WRTDS)

Overview of EGRET:  The following are 4 major features of EGRET.

1.  It is designed to obtain its water quality sample data, streamflow data, and metadata directly from the USGS NWIS (National Water Information System), but it allows for user-supplied text files as inputs.  The program is designed to ingest the data directly into R and structure them into file structures suited to the analysis.  For those familiar with WRTDS_4c, the text file inputs used in that system will also work in EGRET.

2.  It has all of the existing WRTDS functionality - computing concentrations, fluxes, flow normalized versions of those, trends in those and graphics to show results and to explore the behavior of the data (by season, by flow class...). Many graph and table outputs are possible and all are clearly labeled and suitable for presentation or publication.  It is designed for both batch and interactive processing.  It is very much oriented to graphics and should be thought of as an exploratory tool.  It is intended for use with data sets of about 200 or more samples, over a time period of about 20 or more years.  Some testing with smaller data sets has been done, and no significant problems have been identified in cases with sample sizes slightly larger than 100 but extensive testing with smaller data sets has not taken place yet.

3. It has additional statistics and graphics to help evaluate the possibility that flux estimates may be biased (it is known that in certain cases, regression-based methods can produce severely biased flux estimates).  It can also accept results from other estimation methods like LOADEST and produce the same types of graphics and statistics for them (this part is not yet documented).

4. It has a streamflow history component, not related to water quality, that is not a part of WRTDS, but uses some similar concepts and shares some of the basic software and data structures.  This component, called flowHistory provides a variety of table and graphical outputs looking only at flow statistics (like annual mean, annual 7-day low flow, annual 1-day maximum, or seasonal versions of these) all based on time-series smoothing.  It is designed to be used in long-term studies of streamflow change (associated with climate or land use or water use change) and works best for daily streamflow data sets of 50 years or longer.   It is put together with the WRTDS method because it uses the same data retrieval infrastructure as WRTDS and the same data structure.  

Please visit the wiki for more information:
[EGRET Wiki](https://github.com/USGS-R/EGRET/wiki)

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This software is provided "AS IS".

Subscribe
---------
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.

Package Installation
---------------------------------

To install the EGRET and dataRetrieval packages you need to be using R 3.0 or greater. Then use the following command:

	install.packages(c("dataRetrieval","EGRET"), 
	repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
	dependencies=TRUE,
	type="both")



Background Information
----------------------

WRTDS is a method of analysis for long-term surface water quality data.  It is intended for use with data sets of more than about 200 observations of water quality over a time span of about 20 years or more.  There also needs to be a daily time series of streamflow data covering the entire period of the water quality data collection.  The method can be used with somewhat smaller data sets, but it will not work with less than 100 water quality observations.  The best way to learn about the WRTDS approach and to see examples of its application to multiple large data sets is to read two journal articles.  Both are available, for free, from the journals in which they were published.

The first relates to nitrate and total phosphorus data for 9 rivers draining to Chesapeake Bay:

[Chesapeake Bay](http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.2010.00482.x/full)

The second is an application to nitrate data for 8 monitoring sites on the Mississippi River or its major tributaries:

[Mississippi River](http://pubs.acs.org/doi/abs/10.1021/es201221s)

The manual assumes that the user understands the concepts underlying WRTDS.  Thus, reading at least the first of these papers is necessary to understanding the manual.


* [EGRET User Guide](https://github.com/USGS-R/EGRET/raw/master/inst/doc/draft+user+guide+for+EGRET+and+dataRetrieval+2014-04-14.pdf) (pdf)

* [EGRET vignette](https://github.com/USGS-R/EGRET/raw/master/inst/doc/EGRET.pdf) (pdf)

* [dataRetrieval User Guide](https://github.com/USGS-R/dataRetrieval/raw/master/inst/doc/dataRetrieval.pdf) (pdf)

* [WRTDS Water Quality Seminar July 2013](https://github.com/USGS-R/EGRET/raw/Documentation/WRTDS OWQ 10July2013.pdf) (pdf)

* [WRTDS NWQMC course slides 2012](https://github.com/USGS-R/EGRET/raw/Documentation/WRTDS+NWQMC+course.pdf) (pdf)

* [EGRET flowHistory presentation 2012](https://github.com/USGS-R/EGRET/raw/Documentation/EGRET+flowHistory+presentation.pdf) (pdf)


Version updates
---------------

####EGRET

* Version 1.2.5 March 14, 2014

	* Added period of record control to all graphs and tables (with execption of plotContours, plotDiffContours, plotConcQSmooth, and plotConcTimeSmooth).
	* Changed default color palette for contour plots
	* Updated vignette.
	* Added setupYears call within functions to generally eliminate the need for AnnualResults.
	* Updated calculateMonthlyResults to give month, year, and decimal year.
	* Continued to improve documentation.

* Version 1.2.4 July 10, 2013

	* Fixed a small leap year bug
	* Improved ability to modify graphic functions
	* Added dataframe returns to table functions

* Version 1.2.3 February 21, 2013

	* New estDailyFromSurfaces function utilizing "fields" package, written by Jeffrey Chanat. Greatly increases the speed of computations of the daily results. Two additional R packages are needed: fields and spam
	* Improved documentation, especially example functions.
	* runSurvReg modified to include the option of a very wide seasonal window (windowS), which has the effect of eliminating the influence of the seasonal weights.
	* Progress indicators modified to be more informative.
	* Fixed a calculation bug in function plotLogFluxPred.

* Version 1.2.1 June 8, 2012
 
	* Differs only from 1.1.3 in two respects.  It adds four new functions to the flowHistory capability and makes one bug fix to WRTDS.  The four new flow History functions are: plot15 (makes an array of 15 plots for a given site - a matrix of 3 flow statistics (7-day min, mean, and 1-day max) versus 5 periods of analysis (annual, fall, winter, spring, and summer), plotSDLogQ (makes a plot of the running standard deviation of the Log Discharge versus time), plotFour (makes a set of four graphs: 1-day max, mean, 7-day min, and running standard deviation of the logs), and plotQTimeDaily (a way to plot discharge versus time, setup particularly to show flows above some threshold discharge).  The bug fix in WRTDS made no changes to calculations, it just prevents a crash in a situation where the user has shortened the length of the daily record.

* Version 1.1.3	April 26, 2012
	
	* Differs only from the March 16, 2012 version 1.0.0 in a few small bug fixes and cosmetic changes in some graphics, improved saveResults function, and better formatting of help pages for the functions.


* Version 1.0.0	March 16, 2012

	* Contains the code for all WRTDS and flowHistory analysis, plus graph and tabular output.


### dataRetrieval

* Version 1.2.2:        July 10, 2013

	* Added getDataAvailability function to find measured parameters and period of record information for a requested station.

	* Added constructNWISURL function to get the URL that is used to retrieve the data.

	* Added getSampleSTORET function to get STORET data directly in Sample dataframe form.

	* Fixed a small leap year bug by changing day of year by making Feb. 29 always 59, and March 1st always 60 (even in non-leap years).

* Version 1.0.5:	June 27, 2012

	* Fixed a bug that caused problems if not explicitly defining Daily and Sample in mergeReport().

* Version 1.0.4:	June 19, 2012

	* Censored QW data retrieval bug fixed

* Version 1.0.3:	May 30, 2012

	* R 2.15.0 bug fixed

* Version 1.0.2:	May 24, 2012

	* Sample data included modified to match the data in the EGRET package
	* Bug fixed in populateData function.  

* Version 1.0:	Feburary 23, 2012

	* First full package for retrieving data for use in EGRET.  Includes data entry from USGS web services for water quality sample data, daily streamflow data, and metadata.



Sample Workflow
---------------

Load data from web services:

	library(dataRetrieval)
	Daily <- getDVData("06934500","00060","1979-10-01","2010-09-30")
	Sample <-getSampleData("06934500","00631","1970-10-01","2011-09-30")
	INFO <-getMetaData("06934500","00631", interactive=FALSE)
	Sample <-mergeReport(Daily, Sample)

This is a sample workflow for using WRTDS on the Choptank River at Greensboro MD, for Nitrate:

	library(dataRetrieval)
	library(EGRET)
	sta<-"01491000"
	param<-"00631"
	StartDate<-"1979-09-01"
	EndDate<-"2011-09-30"
	Sample<-getSampleData(sta,param,StartDate,EndDate)
	summary(Sample)
	length(Sample$Date)
	Sample<-removeDuplicates(Sample)
	length(Sample$Date)
	Daily<-getDVData(sta,"00060",StartDate,EndDate)
	summary(Daily)
	Sample<-mergeReport()
	INFO<-getMetaData(sta,param)
	INFO
	multiPlotDataOverview()
	modelEstimation()
	AnnualResults<-setupYears()
	plotConcHist(1975,2012)
	plotFluxHist(1975,2012,fluxUnit=5)
	tableResults(qUnit=1,fluxUnit=5)
	tableChange(fluxUnit=5,yearPoints=c(1980,1995,2011))
	fluxBiasMulti(qUnit=1,fluxUnit=1,moreTitle="WRTDS")
	plotFluxTimeDaily(1998,2005)
	plotFluxTimeDaily(2010,2011.75)
	plotContours(1980,2012,5,1000,qUnit=1,contourLevels=seq(0,2,0.25))
	plotDiffContours(1980,2011,5,1000,1,qUnit=1)
	plotConcQSmooth("1984-04-01","1995-04-01","2008-04-01",30,2000,qUnit=1)
	plotContours(1980,2012,5,1000,qUnit=1,whatSurface=2)
	plotResidPred()
	plotResidQ()
	savePath<-"/Users/rhirsch/Desktop/" #### modify this for your own computer file structure ####
	saveResults(savePath)

This is a sample workflow for a flowHistory application for the Mississippi River at Keokuk, Iowa for the entire record.

	library(dataRetrieval)
	library(EGRET)
	sta<-"05474500"
	param<-"00060"
	Daily<-getDVData(sta,param,"","")
	summary(Daily)
	INFO<-getMetaData(sta,param)
	annualSeries<-makeAnnualSeries()
	plotFlowSingle(istat=5,qUnit=4)
	printSeries(istat=5,qUnit=4)
	tableFlowChange(istat=5,yearPoints=c(1880,1930,1970,2010))
	plotFourStats(qUnit=4)
	INFO<-setPA(paStart=12,paLong=3)
	annualSeries<-makeAnnualSeries()
	plotFlowSingle(istat=1,qUnit=4)
	plotFlowSingle(istat=7,qUnit=4)
	tableFlowChange(istat=5,yearPoints=c(1880,1930,1970,2010))
	plotFourStats(qUnit=4)
	savePath<-"/Users/rhirsch/Desktop/" #### modify this for your own computer file structure ####
	saveResults(savePath)


 

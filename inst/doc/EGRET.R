## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, concordance=TRUE,tidy=FALSE,comment="")

knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

bold.colHeaders <- function(x) {
  x <- gsub("\\^(\\d)","$\\^\\1$",x)
  x <- gsub("\\%","\\\\%",x)
  x <- gsub("\\_"," ",x)
  returnX <- paste("\\multicolumn{1}{c}{\\textbf{\\textsf{", x, "}}}", sep = "")
}

addSpace <- function(x) ifelse(x != "1", "[5pt]","")


## ----workflowFlowHistory, echo=TRUE,eval=FALSE------------
#  library(dataRetrieval)
#  library(EGRETdemo)
#  
#  # Flow history analysis
#  
#  ############################
#  # Gather discharge data:
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "" # Get earliest date
#  endDate <- "" # Get latest date
#  Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
#  # Gather site and parameter information:
#  # Here user must input some values for
#  # the default (interactive=TRUE)
#  INFO<- readNWISInfo(siteNumber,"00060")
#  INFO$shortName <- "Choptank River at Greensboro, MD"
#  ############################
#  
#  ############################
#  # Check flow history data:
#  eList <- as.egret(INFO, Daily, NA, NA)
#  plotFlowSingle(eList, istat=7,qUnit="thousandCfs")
#  plotSDLogQ(eList)
#  plotQTimeDaily(eList, qLower=1,qUnit=3)
#  plotFour(eList, qUnit=3)
#  plotFourStats(eList, qUnit=3)
#  ############################
#  
#  # modify this for your own computer file structure:
#  savePath<-"/Users/rhirsch/Desktop/"
#  saveResults(savePath, INFO)
#  

## ----workflowWaterQuality, echo=TRUE,eval=FALSE-----------
#  library(dataRetrieval)
#  library(EGRETdemo)
#  
#  ############################
#  # Gather discharge data:
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "" #Gets earliest date
#  endDate <- "2011-09-30"
#  # Gather sample data:
#  parameter_cd<-"00631" #5 digit USGS code
#  Sample <- readNWISSample(siteNumber,parameter_cd,startDate,endDate)
#  #Gets earliest date from Sample record:
#  #This is just one of many ways to assure the Daily record
#  #spans the Sample record
#  startDate <- min(as.character(Sample$Date))
#  # Gather discharge data:
#  Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
#  # Gather site and parameter information:
#  
#  # Here user must input some values for
#  # the default (interactive=TRUE)
#  INFO<- readNWISInfo(siteNumber,parameter_cd, interactive=FALSE)
#  INFO$shortName <- "Choptank River at Greensboro, MD"
#  
#  # Merge discharge with sample data:
#  eList <- mergeReport(INFO, Daily, Sample)
#  ############################
#  
#  ############################
#  # Check sample data:
#  boxConcMonth(eList)
#  boxQTwice(eList)
#  plotConcTime(eList)
#  plotConcQ(eList)
#  multiPlotDataOverview(eList)
#  ############################
#  
#  ############################
#  # Run WRTDS model:
#  eList <- modelEstimation(eList)
#  ############################
#  
#  ############################
#  #Check model results:
#  
#  #Require Sample + INFO:
#  plotConcTimeDaily(eList)
#  plotFluxTimeDaily(eList)
#  plotConcPred(eList)
#  plotFluxPred(eList)
#  plotResidPred(eList)
#  plotResidQ(eList)
#  plotResidTime(eList)
#  boxResidMonth(eList)
#  boxConcThree(eList)
#  
#  #Require Daily + INFO:
#  plotConcHist(eList)
#  plotFluxHist(eList)
#  
#  # Multi-line plots:
#  date1 <- "2000-09-01"
#  date2 <- "2005-09-01"
#  date3 <- "2009-09-01"
#  qBottom<-100
#  qTop<-5000
#  plotConcQSmooth(date1, date2, date3, qBottom, qTop,
#                     concMax=2,qUnit=1)
#  q1 <- 10
#  q2 <- 25
#  q3 <- 75
#  centerDate <- "07-01"
#  yearEnd <- 2009
#  yearStart <- 2000
#  plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd)
#  
#  # Multi-plots:
#  fluxBiasMulti(eList)
#  
#  #Contour plots:
#  clevel<-seq(0,2,0.5)
#  maxDiff<-0.8
#  yearStart <- 2000
#  yearEnd <- 2010
#  
#  plotContours(eList, yearStart,yearEnd,qBottom,qTop,
#               contourLevels = clevel,qUnit=1)
#  plotDiffContours(eList, yearStart,yearEnd,
#                   qBottom,qTop,maxDiff,qUnit=1)
#  
#  # modify this for your own computer file structure:
#  savePath<-"/Users/rhirsch/Desktop/"
#  saveResults(savePath, INFO)

## ----openlibraries, echo=TRUE,eval=TRUE-------------------
library(EGRETdemo)

## ----ThirdExample-----------------------------------------
parameterCd <- "00618"
siteNumber <- "01491000"
INFO <- readNWISInfo(siteNumber,parameterCd, interactive=FALSE)

## ----WQPInfo, eval=FALSE----------------------------------
#  parameterCd <- "00618"
#  INFO_WQP <- readWQPInfo("USGS-01491000",parameterCd)

## ----addInfo, eval=FALSE, echo=TRUE-----------------------
#  
#  INFO$riverInfo <- "Major tributary of the Chesapeake Bay"
#  INFO$GreensboroPopulation <- 1931
#  

## ----firstExample-----------------------------------------
siteNumber <- "01491000"
startDate <- "2000-01-01"
endDate <- "2013-01-01"
# This call will get NWIS (ft3/s) data , and convert it to m3/s:
Daily <- readNWISDaily(siteNumber, "00060", startDate, endDate)

## ----colNamesDaily, echo=FALSE,results='asis'-------------
ColumnName <- c("Date", "Q", "Julian","Month","Day","DecYear","MonthSeq","Qualifier","i","LogQ","Q7","Q30")
Type <- c("Date", "number", "number","integer","integer","number","integer","string","integer","number","number","number")
Description <- c("Date", "Discharge in m$^3$/s", "Number of days since January 1, 1850", "Month of the year [1-12]", "Day of the year [1-366]", "Decimal year", "Number of months since January 1, 1850", "Qualifying code", "Index of days, starting with 1", "Natural logarithm of Q", "7 day running average of Q", "30 day running average of Q")
Units <- c("date", "m$^3$/s","days", "months","days","years","months", "character","days","numeric","m$^3$/s","m$^3$/s")

DF <- data.frame(ColumnName,Type,Description,Units)

print(xtable(DF, caption="Daily dataframe",label="tab:DailyDF1"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
       
      )


## ----secondExample----------------------------------------
parameterCd <- "00618"
Sample <-readNWISSample(siteNumber,parameterCd,
      startDate, endDate)

## ----STORET,echo=TRUE,eval=FALSE--------------------------
#  site <- 'WIDNR_WQX-10032762'
#  characteristicName <- 'Specific conductance'
#  Sample <-readWQPSample(site,characteristicName,
#        startDate, endDate)

## ----label=tab:exampleComplexQW, echo=FALSE, eval=TRUE,results='asis'----
cdate <- c("2003-02-15","2003-06-30","2004-09-15","2005-01-30","2005-05-30","2005-10-30")
rdp <- c("", "<","<","","","")
dp <- c(0.02,0.01,0.005,NA,NA,NA)
rpp <- c("", "","<","","","")
pp <- c(0.5,0.3,0.2,NA,NA,NA)
rtp <- c("","","","","<","<")
tp <- c(NA,NA,NA,0.43,0.05,0.02)

DF <- data.frame(cdate,rdp,dp,rpp,pp,rtp,tp,stringsAsFactors=FALSE)

xTab <- xtable(DF, caption="Example data",digits=c(0,0,0,3,0,3,0,3),label="tab:exampleComplexQW")

print(xTab,
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----thirdExample,echo=FALSE------------------------------
  compressedData <- compressData(DF)
  Sample <- populateSampleColumns(compressedData)

## ----thirdExampleView,echo=TRUE---------------------------
  Sample

## ----openDaily, eval = FALSE------------------------------
#  fileName <- "ChoptankRiverFlow.txt"
#  filePath <-  "C:/RData/"
#  Daily <-readDataFromFile(filePath,fileName,
#                      separator="\t")

## ----openSample, eval = FALSE-----------------------------
#  fileName <- "ChoptankRiverNitrate.csv"
#  filePath <-  "C:/RData/"
#  Sample <-readUserSample(filePath,fileName,
#                                  separator=",")

## ----openSample2, eval = FALSE----------------------------
#  fileName <- "ChoptankPhosphorus.txt"
#  filePath <-  "C:/RData/"
#  Sample <-readUserSample(filePath,fileName,
#                                  separator="\t")

## ----mergeExample-----------------------------------------
siteNumber <- "01491000"
parameterCd <- "00631"  # Nitrate
startDate <- "2000-01-01"
endDate <- "2013-01-01"

Daily <- readNWISDaily(siteNumber, "00060", startDate, endDate)
Sample <- readNWISSample(siteNumber,parameterCd, startDate, endDate)
INFO <- readNWISInfo(siteNumber, parameterCd, interactive=FALSE)
eList <- mergeReport(INFO, Daily,Sample)
# eList

## ----cheatSheets,echo=TRUE,eval=TRUE,results='markup'-----
printqUnitCheatSheet()

## ----cheatSheets2,echo=TRUE,eval=TRUE,results='markup'----
printFluxUnitCheatSheet()

## ----vignette1, eval=FALSE, echo=TRUE---------------------
#  vignette("dataRetrieval")

## ----flowHistory,echo=TRUE,eval=TRUE----------------------
siteNumber <- "14105700"  
startDate <- ""
endDate <- ""

Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Columbia River at The Dalles, OR"

eList <- as.egret(INFO, Daily, NA, NA)


## ----newChunckWinter, echo=TRUE,eval=FALSE----------------
#  eList <- setPA(eList,paStart=12,paLong=3)

## ----newChunck, echo=TRUE,eval=TRUE-----------------------
eList <- setPA(eList)

## ----newChunckAS, echo=TRUE,eval=TRUE---------------------
annualSeries <- makeAnnualSeries(eList)

## ----plotSingleandSD, echo=TRUE, fig.cap="Plots of discharge statistics",fig.subcap=c("plotFlowSingle(istat=5,qUnit='thousandCfs')","plotSDLogQ(eList)"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotFlowSingle(eList, istat=5,qUnit="thousandCfs")
plotSDLogQ(eList)

## ----Merced, echo=TRUE,eval=TRUE,fig.cap="Merced River Winter Trend",fig.subcap=c("Water Year", "December - February"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
siteNumber<-"11264500"
Daily <-readNWISDaily(siteNumber,"00060",startDate="",endDate="")
INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Merced River at Happy Isles Bridge, CA"
eListMerced <- as.egret(INFO, Daily, NA, NA)
eListMerced <- setPA(eListMerced)
plotFlowSingle(eListMerced, istat=5)

# Then, we can run the same function, but first set 
# the pa to start in December and only run for 3 months.

eListMerced <- setPA(eListMerced,paStart=12,paLong=3)
plotFlowSingle(eListMerced,istat=5,qMax=200)


## ----plotFour, echo=TRUE, fig.cap="\\texttt{plotFour(qUnit=3)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotFour(eListMerced, qUnit=3)

## ----plotFourStats,echo=TRUE, fig.cap="\\texttt{plotFourStats(qUnit=3)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotFourStats(eListMerced, qUnit=3)

## ----Mississippi, echo=TRUE,eval=TRUE,fig.cap="Mississippi River at Keokuk Iowa",fig.subcap=c("Water Year", "Dec-Feb"),out.width='1\\linewidth',out.height='1\\linewidth',fig.show='hold',fig.pos="h"----
siteNumber<-"05474500"
Daily <-readNWISDaily(siteNumber,"00060",startDate="",endDate="")
INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Mississippi River at Keokuk Iowa"
eListMiss <- as.egret(INFO, Daily, NA, NA)

plotQTimeDaily(eListMiss, qUnit=3,qLower=300)


## ----printSeries, eval=FALSE,echo=TRUE--------------------
#  seriesResult <- printSeries(eListMiss, istat=3, qUnit=3)

## ----tfc, eval=TRUE,echo=TRUE-----------------------------
tableFlowChange(eListMiss, istat=3, qUnit=3,yearPoints=c(1890,1950,2010))

## ----wrtds1,eval=FALSE,echo=TRUE--------------------------
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "1979-10-01"
#  endDate <- "2011-09-30"
#  param<-"00631"
#  Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
#  INFO<- readNWISInfo(siteNumber,param,interactive=FALSE)
#  INFO$shortName <- "Choptank River"
#  
#  Sample <- readNWISSample(siteNumber,param,startDate,endDate)
#  eList <- mergeReport(INFO, Daily, Sample)

## ----wrtds2,eval=TRUE,echo=FALSE--------------------------
siteNumber <- "01491000" #Choptank River at Greensboro, MD
startDate <- "1979-10-01"
endDate <- "2011-09-30"
param<-"00631"
Daily <- ChopDaily
Sample <- ChopSample
INFO <- ChopINFO
eList <- Choptank_eList

## ----plotBoxes, echo=TRUE, fig.cap="Concentration box plots",fig.subcap=c("\\texttt{boxConcMonth(eList)}","\\texttt{boxQTwice(eList, qUnit=1)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
boxConcMonth(eList)
boxQTwice(eList,qUnit=1)

## ----plotConcTime,echo=TRUE, fig.cap="The relation of concentration vs time or discharge",fig.subcap=c("\\texttt{plotConcTime(eList)}","\\texttt{plotConcQ(eList)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcTime(eList)
plotConcQ(eList, qUnit=1)

## ----plotFluxQ,echo=TRUE, fig.cap="The relation of flux vs discharge",out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotFluxQ(eList, fluxUnit=4)

## ----multiPlotDataOverview, echo=TRUE, fig.cap="\\texttt{multiPlotDataOverview(qUnit=1)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
multiPlotDataOverview(eList, qUnit=1)

## ----flowDuration, eval=TRUE, echo=TRUE-------------------
flowDuration(eList, qUnit=1)

flowDuration(eList, qUnit=1, centerDate="09-30", span=30)

## ----wrtds3, eval=FALSE, echo=TRUE------------------------
#  eList <- modelEstimation(eList)

## ----wrtds5, eval=FALSE, echo=TRUE------------------------
#  savePath <- "C:/Users/egretUser/WRTDS_Output/" #An example directory name
#  saveResults(savePath, INFO)

## ----wrtds8, eval=FALSE, echo=TRUE------------------------
#  loadPath <- "C:/Users/egretUser/WRTDS_Output/"
#  staAbbrev <- "Chop"
#  constitAbbrev <- "NO3"
#  pathToFile <- paste(loadPath,staAbbrev,".",
#                      constitAbbrev,".RData",sep="")
#  load(pathToFile)

## ----getChopData1,echo=FALSE,eval=TRUE--------------------
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
surfaces <- exsurfaces
eList <- Choptank_eList

## ----plotConcTimeDaily, echo=TRUE, fig.cap="Concentration and flux vs time",fig.subcap=c("\\texttt{plotConcTimeDaily(2008, 2010)}","\\texttt{plotFluxTimeDaily(2008, 2010)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
# Return to water year:
eList <- setPA(eList)

yearStart <- 2008
yearEnd <- 2010

plotConcTimeDaily(eList, yearStart, yearEnd)
plotFluxTimeDaily(eList, yearStart, yearEnd)

## ----plotFluxPred, echo=TRUE, fig.cap="Concentration and flux predictions",fig.subcap=c('\\texttt{plotConcPred(eList)}','\\texttt{plotFluxPred(eList)}'),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcPred(eList)
plotFluxPred(eList)

## ----plotResidQ, echo=TRUE, fig.cap="Residuals",fig.subcap=c("\\texttt{plotResidPred(eList)}","\\texttt{plotResidQ(eList, qUnit=1)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotResidPred(eList)
plotResidQ(eList, qUnit=1)

## ----boxResidMonth, echo=TRUE, fig.cap="Residuals with respect to time",fig.subcap=c("\\texttt{plotResidTime(eList)}","\\texttt{boxResidMonth(eList)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotResidTime(eList)
boxResidMonth(eList)

## ----boxConcThree, echo=TRUE, fig.cap="Default \\texttt{boxConcThree(eList)}",out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='asis',results='hide',fig.pos="h"----
boxConcThree(eList)

## ----plotFluxHist, echo=TRUE, fig.cap="Concentration and flux history",fig.subcap=c("\\texttt{plotConcHist(eList)}", "\\texttt{plotFluxHist(eList)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcHist(eList)
plotFluxHist(eList)

## ----plotConcQSmooth, echo=TRUE, fig.cap="Concentration vs. discharge",fig.subcap=c("\\texttt{plotConcQSmooth}","\\texttt{plotConcQSmooth(logScale=TRUE)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
qBottom<-20
qTop<-700
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
plotConcQSmooth(eList, date1, date2, date3,
                qBottom, qTop, qUnit=1)
plotConcQSmooth(eList, date1, date2, date3,
                qBottom, qTop, qUnit=1,logScale=TRUE)

## ----plotConcTimeSmooth, echo=TRUE, fig.cap="\\texttt{plotConcTimeSmooth(eList))}",fig.subcap=c("\\texttt{plotConcTimeSmooth}","\\texttt{plotConcTimeSmooth(logScale=TRUE)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',results='hide',fig.pos="h"----
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, 2000, 2010)
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, 
                   2000, 2010,logScale=TRUE)

## ----fluxBiasMulti, echo=TRUE, fig.cap="\\texttt{fluxBiasMulti(eList, qUnit=1)}",fig.show='asis',fig.width=8, fig.height=10,fig.pos="h"----
fluxBiasMulti(eList, qUnit=1)

## ----plotContours, echo=TRUE,fig.cap="plotContours(eList)",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
clevel<-seq(0,2,0.2)
plotContours(eList, yearStart=2008,yearEnd=2010,qBottom=20,qTop=1000, 
             contourLevels = clevel,qUnit=1,
             flowDuration=FALSE)

## ----plotDiffContours, echo=TRUE, fig.cap="plotDiffContours(eList)",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotDiffContours(eList, year0=2000,year1=2010,
                 qBottom=20,qTop=1000,maxDiff=0.6,qUnit=1,
             flowDuration=FALSE)

## ----tableResults1, echo=TRUE, eval=FALSE-----------------
#  tableResults(eList)
#  returnDF <- tableResults(eList)

## ----tableResults2, echo=FALSE, eval=TRUE,results='hide'----
returnDF <- tableResults(eList)

## ----tableResultshead, echo=FALSE, results='asis'---------
print(xtable(head(returnDF),
       label="table:tableChangeHead",
       caption="Table created from \\texttt{head(returnDF)}",
       digits=c(0,0,2,3,3,3,3)),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )

## ----tableChange1, eval=TRUE, echo=TRUE-------------------
tableChange(eList, yearPoints=c(2000,2005,2010))

## ----tableChangeSingleR, eval=TRUE, echo=TRUE,results='hide'----
returnDF <- tableChangeSingle(eList, yearPoints=c(2000,2005,2010), 
                              returnDataFrame=TRUE)

## ----tableResultsShow, echo=FALSE, results='asis'---------
print(xtable(returnDF,
       label="tableChangeSingle",
       caption="Table created from \\texttt{tableChangeSingle} function",
       digits=c(0,0,0,3,2,1,1)),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )

## ----adjustSize,echo=TRUE,eval=TRUE,fig.cap="Modifying text and point size", fig.subcap=c("\\texttt{plotConcQ(cex.axis=2,cex.main=1.5,logScale=TRUE)}","\\texttt{plotConcQ(cex.lab=2,cex=2,logScale=TRUE)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcQ(eList, cex.axis=2,cex.main=1.5,logScale=TRUE)
plotConcQ(eList, cex.lab=2,cex=2,logScale=TRUE)

## ----plotConcQComparison,echo=TRUE,eval=TRUE,fig.cap="Modified plotConcQ", fig.subcap=c("Default","Modified"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcQ(eList, logScale=TRUE)
par(mar=c(8,8,8,8))
plotConcQ(eList, customPar=TRUE,col="blue",cex=1.1,
          cex.axis=1.4,cex.main=1.5,cex.lab=1.2,
          pch=18,lwd=2,logScale=TRUE)
grid(lwd=2)
legend(4.5,.09,"Choptank Nitrogen", pch=18, col="blue",bg="white")
arrows(3, 0.14, 1, .05,lwd=2)
text(12,.14,"Censored Value")

## ----easyFontChange,echo=TRUE,eval=TRUE,fig.cap="Serif font",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth', fig.pos="h"----
# Switching to serif font:
par(family="serif")
plotFluxPred(eList, customPar=TRUE)
mtext(side=3,line=-3,"Serif font example",cex=3)

## ----modifiedContour1,echo=TRUE,eval=TRUE,fig.cap="Contour plot with modified axis and color scheme",fig.show='hold',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
colors <- colorRampPalette(c("white","black"))
yTicksModified <- c(.5,1,10,25)
plotContours(eList, 2001,2010,0.5,50, 
             contourLevels = seq(0,2.5,0.5),qUnit=2,
             yTicks=yTicksModified,
             color.palette=colors,
             flowDuration=FALSE,
             tcl=0.2,tick.lwd=2.5)  

## ----modifiedDiffContour,echo=TRUE,eval=TRUE,fig.cap="Difference contour plot with modified color scheme",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
colors <- colorRampPalette(c("yellow","white","blue"))
maxDiff<-0.6
par(oma=c(1,1,1,1))
plotDiffContours(eList, year0=2001,year1=2010,qBottom=0.5,qTop=50, 
             maxDiff,lwd=2,qUnit=2,
             color.palette=colors,
             flowDuration=FALSE, customPar=TRUE)

## ----tinyPlot1,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot using tinyPlot",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
par(mfcol = c(2, 2), oma = c(0, 1.7, 6, 1.7))

plotFluxQ(eList, tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=FALSE,fluxMax=1)
plotConcQ(eList, tinyPlot=TRUE,printTitle=FALSE)
plotFluxHist(eList, tinyPlot=TRUE,printTitle=FALSE,fluxMax=1)
plotConcHist(eList, tinyPlot=TRUE,printTitle=FALSE,concMax=3)
mtext("Custom multi-pane graph using tinyPlot=TRUE", outer=TRUE, font=2)

## ----customPanel,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
par(mar=c(3.5,3.5,0.2,0.2), # whitespace around the plots
    oma=c(1,1,3,1), # outer margin
    mgp=c(2,0.5,0), # spacing between the label numbers and plots
    mfcol = c(2,2)) # rows/columns

plotFluxQ(eList, tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=FALSE,fluxMax=1,
          showXLabels=FALSE,showXAxis=FALSE, 
          showYLabels=TRUE,customPar=TRUE)

plotConcQ(eList, tinyPlot=TRUE,printTitle=FALSE, customPar=TRUE,
          removeLastY=TRUE,removeLastX=TRUE,
          showYLabels=TRUE)

plotFluxHist(eList, tinyPlot=TRUE,printTitle=FALSE,fluxMax=1,
          showYLabels=FALSE,showYAxis=FALSE,
          showXLabels=FALSE,showXAxis=FALSE, customPar=TRUE)
plotConcHist(eList, tinyPlot=TRUE,printTitle=FALSE,concMax=3,
          showYLabels=FALSE, showYAxis=FALSE, customPar=TRUE)
mtext("Custom multi-pane graph using customPar", outer=TRUE, font=2)

## ----helpFunc,eval = FALSE--------------------------------
#  ?getJulian

## ----rawFunc,eval = FALSE---------------------------------
#  getJulian

## ----installFromCran,eval = FALSE-------------------------
#  install.packages(c("dataRetrieval","EGRETdemo"),
#  repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
#  dependencies=TRUE,
#  type="both")

## ----openLibraryTest, eval=FALSE--------------------------
#  library(dataRetrieval)
#  library(EGRETdemo)

## ----label=getSiteApp, echo=TRUE,eval=TRUE----------------

tableData <- tableResults(eList)

## ----label=saveData, echo=TRUE, eval=FALSE----------------
#  write.table(tableData, file="tableData.tsv",sep="\t",
#              row.names = FALSE,quote=FALSE)

## ----label=savePlots, echo=TRUE, eval=FALSE---------------
#  jpeg("plotFlowSingle.jpg")
#  plotFlowSingle(1)
#  dev.off()
#  
#  png("plotFlowSingle.png")
#  plotFlowSingle(1)
#  dev.off()
#  
#  pdf("plotFlowSingle.pdf")
#  plotFlowSingle(1)
#  dev.off()
#  
#  postscript("plotFlowSingle.ps")
#  plotFlowSingle(1)
#  dev.off()
#  
#  #Many plots saved to one pdf:
#  pdf("manyPlots.pdf")
#  plotFlowSingle(1)
#  plotFlowSingle(2)
#  plotFlowSingle(3)
#  plotFlowSingle(4)
#  dev.off()
#  

## ----label=savePlots2, echo=TRUE, eval=FALSE--------------
#  postscript("fluxBiasMulti.ps", height=10,width=8)
#  fluxBiasMulti()
#  dev.off()


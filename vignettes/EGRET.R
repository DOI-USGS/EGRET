## ----include=TRUE ,echo=FALSE,eval=TRUE, message=FALSE, warning=FALSE---------

library(EGRET)
library(knitr)

knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE,
                      fig.height=7,
                      fig.width=7,
                      out.width = "45%",
                      fig.show = 'hold',
                      fig.align = 'center')


## ----workflowFlowHistory, echo=TRUE,eval=FALSE--------------------------------
#  library(EGRET)
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
#  INFO <- readNWISInfo(siteNumber,"00060")
#  INFO$shortName <- "Choptank River near Greensboro, MD"
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
#  saveResults(savePath, eList)
#  

## ----workflowWaterQuality, echo=TRUE,eval=FALSE-------------------------------
#  library(EGRET)
#  
#  ############################
#  # Gather discharge data:
#  siteNumber <- "01491000" #Choptank River near Greensboro, MD
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
#  # Here user must input some values:
#  INFO<- readNWISInfo(siteNumber,parameter_cd)
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
#  date1 <- "1985-09-01"
#  date2 <- "1997-09-0"
#  date3 <- "2010-09-01"
#  qBottom<-0.2
#  qTop<-10
#  plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop,
#                     concMax=2,legendTop = 0.8)
#  q1 <- 2
#  q2 <- 10
#  q3 <- 20
#  centerDate <- "07-01"
#  yearEnd <- 1980
#  yearStart <- 2010
#  plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd, legendTop = 0.7)
#  
#  # Multi-plots:
#  fluxBiasMulti(eList)
#  
#  #Contour plots:
#  clevel<-seq(0,2,0.5)
#  maxDiff<-0.8
#  yearStart <- 1980
#  yearEnd <- 2010
#  
#  plotContours(eList, yearStart,yearEnd,qBottom,qTop,
#               contourLevels = clevel,qUnit=2)
#  plotDiffContours(eList, yearStart,yearEnd,
#                   qBottom,qTop,maxDiff,qUnit=2)
#  
#  # modify this for your own computer file structure:
#  savePath<-"/Users/rhirsch/Desktop/"
#  saveResults(savePath, eList)

## ----openDataRetrieval, eval=FALSE--------------------------------------------
#  library(dataRetrieval)
#  vignette("dataRetrieval")

## ----openlibraries, echo=TRUE,eval=FALSE--------------------------------------
#  library(EGRET)

## ----firstExample, echo=TRUE, eval=FALSE--------------------------------------
#  siteNumber <- "01491000"
#  startDate <- "2000-01-01"
#  endDate <- "2013-01-01"
#  # This call will get NWIS (ft3/s) data , and convert it to m3/s:
#  Daily <- readNWISDaily(siteNumber, "00060", startDate, endDate)

## ----openDaily, eval = FALSE--------------------------------------------------
#  fileName <- "ChoptankRiverFlow.txt"
#  filePath <-  "C:/RData/"
#  Daily <- readUserDaily(filePath,fileName,
#                      separator="\t")

## ----secondExample,echo=TRUE,eval=FALSE---------------------------------------
#  siteNumber <- "01491000"
#  parameterCd <- "00618"
#  Sample <-readNWISSample(siteNumber,parameterCd,
#        startDate, endDate)

## ----STORET,echo=TRUE,eval=FALSE----------------------------------------------
#  site <- 'WIDNR_WQX-10032762'
#  characteristicName <- 'Specific conductance'
#  Sample <-readWQPSample(site,characteristicName,
#        startDate, endDate)

## ----openSample, eval = FALSE-------------------------------------------------
#  fileName <- "ChoptankRiverNitrate.csv"
#  filePath <-  "C:/RData/"
#  Sample <- readUserSample(filePath,fileName,
#                                  separator=",")

## ----openSample2, eval = FALSE------------------------------------------------
#  fileName <- "ChoptankPhosphorus.txt"
#  filePath <-  "C:/RData/"
#  Sample <-readUserSample(filePath,fileName,
#                                  separator="\t")

## ----label=tab:exampleComplexQW, echo=FALSE, eval=TRUE,results='asis'---------
cdate <- c("2003-02-15","2003-06-30","2004-09-15","2005-01-30","2005-05-30","2005-10-30")
rdp <- c("", "<","<","","","")
dp <- c(0.02,0.01,0.005,NA,NA,NA)
rpp <- c("", "","<","","","")
pp <- c(0.5,0.3,0.2,NA,NA,NA)
rtp <- c("","","","","<","<")
tp <- c(NA,NA,NA,0.43,0.05,0.02)

DF <- data.frame(cdate,rdp,dp,rpp,pp,rtp,tp,stringsAsFactors=FALSE)

knitr::kable(DF,  caption="Example data", 
                     digits=c(0,0,0,3,0,3,0,3))


## ----thirdExample,echo=FALSE--------------------------------------------------
  compressedData <- compressData(DF)
  Sample <- populateSampleColumns(compressedData)

## ----thirdExampleView,echo=TRUE-----------------------------------------------
  Sample

## ----ThirdExample, eval=FALSE-------------------------------------------------
#  parameterCd <- "00618"
#  siteNumber <- "01491000"
#  INFO <- readNWISInfo(siteNumber,parameterCd, interactive=FALSE)

## ----WQPInfo, eval=FALSE------------------------------------------------------
#  parameterCd <- "00618"
#  INFO_WQP <- readWQPInfo("USGS-01491000",parameterCd)

## ----addInfoCustom, eval=FALSE, echo=TRUE-------------------------------------
#  
#  fileName <- "INFO.csv"
#  filePath <- "C:/RData/"
#  
#  INFO <- readUserInfo(filePath, fileName)
#  

## ----addInfo, eval=FALSE, echo=TRUE-------------------------------------------
#  
#  INFO$riverInfo <- "Major tributary of the Chesapeake Bay"
#  INFO$GreensboroPopulation <- 1931
#  

## ----mergeExample, eval=FALSE-------------------------------------------------
#  siteNumber <- "01491000"
#  parameterCd <- "00631"  # Nitrate
#  startDate <- "2000-01-01"
#  endDate <- "2013-01-01"
#  
#  Daily <- readNWISDaily(siteNumber, "00060", startDate, endDate)
#  Sample <- readNWISSample(siteNumber,parameterCd, startDate, endDate)
#  INFO <- readNWISInfo(siteNumber, parameterCd)
#  
#  eList <- mergeReport(INFO, Daily,Sample)

## ----egretObedit, echo=TRUE, eval=FALSE---------------------------------------
#  eListNew <- as.egret(INFO, Daily, Sample, surfaces)
#  #To pull out the INFO data frame:
#  INFO <- getInfo(eListNew)
#  #Edit the INFO data frame:
#  INFO$importantNews <- "New EGRET workflow started"
#  #Put new data frame in eListNew
#  eListNew$INFO <- INFO
#  #To pull out Daily:
#  Daily <- getDaily(eListNew)
#  #Edit for some reason:
#  DailyNew <- Daily[Daily$DecYear > 1985,]
#  #Put new Daily data frame back in eListNew:
#  eListNew$Daily <- DailyNew
#  #To create a whole new egret object:
#  eList_2 <- as.egret(INFO, DailyNew, getSample(eListNew), NA)
#  

## ----cheatSheets,echo=TRUE,eval=TRUE,results='markup'-------------------------
printqUnitCheatSheet()

## ----cheatSheets2,echo=TRUE,eval=TRUE,results='markup'------------------------
printFluxUnitCheatSheet()

## ----flowHistory,echo=TRUE,eval=FALSE-----------------------------------------
#  siteNumber <- "14105700"
#  startDate <- ""
#  endDate <- ""
#  
#  Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
#  INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
#  INFO$shortName <- "Columbia River at The Dalles, OR"
#  
#  eList <- as.egret(INFO, Daily, NA, NA)

## ----flowHistoryLoad,echo=FALSE-----------------------------------------------
fileName <- "eListColumbia.RData"
load(fileName)
eList <- eListColumbia

## ----newChunckWinter, echo=TRUE,eval=FALSE------------------------------------
#  eList <- setPA(eList,paStart=12,paLong=3)

## ----newChunck, echo=TRUE-----------------------------------------------------
eList <- setPA(eList)

## ----plotSingleandSD, echo=TRUE, fig.cap="Plots of discharge statistics"------
plotFlowSingle(eList, istat=5,qUnit="thousandCfs")
plotSDLogQ(eList)

## ----Merced, echo=TRUE,eval=FALSE---------------------------------------------
#  # Merced River at Happy Isles Bridge, CA:
#  siteNumber<-"11264500"
#  Daily <-readNWISDaily(siteNumber,"00060",startDate="",endDate="")
#  INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
#  INFO$shortName <- "Merced River at Happy Isles Bridge, CA"
#  eList <- as.egret(INFO, Daily, NA, NA)

## ----Merceddata, echo=FALSE---------------------------------------------------
fileName <- "eListMerced.RData"
load(fileName)
eList <- eListMerced

## ----Mercedplot, echo=TRUE,fig.cap="Merced River winter trend"----------------
plotFlowSingle(eList, istat=5)
# Then, we can run the same function, but first set 
# the pa to start in December and only run for 3 months.
eListMerced <- setPA(eList,paStart=12,paLong=3)
plotFlowSingle(eListMerced,istat=5,qMax=200)


## ----plotFour, echo=TRUE, fig.cap="`plotFour(eListMerced, qUnit=3)`", out.width="100%"----
plotFour(eList, qUnit=3)

## ----plotFourStats,echo=TRUE,out.width="100%", fig.cap="`plotFourStats(eListMerced, qUnit=3)`"----
plotFourStats(eList, qUnit=3)

## ----MississippiData, echo=TRUE,eval=FALSE------------------------------------
#  #Mississippi River at Keokuk Iowa:
#  siteNumber<-"05474500"
#  Daily <-readNWISDaily(siteNumber,"00060",startDate="",endDate="")
#  INFO <- readNWISInfo(siteNumber,"",interactive=FALSE)
#  INFO$shortName <- "Mississippi River at Keokuk Iowa"
#  eList <- as.egret(INFO, Daily, NA, NA)

## ----MissDataRetrieval, echo=FALSE--------------------------------------------
fileName <- "eListMiss.RData"
load(fileName)
eList <- eListMiss

## ----MississippiPlot, echo=TRUE,fig.cap="Mississippi River at Keokuk Iowa"----
plotQTimeDaily(eList, qUnit=3,qLower=300)


## ----printSeries, eval=FALSE,echo=TRUE----------------------------------------
#  seriesResult <- printSeries(eList, istat=3, qUnit=3)

## ----tfc, echo=TRUE-----------------------------------------------------------
tableFlowChange(eList, istat=3, qUnit=3,yearPoints=c(1890,1950,2010))

## ----wrtds1,eval=FALSE,echo=TRUE----------------------------------------------
#  #Choptank River at Greensboro, MD:
#  siteNumber <- "01491000"
#  startDate <- "1979-10-01"
#  endDate <- "2011-09-30"
#  param<-"00631"
#  Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
#  INFO<- readNWISInfo(siteNumber,param,interactive=FALSE)
#  INFO$shortName <- "Choptank River"
#  
#  Sample <- readNWISSample(siteNumber,param,startDate,endDate)
#  eList <- mergeReport(INFO, Daily, Sample)

## ----wrtds2,eval=TRUE,echo=FALSE----------------------------------------------
siteNumber <- "01491000" #Choptank River at Greensboro, MD
startDate <- "1979-10-01"
endDate <- "2011-09-30"
param<-"00631"
eList <- Choptank_eList
Daily <- getDaily(eList)
Sample <- getSample(eList)
INFO <- getInfo(eList)


## ----plotBoxes, echo=TRUE, fig.cap="Concentration box plots"------------------
boxConcMonth(eList)
boxQTwice(eList,qUnit=1)

## ----plotConcTime,echo=TRUE, fig.cap="The relation of concentration vs time or discharge"----
plotConcTime(eList)
plotConcQ(eList, qUnit=1)

## ----plotFluxQ,echo=TRUE, fig.cap="The relation of flux vs discharge"---------
plotFluxQ(eList, fluxUnit=4)

## ----multiPlotDataOverview, echo=TRUE,out.width="100%", fig.cap="`multiPlotDataOverview(eList, qUnit=1)`"----
multiPlotDataOverview(eList, qUnit=1)

## ----flowDuration, echo=TRUE--------------------------------------------------
flowDuration(eList, qUnit=1)

flowDuration(eList, qUnit=1, centerDate="09-30", span=30)

## ----wrtds3, eval=FALSE, echo=TRUE--------------------------------------------
#  eList <- modelEstimation(eList)

## ----wrtds5, eval=FALSE, echo=TRUE--------------------------------------------
#  #An example directory name
#  savePath <- "C:/Users/egretUser/WRTDS_Output/"
#  saveResults(savePath, eList)

## ----wrtds8, eval=FALSE, echo=TRUE--------------------------------------------
#  loadPath <- "C:/Users/egretUser/WRTDS_Output/"
#  staAbbrev <- "Chop"
#  constitAbbrev <- "NO3"
#  pathToFile <- paste0(loadPath,staAbbrev,".",
#                      constitAbbrev,".RData")
#  load(pathToFile)

## ----getChopData1,echo=FALSE,eval=TRUE----------------------------------------
# Sample <- getSample(eList)
# Daily <- getDaily(eList)
# INFO <- getInfo(eList)
# surfaces <- getSurfaces(eList)
eList <- Choptank_eList

## ----plotConcTimeDaily, echo=TRUE, fig.cap="Concentration and flux vs time"----
# Return to water year:
eList <- setPA(eList)

yearStart <- 2008
yearEnd <- 2010

plotConcTimeDaily(eList, yearStart, yearEnd)
plotFluxTimeDaily(eList, yearStart, yearEnd)

## ----plotFluxPred, echo=TRUE, fig.cap="Concentration and flux predictions"----
plotConcPred(eList)
plotFluxPred(eList)

## ----plotResidQ, echo=TRUE, fig.cap="Residuals"-------------------------------
plotResidPred(eList)
plotResidQ(eList, qUnit=1)

## ----boxResidMonth, echo=TRUE, fig.cap="Residuals with respect to time"-------
plotResidTime(eList)
boxResidMonth(eList)

## ----boxConcThree, echo=TRUE, fig.cap="Default `boxConcThree(eList)`"---------
boxConcThree(eList)

## ----plotFluxHist, echo=TRUE, fig.cap="Concentration and flux history"--------
plotConcHist(eList)
plotFluxHist(eList)

## ----plotConcQSmooth, echo=TRUE, fig.cap="Concentration vs. discharge"--------
# Multi-line plots:
date1 <- "1985-09-01"
date2 <- "1997-09-01"
date3 <- "2010-09-01"
qBottom <- 0.2
qTop <- 10
plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop, 
                   concMax=2,legendTop = 0.5)

plotConcQSmooth(eList, date1, date2, date3,
                qBottom, qTop, legendTop = 0.5,logScale=TRUE)

## ----plotConcTimeSmooth, echo=TRUE, fig.cap="`plotConcTimeSmooth(eList))`"----
q1 <- 2
q2 <- 10
q3 <- 20
centerDate <- "07-01"
yearEnd <- 1980
yearStart <- 2010
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, 
                   yearStart, yearEnd, legendTop = 0.4)

plotConcTimeSmooth(eList, q1, q2, q3, centerDate, 
                   yearStart, yearEnd, 
                   legendTop = 0.4,logScale=TRUE)

## ----fluxBiasMulti, echo=TRUE, fig.cap="`fluxBiasMulti(eList, qUnit=1)`",fig.show='asis',fig.width=8, fig.height=10,fig.pos="h", out.width="100%"----
fluxBiasMulti(eList, qUnit=1)

## ----plotContours,out.width="100%",fig.height=5, echo=TRUE,fig.cap="`plotContours(eList)`"----
clevel<-seq(0,2,0.2)
plotContours(eList, yearStart=1980,yearEnd=2010,qBottom=0.5,qTop=20, 
             contourLevels = clevel)

## ----plotDiffContours, echo=TRUE,out.width="100%",fig.height=5, fig.cap="`plotDiffContours(eList)`"----
plotDiffContours(eList, year0=1990,year1=2010,
                 qBottom=0.5,qTop=20,maxDiff=0.6)

## ----tableResults1, echo=TRUE, eval=FALSE-------------------------------------
#  tableResults(eList)
#  returnDF <- tableResults(eList)

## ----tableResults2, echo=FALSE, eval=TRUE,results='hide'----------------------
returnDF <- tableResults(eList)

## ----tableResultshead, echo=FALSE, results='asis'-----------------------------
knitr::kable(head(returnDF),
       caption="Table created from `head(returnDF)`",
       digits=c(0,0,2,3,3,3,3))

## ----tableChange1, eval=TRUE, echo=TRUE---------------------------------------
tableChange(eList, yearPoints=c(2000,2005,2010))

## ----tableChangeSingleR, eval=TRUE, echo=TRUE,results='hide'------------------
returnDF <- tableChangeSingle(eList, yearPoints=c(2000,2005,2010))

## ----tableResultsShow, echo=FALSE, results='asis'-----------------------------
knitr::kable(returnDF,
       caption="Table created from `tableChangeSingle` function",
       digits=c(0,0,0,3,2,1,1))

## ----adjustSize,echo=TRUE,eval=TRUE,fig.cap="Modifying text and point size, as shown using the `plotConcQ` function"----
plotConcQ(eList, cex.axis=2,cex.main=1.5,logScale=TRUE)
plotConcQ(eList, cex.lab=2,cex=2,logScale=TRUE)

## ----plotConcQComparison,echo=TRUE,eval=TRUE,fig.cap="Modified `plotConcQ`"----
plotConcQ(eList, logScale=TRUE)
par(mar=c(8,8,8,8))
plotConcQ(eList, customPar=TRUE,col="blue",cex=1.1,
          cex.axis=1.4,cex.main=1.5,cex.lab=1.2,
          pch=18,lwd=2,logScale=TRUE)
grid(lwd=2)
legend(4.5,.09,"Choptank Nitrogen", pch=18, col="blue",bg="white")
arrows(3, 0.14, 1, .05,lwd=2)
text(12,.14,"Censored Value")

## ----easyFontChange,echo=TRUE,eval=TRUE,fig.cap="Serif font"------------------
# Switching to serif font:
par(family="serif")
plotFluxPred(eList, customPar=TRUE)
mtext(side=3,line=-3,"Serif font example",cex=3)

## ----modifiedContour1,echo=TRUE,eval=TRUE,fig.cap="Contour plot with modified axis and color scheme", out.width="100%",fig.height=5----
colors <- colorRampPalette(c("white","black"))
yTicksModified <- c(.5,1,10,25)
plotContours(eList, 2001,2010,0.5,50, 
             contourLevels = seq(0,2.5,0.5),qUnit=2,
             yTicks=yTicksModified,
             color.palette=colors,
             flowDuration=FALSE,
             tcl=0.2,tick.lwd=2.5)  

## ----modifiedDiffContour,echo=TRUE,eval=TRUE,fig.cap="Difference contour plot with modified color scheme",out.width="100%",fig.height=5----
colors <- colorRampPalette(c("yellow","white","blue"))
maxDiff<-0.6
par(oma=c(1,1,1,1))
plotDiffContours(eList, year0=2001,year1=2010,qBottom=0.5,qTop=50, 
             maxDiff,lwd=2,qUnit=2,
             color.palette=colors,
             flowDuration=FALSE, customPar=TRUE)

## ----tinyPlot1,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot using tinyPlot", out.width="100%"----
par(mfcol = c(2, 2), oma = c(0, 1.7, 6, 1.7))

plotFluxQ(eList, tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=TRUE,fluxMax=1)
plotConcQ(eList, tinyPlot=TRUE,printTitle=FALSE)
plotFluxHist(eList, tinyPlot=TRUE,printTitle=FALSE,fluxMax=1)
plotConcHist(eList, tinyPlot=TRUE,printTitle=FALSE,concMax=3)
mtext("Custom multi-pane graph using tinyPlot=TRUE", outer=TRUE, font=2)

## ----customPanel,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot", out.width="100%"----
par(mar=c(3.5,3.5,0.2,0.2), # whitespace around the plots
    oma=c(1,1,3,1), # outer margin
    mgp=c(2,0.5,0), # spacing between the label numbers and plots
    mfcol = c(2,2)) # rows/columns

plotFluxQ(eList, tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=TRUE,fluxMax=1,
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

## ----helpFunc,eval = FALSE----------------------------------------------------
#  ?plotConcQ

## ----rawFunc,eval = FALSE-----------------------------------------------------
#  plotConcQ

## ----installFromCran,eval = FALSE---------------------------------------------
#  install.packages("EGRET")

## ----openLibraryTest, eval=FALSE----------------------------------------------
#  library(EGRET)

## ----label=getSiteApp, echo=TRUE,eval=TRUE------------------------------------
tableData <- tableResults(eList)

## ----label=saveData, echo=TRUE, eval=FALSE------------------------------------
#  write.table(tableData, file="tableData.tsv",sep="\t",
#              row.names = FALSE,quote=FALSE)

## ----getTable, echo = FALSE---------------------------------------------------
knitr::include_graphics("table1.png")

## ----label=savePlots, echo=TRUE, eval=FALSE-----------------------------------
#  jpeg("plotFlowSingle.jpg")
#  plotFlowSingle(eList, 1)
#  dev.off()
#  
#  png("plotFlowSingle.png")
#  plotFlowSingle(eList,1)
#  dev.off()
#  
#  pdf("plotFlowSingle.pdf")
#  plotFlowSingle(eList,1)
#  dev.off()
#  
#  postscript("plotFlowSingle.ps")
#  plotFlowSingle(eList,1)
#  dev.off()
#  
#  #Many plots saved to one pdf:
#  pdf("manyPlots.pdf")
#  plotFlowSingle(eList,1)
#  plotFlowSingle(eList,2)
#  plotFlowSingle(eList,3)
#  plotFlowSingle(eList,4)
#  dev.off()
#  

## ----label=savePlots2, echo=TRUE, eval=FALSE----------------------------------
#  postscript("fluxBiasMulti.ps", height=10,width=8)
#  fluxBiasMulti(eList)
#  dev.off()


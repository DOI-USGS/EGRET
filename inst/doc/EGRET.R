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
#  library(EGRET)
#  
#  # Flow history analysis
#  
#  ############################
#  # Gather discharge data:
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "" # Get earliest date
#  endDate <- "" # Get latest date
#  Daily <- getNWISDaily(siteNumber,"00060",startDate,endDate)
#  # Gather site and parameter information:
#  # Here user must input some values for
#  # the default (interactive=TRUE)
#  INFO<- getNWISInfo(siteNumber,"00060")
#  INFO$shortName <- "Choptank River at Greensboro, MD"
#  ############################
#  
#  ############################
#  # Check flow history data:
#  annualSeries <- makeAnnualSeries()
#  plotFlowSingle(istat=7,qUnit="thousandCfs")
#  plotSDLogQ()
#  plotQTimeDaily(qLower=1,qUnit=3)
#  plotFour(qUnit=3)
#  plotFourStats(qUnit=3)
#  ############################
#  
#  # modify this for your own computer file structure:
#  savePath<-"/Users/rhirsch/Desktop/"
#  saveResults(savePath)
#  

## ----workflowWaterQuality, echo=TRUE,eval=FALSE-----------
#  library(dataRetrieval)
#  library(EGRET)
#  
#  ############################
#  # Gather discharge data:
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "" #Gets earliest date
#  endDate <- "2011-09-30"
#  # Gather sample data:
#  parameter_cd<-"00631" #5 digit USGS code
#  Sample <- getNWISSample(siteNumber,parameter_cd,startDate,endDate)
#  #Gets earliest date from Sample record:
#  #This is just one of many ways to assure the Daily record
#  #spans the Sample record
#  startDate <- min(as.character(Sample$Date))
#  # Gather discharge data:
#  Daily <- getNWISDaily(siteNumber,"00060",startDate,endDate)
#  # Gather site and parameter information:
#  
#  # Here user must input some values for
#  # the default (interactive=TRUE)
#  INFO<- getNWISInfo(siteNumber,parameter_cd)
#  INFO$shortName <- "Choptank River at Greensboro, MD"
#  
#  # Merge discharge with sample data:
#  Sample <- mergeReport(Daily, Sample)
#  ############################
#  
#  ############################
#  # Check sample data:
#  boxConcMonth()
#  boxQTwice()
#  plotConcTime()
#  plotConcQ()
#  multiPlotDataOverview()
#  ############################
#  
#  ############################
#  # Run WRTDS model:
#  modelEstimation()
#  ############################
#  
#  ############################
#  #Check model results:
#  
#  #Require Sample + INFO:
#  plotConcTimeDaily()
#  plotFluxTimeDaily()
#  plotConcPred()
#  plotFluxPred()
#  plotResidPred()
#  plotResidQ()
#  plotResidTime()
#  boxResidMonth()
#  boxConcThree()
#  
#  #Require Daily + INFO:
#  plotConcHist()
#  plotFluxHist()
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
#  plotConcTimeSmooth(q1, q2, q3, centerDate, yearStart, yearEnd)
#  
#  # Multi-plots:
#  fluxBiasMulti()
#  
#  #Contour plots:
#  clevel<-seq(0,2,0.5)
#  maxDiff<-0.8
#  yearStart <- 2000
#  yearEnd <- 2010
#  
#  plotContours(yearStart,yearEnd,qBottom,qTop,
#               contourLevels = clevel,qUnit=1)
#  plotDiffContours(yearStart,yearEnd,
#                   qBottom,qTop,maxDiff,qUnit=1)
#  
#  # modify this for your own computer file structure:
#  savePath<-"/Users/rhirsch/Desktop/"
#  saveResults(savePath)

## ----openlibraries, echo=TRUE,eval=TRUE-------------------
library(dataRetrieval)
library(EGRET)

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

Daily <- getNWISDaily(siteNumber,"00060",startDate,endDate)
INFO <- getNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Columbia River at The Dalles, OR"

## ----newChunckWinter, echo=TRUE,eval=FALSE----------------
#  INFO <- setPA(paStart=12,paLong=3)

## ----newChunck, echo=TRUE,eval=TRUE-----------------------
INFO <- setPA()

## ----newChunckAS, echo=TRUE,eval=TRUE---------------------
annualSeries <- makeAnnualSeries()

## ----plotSingleandSD, echo=TRUE, fig.cap="Plots of discharge statistics",fig.subcap=c("plotFlowSingle(istat=5,qUnit='thousandCfs')","plotSDLogQ()"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotFlowSingle(istat=5,qUnit="thousandCfs")
plotSDLogQ()

## ----Merced, echo=TRUE,eval=TRUE,fig.cap="Merced River Winter Trend",fig.subcap=c("Water Year", "December - February"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
siteNumber<-"11264500"
Daily <-getNWISDaily(siteNumber,"00060",startDate="",endDate="")
INFO <- getNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Merced River at Happy Isles Bridge, CA"
INFO <- setPA()
annualSeries <- makeAnnualSeries()
plotFlowSingle(istat=5)

# Then, we can run the same function, but first set 
# the pa to start in December and only run for 3 months.

INFO<-setPA(paStart=12,paLong=3)
annualSeries<-makeAnnualSeries()
plotFlowSingle(istat=5,qMax=200)


## ----plotFour, echo=TRUE, fig.cap="\\texttt{plotFour(qUnit=3)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotFour(qUnit=3)

## ----plotFourStats,echo=TRUE, fig.cap="\\texttt{plotFourStats(qUnit=3)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotFourStats(qUnit=3)

## ----Mississippi, echo=TRUE,eval=TRUE,fig.cap="Mississippi River at Keokuk Iowa",fig.subcap=c("Water Year", "Dec-Feb"),out.width='1\\linewidth',out.height='1\\linewidth',fig.show='hold',fig.pos="h"----
siteNumber<-"05474500"
Daily <-getNWISDaily(siteNumber,"00060",startDate="",endDate="")
INFO <- getNWISInfo(siteNumber,"",interactive=FALSE)
INFO$shortName <- "Mississippi River at Keokuk Iowa"
INFO <- setPA()

plotQTimeDaily(qUnit=3,qLower=300)


## ----printSeries, eval=FALSE,echo=TRUE--------------------
#  annualSeries<-makeAnnualSeries()
#  seriesResult <- printSeries(istat=3, qUnit=3)

## ----tfc, eval=TRUE,echo=TRUE-----------------------------
annualSeries <- makeAnnualSeries()
tableFlowChange(istat=3, qUnit=3,yearPoints=c(1890,1950,2010))

## ----wrtds1,eval=FALSE,echo=TRUE--------------------------
#  siteNumber <- "01491000" #Choptank River at Greensboro, MD
#  startDate <- "1979-10-01"
#  endDate <- "2011-09-30"
#  param<-"00631"
#  Daily <- getNWISDaily(siteNumber,"00060",startDate,endDate)
#  INFO<- getNWISInfo(siteNumber,param,interactive=FALSE)
#  INFO$shortName <- "Choptank River"
#  
#  Sample <- getNWISSample(siteNumber,param,startDate,endDate)
#  Sample <- mergeReport(Daily, Sample)

## ----wrtds2,eval=TRUE,echo=FALSE--------------------------
siteNumber <- "01491000" #Choptank River at Greensboro, MD
startDate <- "1979-10-01"
endDate <- "2011-09-30"
param<-"00631"
Daily <- ChopDaily
Sample <- ChopSample
INFO <- ChopINFO
annualSeries <- makeAnnualSeries()

## ----plotBoxes, echo=TRUE, fig.cap="Concentration box plots",fig.subcap=c("\\texttt{boxConcMonth()}","\\texttt{boxQTwice(qUnit=1)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
boxConcMonth()
boxQTwice(qUnit=1)

## ----plotConcTime,echo=TRUE, fig.cap="The relation of concentration vs time or discharge",fig.subcap=c("\\texttt{plotConcTime()}","\\texttt{plotConcQ()}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcTime()
plotConcQ(qUnit=1)

## ----plotFluxQ,echo=TRUE, fig.cap="The relation of flux vs discharge",out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotFluxQ(fluxUnit=4)

## ----multiPlotDataOverview, echo=TRUE, fig.cap="\\texttt{multiPlotDataOverview(qUnit=1)}",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
multiPlotDataOverview(qUnit=1)

## ----flowDuration, eval=TRUE, echo=TRUE-------------------
flowDuration(qUnit=1)

flowDuration(qUnit=1, centerDate="09-30", span=30)

## ----wrtds3, eval=FALSE, echo=TRUE------------------------
#  modelEstimation()

## ----wrtds5, eval=FALSE, echo=TRUE------------------------
#  savePath <- "C:/Users/egretUser/WRTDS_Output/" #An example directory name
#  saveResults(savePath)

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

## ----plotConcTimeDaily, echo=TRUE, fig.cap="Concentration and flux vs time",fig.subcap=c("\\texttt{plotConcTimeDaily(2008, 2010)}","\\texttt{plotFluxTimeDaily(2008, 2010)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
# Return to water year:
INFO <- setPA()

yearStart <- 2008
yearEnd <- 2010

plotConcTimeDaily(yearStart, yearEnd)
plotFluxTimeDaily(yearStart, yearEnd)

## ----plotFluxPred, echo=TRUE, fig.cap="Concentration and flux predictions",fig.subcap=c('\\texttt{plotConcPred()}','\\texttt{plotFluxPred()}'),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcPred()
plotFluxPred()

## ----plotResidQ, echo=TRUE, fig.cap="Residuals",fig.subcap=c("\\texttt{plotResidPred()}","\\texttt{plotResidQ(qUnit=1)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotResidPred()
plotResidQ(qUnit=1)

## ----boxResidMonth, echo=TRUE, fig.cap="Residuals with respect to time",fig.subcap=c("\\texttt{plotResidTime()}","\\texttt{boxResidMonth()}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotResidTime()
boxResidMonth()

## ----boxConcThree, echo=TRUE, fig.cap="Default \\texttt{boxConcThree()}",out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='asis',results='hide',fig.pos="h"----
boxConcThree()

## ----plotFluxHist, echo=TRUE, fig.cap="Concentration and flux history",fig.subcap=c("\\texttt{plotConcHist()}", "\\texttt{plotFluxHist()}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcHist()
plotFluxHist()

## ----plotConcQSmooth, echo=TRUE, fig.cap="Concentration vs. discharge",fig.subcap=c("\\texttt{plotConcQSmooth}","\\texttt{plotConcQSmooth(logScale=TRUE)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
qBottom<-20
qTop<-700
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
plotConcQSmooth(date1, date2, date3,
                qBottom, qTop, qUnit=1)
plotConcQSmooth(date1, date2, date3,
                qBottom, qTop, qUnit=1,logScale=TRUE)

## ----plotConcTimeSmooth, echo=TRUE, fig.cap="\\texttt{plotConcTimeSmooth())}",fig.subcap=c("\\texttt{plotConcTimeSmooth}","\\texttt{plotConcTimeSmooth(logScale=TRUE)}"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',results='hide',fig.pos="h"----
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"
plotConcTimeSmooth(q1, q2, q3, centerDate, 2000, 2010)
plotConcTimeSmooth(q1, q2, q3, centerDate, 
                   2000, 2010,logScale=TRUE)

## ----fluxBiasMulti, echo=TRUE, fig.cap="\\texttt{fluxBiasMulti(qUnit=1)}",fig.show='asis',fig.width=8, fig.height=10,fig.pos="h"----
fluxBiasMulti(qUnit=1)

## ----plotContours, echo=TRUE,fig.cap="plotContours()",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
clevel<-seq(0,2,0.2)
plotContours(yearStart=2008,yearEnd=2010,qBottom=20,qTop=1000, 
             contourLevels = clevel,qUnit=1,
             flowDuration=FALSE)

## ----plotDiffContours, echo=TRUE, fig.cap="plotDiffContours()",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
plotDiffContours(year0=2000,year1=2010,
                 qBottom=20,qTop=1000,maxDiff=0.6,qUnit=1,
             flowDuration=FALSE)

## ----tableResults1, echo=TRUE, eval=FALSE-----------------
#  tableResults()
#  returnDF <- tableResults(returnDataFrame=TRUE)

## ----tableResults2, echo=FALSE, eval=TRUE,results='hide'----
returnDF <- tableResults(returnDataFrame=TRUE)

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
tableChange(yearPoints=c(2000,2005,2010))

## ----tableChangeSingleR, eval=TRUE, echo=TRUE,results='hide'----
returnDF <- tableChangeSingle(yearPoints=c(2000,2005,2010), 
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
plotConcQ(cex.axis=2,cex.main=1.5,logScale=TRUE)
plotConcQ(cex.lab=2,cex=2,logScale=TRUE)

## ----plotConcQComparison,echo=TRUE,eval=TRUE,fig.cap="Modified plotConcQ", fig.subcap=c("Default","Modified"),out.width='.5\\linewidth',out.height='.5\\linewidth',fig.show='hold',fig.pos="h"----
plotConcQ(logScale=TRUE)
par(mar=c(8,8,8,8))
plotConcQ(customPar=TRUE,col="blue",cex=1.1,
          cex.axis=1.4,cex.main=1.5,cex.lab=1.2,
          pch=18,lwd=2,logScale=TRUE)
grid(lwd=2)
legend(4.5,.09,"Choptank Nitrogen", pch=18, col="blue",bg="white")
arrows(3, 0.14, 1, .05,lwd=2)
text(12,.14,"Censored Value")

## ----easyFontChange,echo=TRUE,eval=TRUE,fig.cap="Serif font",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth', fig.pos="h"----
# Switching to serif font:
par(family="serif")
plotFluxPred(customPar=TRUE)
mtext(side=3,line=-3,"Serif font example",cex=3)

## ----modifiedContour1,echo=TRUE,eval=TRUE,fig.cap="Contour plot with modified axis and color scheme",fig.show='hold',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
colors <- colorRampPalette(c("white","black"))
yTicksModified <- c(.5,1,10,25)
plotContours(2001,2010,0.5,50, 
             contourLevels = seq(0,2.5,0.5),qUnit=2,
             yTicks=yTicksModified,
             color.palette=colors,
             flowDuration=FALSE,
             tcl=0.2,tick.lwd=2.5)  

## ----modifiedDiffContour,echo=TRUE,eval=TRUE,fig.cap="Difference contour plot with modified color scheme",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
colors <- colorRampPalette(c("yellow","white","blue"))
maxDiff<-0.6
par(oma=c(1,1,1,1))
plotDiffContours(year0=2001,year1=2010,qBottom=0.5,qTop=50, 
             maxDiff,lwd=2,qUnit=2,
             color.palette=colors,
             flowDuration=FALSE, customPar=TRUE)

## ----tinyPlot1,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot using tinyPlot",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
par(mfcol = c(2, 2), oma = c(0, 1.7, 6, 1.7))

plotFluxQ(tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=FALSE,fluxMax=1)
plotConcQ(tinyPlot=TRUE,printTitle=FALSE)
plotFluxHist(tinyPlot=TRUE,printTitle=FALSE,fluxMax=1)
plotConcHist(tinyPlot=TRUE,printTitle=FALSE,concMax=3)
mtext("Custom multi-pane graph using tinyPlot=TRUE", outer=TRUE, font=2)

## ----customPanel,echo=TRUE,eval=TRUE,fig.cap="Custom multipanel plot",fig.show='asis',out.width='1\\linewidth',out.height='1\\linewidth',fig.pos="h"----
par(mar=c(3.5,3.5,0.2,0.2), # whitespace around the plots
    oma=c(1,1,3,1), # outer margin
    mgp=c(2,0.5,0), # spacing between the label numbers and plots
    mfcol = c(2,2)) # rows/columns

plotFluxQ(tinyPlot=TRUE,printTitle=FALSE,
          fluxUnit=9,logScale=FALSE,fluxMax=1,
          showXLabels=FALSE,showXAxis=FALSE, 
          showYLabels=TRUE,customPar=TRUE)

plotConcQ(tinyPlot=TRUE,printTitle=FALSE, customPar=TRUE,
          removeLastY=TRUE,removeLastX=TRUE,
          showYLabels=TRUE)

plotFluxHist(tinyPlot=TRUE,printTitle=FALSE,fluxMax=1,
          showYLabels=FALSE,showYAxis=FALSE,
          showXLabels=FALSE,showXAxis=FALSE, customPar=TRUE)
plotConcHist(tinyPlot=TRUE,printTitle=FALSE,concMax=3,
          showYLabels=FALSE, showYAxis=FALSE, customPar=TRUE)
mtext("Custom multi-pane graph using customPar", outer=TRUE, font=2)

## ----helpFunc,eval = FALSE--------------------------------
#  ?getJulian

## ----rawFunc,eval = FALSE---------------------------------
#  getJulian

## ----installFromCran,eval = FALSE-------------------------
#  install.packages(c("dataRetrieval","EGRET"),
#  repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
#  dependencies=TRUE,
#  type="both")

## ----openLibraryTest, eval=FALSE--------------------------
#  library(dataRetrieval)
#  library(EGRET)

## ----label=getSiteApp, echo=TRUE,eval=FALSE---------------
#  
#  tableData <- tableResults(returnDataFrame=TRUE)

## ----label=getSiteApp2, echo=FALSE,eval=TRUE--------------

tableData <- tableResults(returnDataFrame=TRUE)

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


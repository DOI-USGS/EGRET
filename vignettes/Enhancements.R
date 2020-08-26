## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(EGRET)

library(knitr)

knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE,
                      fig.width=7, fig.height=7)


## ----echo = FALSE-------------------------------------------------------------
library(EGRET)

firstQDate0 <- "1981-08-06"
lastQDate0 <- "2016-01-14" 

plot(3000, 30,  xlim = c(1980,2020), xaxs = "i", ylim = c(0, 23), yaxs = "i",xlab = "",
     ylab = "", tck = 0.02, axes = FALSE)
axis(1,tick = TRUE, tck = 0.02)
axis(2,tick = FALSE, labels = FALSE)
axis(3,tick = TRUE, labels = FALSE, tck = 0.02)
axis(4,tick = FALSE, labels = FALSE)
box()
par(new=TRUE)
for(i in 1994:2010) {
  surfaceStart <- paste0(i,"-10-01")
  surfaceEnd <- paste0(i+1, "-09-30")
  x <- EGRET:::makeDateInfo(windowSide = 7, 
                            surfaceStart, surfaceEnd,
                            firstQDate0 = "1981-08-06",
                            lastQDate0 = "2016-01-14")
  ns <- decimalDate(x$flowNormStart)
  ne <- decimalDate(x$flowNormEnd)
  fs <- decimalDate(x$flowStart)
  fe <- decimalDate(x$flowEnd)
  xn <- c(ns,ne)
  y <- c(i-1993,i-1993)
  plot(xn, y, type = "l", lwd = 3, col = "grey", 
       xlim = c(1980,2020), xaxs = "i", 
       ylim = c(0, 23), yaxs = "i",
       xlab = "", ylab = "", axes = FALSE)
  par(new=TRUE)
  xn <- c(fs,fe)
  plot(xn, y, type = "l", lwd = 5, col = "red", 
       xlim = c(1980,2020), xaxs = "i", 
       ylim = c(0, 23), yaxs = "i",
       xlab = "", ylab = "", axes = FALSE)
  if(i < 2010) par(new=TRUE)
}
abline(v = decimalDate("1981-08-06"), col = "blue", lwd = 2)
abline(v = decimalDate("2016-01-14"), col = "blue", lwd = 2)

## ----loadData, eval=FALSE-----------------------------------------------------
#  library(EGRET)
#  
#  eList <- Choptank_eList
#  
#  pairResults <- runPairs(eList,
#                          year1 = 1985, year2 = 2010,
#                          windowSide = 7)

## ----loadDataReal, echo=FALSE-------------------------------------------------
eList <- Choptank_eList
pairResults <- readRDS("pairResults.rds")


## ----showPercentages----------------------------------------------------------
concPercents <- attr(pairResults, "Other")[["PercentChangeConc"]]
concPercents

fluxPercents <- attr(pairResults, "Other")[["PercentChangeFlux"]]
fluxPercents


## ----tableOut-----------------------------------------------------------------
knitr::kable(pairResults, digits = 4)
# note that you don't have to use the kable function from knitr to 
# see the results, you can just give the command pairResults
# and you will get the output, it just won't look as nice as this

## ----tableOutYield------------------------------------------------------------
pairResultsYield <- pairResults * c(1, 1000000 / eList$INFO$drainSqKm )
knitr::kable(pairResultsYield, digits = 4)


## -----------------------------------------------------------------------------
attr(pairResults, "yearPair")
attr(pairResults, "dateInfo")
attr(pairResults, "SampleBlocks")
attr(pairResults, "Other")

## -----------------------------------------------------------------------------
summary(eList$Daily$Date)
summary(eList$Sample$Date)

## ----pairs2, eval=FALSE-------------------------------------------------------
#  pairResults2 <- runPairs(eList, year1 = 1985, year2 = 2010,
#                           windowSide = 7, flowBreak = TRUE,
#                           Q1EndDate = "1995-05-31", wall = TRUE,
#                           sample1EndDate = "1995-05-31",
#                           QStartDate = "1979-10-01",
#                           QEndDate = "2010-09-30",
#                           paStart = 4, paLong = 5)

## ----loadPair2, echo=FALSE----------------------------------------------------
pairResults2 <- readRDS("pairResults2.rds")


## ----pairResultsAttrs---------------------------------------------------------
attr(pairResults2, "yearPair") 
attr(pairResults2, "dateInfo") 
attr(pairResults2, "SampleBlocks") 
attr(pairResults2, "Other")

## ----eval=FALSE---------------------------------------------------------------
#  eListOut <- runSeries(eList, windowSide,
#                        surfaceStart = NA, surfaceEnd = NA,
#                        flowBreak = FALSE, Q1EndDate = NA,
#                        QStartDate = NA, QEndDate = NA,
#                        wall = FALSE, oldSurface = FALSE,
#                        sample1EndDate = NA,
#                        sampleStartDate = NA, sampleEndDate = NA,
#                        paStart = 10, paLong = 12,
#                        minNumObs = 100, minNumUncen = 50,
#                        windowY = 7, windowQ = 2,
#                        windowS = 0.5, edgeAdjust = TRUE,
#                        verbose = TRUE)

## ----series1, eval=FALSE------------------------------------------------------
#  eListOut <- runSeries(eList, windowSide = 7, verbose = FALSE)

## ----series1run, eval=FALSE---------------------------------------------------
#  tableResults(eListOut)
#  plotConcHist(eListOut)
#  plotFluxHist(eListOut)
#  tableChange(eListOut, yearPoints = c(1985, 1995, 2010))

## ----series2, eval=FALSE------------------------------------------------------
#  eListOut <- runSeries(eList, windowSide = 9, verbose = FALSE)
#  plotConcHist(eListOut)
#  plotFluxHist(eListOut)
#  tableChange(eListOut, yearPoints = c(1985, 1995, 2010))

## ---- echo = FALSE------------------------------------------------------------
eListOut <- readRDS("eListOut.rds")
eList <- Choptank_eList

## ----loadDataPretendGreen, echo=TRUE, eval=FALSE------------------------------
#  
#  siteID <- "09234500"
#  parameter_cd<-"00940" #5 digit USGS code
#  Sample <- readNWISSample(siteID,parameter_cd,"1956-10-04","2018-02-01")
#  Daily <- readNWISDaily(siteID,"00060","1950-10-01","2018-04-01")
#  INFO<- readNWISInfo(siteID,parameter_cd, interactive = FALSE)
#  INFO$shortName <- "Green River near Greendale, UT"
#  eList_Green <- mergeReport(INFO, Daily, Sample)

## ----runSeries, eval=FALSE----------------------------------------------------
#  eListOut <- runSeries(eList_Green, windowSide = 12,
#                        flowBreak = TRUE, Q1EndDate = "1963-03-31",
#                        wall = TRUE,  sample1EndDate = "1963-03-01",
#                        verbose = FALSE)

## ----greenOutRun--------------------------------------------------------------
plotConcHist(eListOut)
plotFluxHist(eListOut)
tableResults(eListOut)
tableChange(eListOut, yearPoints = c(1957, 1963, 1983, 2017))

## ----plotGreen----------------------------------------------------------------
eListOut <- blankTime(eListOut, 
                      startBlank = "2000-10-01", 
                      endBlank = "2012-09-30")
plotConcHist(eListOut)
plotFluxHist(eListOut)

## ---- fig.height = 6, fig.width = 11------------------------------------------
plotContours(eListOut, 1957, 2017, 10, 100, 
             contourLevels = seq(0,55,5), flowDuration = FALSE)

## ---- fig.height = 6, fig.width = 11------------------------------------------
plotContours(eListOut, 1961, 1966, 10, 100, 
             contourLevels = seq(0,55,5), flowDuration = FALSE)

## ---- fig.height = 6, fig.width = 11------------------------------------------
plotContours(eListOut, 1964, 1984, 10, 100, 
             contourLevels = seq(0,55,5), flowDuration = FALSE)

## ----runWall, eval = FALSE----------------------------------------------------
#  eListOutNoWall <- runSeries(eList_Green, windowSide = 12,
#    flowBreak = TRUE, Q1EndDate = "1963-03-31",
#    wall = FALSE, verbose = FALSE)
#  
#  plotContours(eListOutNoWall, 1961, 1966, 10, 100,
#               contourLevels = seq(0,55,5), flowDuration = FALSE)
#  

## ----eval=FALSE---------------------------------------------------------------
#  groupResults <- runGroups(eList, windowSide,
#                            group1firstYear, group1lastYear,
#                            group2firstYear, group2lastYear,
#                            surfaceStart = NA, surfaceEnd = NA,
#                            flowBreak = FALSE, Q1EndDate = NA,
#                            QStartDate = NA, QEndDate = NA,
#                            wall = FALSE, oldSurface = FALSE,
#                            fractMin = 0.75, sample1EndDate = NA,
#                            sampleStartDate = NA, sampleEndDate = NA,
#                            paStart = 10, paLong = 12,
#                            minNumObs = 100, minNumUncen = 50,
#                            windowY = 7, windowQ = 2, windowS = 0.5,
#                            edgeAdjust = TRUE, verbose = TRUE)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  groupResults <- runGroups(eList,
#                            group1firstYear = 1995, group1lastYear = 2004,
#                            group2firstYear = 2005, group2lastYear = 2010,
#                            windowSide = 7, wall = TRUE,
#                            sample1EndDate = "2004-10-30",
#                            paStart = 4, paLong = 2, verbose = FALSE)

## ----groupResultsAttr, eval=FALSE---------------------------------------------
#  attr(groupResults, "groupInfo")
#  attr(groupResults, "dateInfo")
#  attr(groupResults, "SampleBlocks")
#  attr(groupResults, "Other")


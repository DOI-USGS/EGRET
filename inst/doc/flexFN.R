## ------------------------------------------------------------------------
# Note that this assumes you have loaded Laura's new version of EGRET
#  If you haven't done so already 
# you need to install the package "devtools"
# and then run this command:  devtools::install_github("ldecicco-USGS/EGRET", ref = "flex_fn")
# it also assumes that you have done these commands
library(EGRET)
library(EGRETci)
eList <- Choptank_Phos



## ------------------------------------------------------------------------
pairResults <- runPairs(eList, year1 = 1985, year2 = 2014, windowSide = 7)

## ------------------------------------------------------------------------
attr(pairResults, "yearPair") 

## ------------------------------------------------------------------------
attr(pairResults, "FDblocks") 

## ------------------------------------------------------------------------
attr(pairResults, "SampleBlocks") 

## ------------------------------------------------------------------------
attr(pairResults, "Other") 

## ------------------------------------------------------------------------

multiplier <- c(1, 1000000 / eList$INFO$drainSqKm)

pairResultsKM2 <- pairResults * multiplier

pairResultsKM2

## ------------------------------------------------------------------------
summary(eList$Daily$Date)
summary(eList$Sample$Date)

## ------------------------------------------------------------------------
pairResults2 <- runPairs(eList,
                         year1 = 1985, year2 = 2014,
                         windowSide = 0, 
                         wall = TRUE,
                         sample1EndDate = "1992-06-01",
                         paStart = 4, paLong = 5)

attr(pairResults2, "yearPair")
attr(pairResults2, "FDblocks")
attr(pairResults2, "SampleBlocks")
attr(pairResults2, "Other")

## ------------------------------------------------------------------------
# source("~/Dropbox/NewFFN/V1/runPairsBootAltC.R")
# pairsBootOut <- runPairsBootAltC(eList, pairResults, nBoot = 3)
# 
# # now we can view the results stored in pairsBootOut
# 
# pairsBootOut

## ----series--------------------------------------------------------------
eListOut <- runSeries(eList, 
                      windowSide = 7)

## ------------------------------------------------------------------------
eList <- Choptank_Phos

eListOut <- runSeries(eList, windowSide = 7,
                      surfaceStart = "1984-10-01", 
                      surfaceEnd = "2014-09-30")

## ------------------------------------------------------------------------
# eListBad <- makeSeriesOutputs(eListOut)

## ------------------------------------------------------------------------
eListOut$INFO
attr(eListOut$INFO, "segmentInfo")

## ------------------------------------------------------------------------
eListOut11 <- runSeries(eList, 
                        surfaceStart = "1984-10-01", 
                        surfaceEnd = "2014-09-30", 
                        windowSide = 12)
# eListBad11 <- makeSeriesOutputs(eListOut11)

## ----eval=FALSE----------------------------------------------------------
#  eList <- Choptank_Phos
#  
#  
#  groupResults <- runGroups(eList,
#                            firstDaySurface1 = "1984-10-01",
#                            lastDaySurface1 = "1999-09-30",
#                            lastDaySurface2 = "2014-09-30")

## ----eval=FALSE----------------------------------------------------------
#  attr(groupResults, "surfaceDates")
#  attr(groupResults, "FDblocks")
#  attr(groupResults, "OtherParams")

## ----eval=FALSE----------------------------------------------------------
#  groupResults <- runGroups(eList,
#                            firstDaySurface1 = "1984-10-01",
#                            lastDaySurface1 = "1999-09-30",
#                            lastDaySurface2 = "2014-09-30",
#                            firstQDay1 = "1984-10-01",
#                            lastQDay1 = "1999-09-30",
#                            firstQDay2 = "1999-10-01",
#                            lastQDay2 = "2014-09-30")
#  attr(groupResults, "surfaceDates")
#  attr(groupResults, "FDblocks")
#  attr(groupResults, "OtherParams")


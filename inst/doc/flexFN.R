## ------------------------------------------------------------------------
# Note that this assumes you have loaded Laura's new version of EGRET
#  If you haven't done so already 
# you need to install the package "devtools"
# and then run this command:  devtools::install_github("ldecicco-USGS/EGRET", ref = "flex_fn")
# it also assumes that you have done these commands
library(EGRET)
library(EGRETci)
eList <- Choptank_Phos



## ------------------------------------------------------------------
pairResults <- runPairs(eList, year1 = 1985, year2 = 2014)

## ------------------------------------------------------------------
attr(pairResults, "yearPair") # the values are paStart, paLong, year1, and year2
attr(pairResults, "FDblocks") # the values are the start and end of Daily0, then the start and end of Daily1, and then Daily2
attr(pairResults, "SampleBlocks") # the values are the start and end of the two Sample groups
attr(pairResults, "Other") # minNumObs, minNumUncen, windowY, windowQ, windowS, wall, edgeAdjust (for the last two 0 = F, 1 = T)

## ------------------------------------------------------------------

multiplier <- c(1, 1000000 / eList$INFO$drainSqKm)

pairResultsKM2 <- pairResults * multiplier

pairResultsKM2

## ------------------------------------------------------------------
summary(eList$Daily$Date)
summary(eList$Sample$Date)

## ------------------------------------------------------------------
pairResults2 <- runPairs(eList,
                         year1 = 1985, year2 = 2014,
                         windowSide = 0, wall = TRUE,
                         lastDaySample1 = "1995-05-31",
                         firstQDate0 = "1979-10-01",
                         lastQDate0 = "2017-09-30",
                         firstQDate1 = "1979-10-01",
                         lastQDate1 = "1995-05-31",
                         firstQDate2 = "1995-06-01",
                         lastQDate2 = "2017-09-30",
                         paStart = 4, paLong = 5)

attr(pairResults2, "yearPair")
attr(pairResults2, "FDblocks")
attr(pairResults2, "SampleBlocks")
attr(pairResults2, "Other")

## ------------------------------------------------------------------
# source("~/Dropbox/NewFFN/V1/runPairsBootAltC.R")
# pairsBootOut <- runPairsBootAltC(eList, pairResults, nBoot = 3)
# 
# # now we can view the results stored in pairsBootOut
# 
# pairsBootOut

## ------------------------------------------------------------------

# groupResults <- runGroups(eList, firstDaySurface1 = "1984-10-01", lastDaySurface1 = "1999-09-30",
#                           lastDaySurface2 = "2014-09-30")
# # we can look at the attributes of groupResults so we know how it was created
# attr(groupResults, "surfaceDates")
# attr(groupResults, "FDblocks")
# attr(groupResults, "OtherParams")

## ------------------------------------------------------------------
# groupResults <- runGroups(eList, firstDaySurface1 = "1984-10-01", lastDaySurface1 = "1999-09-30",
#                           lastDaySurface2 = "2014-09-30", firstQDay1 = "1984-10-01", lastQDay1 = "1999-09-30",                           firstQDay2 = "1999-10-01", lastQDay2 = "2014-09-30")
# attr(groupResults, "surfaceDates")
# attr(groupResults, "FDblocks")
# attr(groupResults, "OtherParams")


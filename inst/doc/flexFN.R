## ----pairs---------------------------------------------------------------
library(EGRET)
eList <- Choptank_Phos
year1 <- 1985
year2 <- 2014

pairOut <- runPairs(eList, year1, year2)


## ----changeUnits---------------------------------------------------------
multiplier <- c(1, 1000000 / eList$INFO$drainSqKm)

pairResultsKM2 <- pairOut * multiplier

pairResultsKM2


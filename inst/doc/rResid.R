## ------------------------------------------------------------------------
library(EGRET)
eList <- Arkansas_eList
plotConcTime(eList)

## ------------------------------------------------------------------------
plotConcQ(eList,qUnit=4)

## ------------------------------------------------------------------------
plotResidQ(eList,qUnit=4)

## ------------------------------------------------------------------------
eList <- makeAugmentedSample(eList)
plotConcQ(eList, qUnit = 4, rResid = TRUE)
# now do it all over again
eList <- makeAugmentedSample(eList)
plotConcQ(eList, qUnit = 4, rResid = TRUE)

## ------------------------------------------------------------------------
plotResidTime(eList, rResid = TRUE)
plotResidQ(eList, qUnit = 4, rResid = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  eList <- makeAugmentedSample(eList)

## ----fig.height=8, fig.width=8-------------------------------------------
multiPlotDataOverview(eList, qUnit = 4, rResid = TRUE)

## ----fig.height=10, fig.width=8------------------------------------------
fluxBiasMulti(eList, qUnit = 4, fluxUnit = 9, rResid = TRUE)


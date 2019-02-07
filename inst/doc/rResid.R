## ----echo=FALSE----------------------------------------------------------
library(EGRET)
eList <- Arkansas_eList

## ---- echo=FALSE---------------------------------------------------------
plotConcTime(eList)

## ---- echo=FALSE---------------------------------------------------------
plotConcQ(eList,qUnit=4)

## ---- echo=FALSE---------------------------------------------------------
plotResidQ(eList,qUnit=4)

## ----echo=FALSE----------------------------------------------------------
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
library(truncnorm)
x <- seq(-8,1,0.01)
z <- dtruncnorm(x,b=1)
x[902] <- 1
z[902] <- 0
area <- 0.01*sum(z)
z <- z/area
plot(x,z,xlim=c(-4,2),type="l",ylim=c(0,1.6),xlab = "log of concentration",
     ylab = "probability density",main="Truncated normal density function\nfor the log of concentration, mean 0, standard deviation 1\ncensoring threshold is at +1 log units",cex.main=1.1)
polygon(x,z,density=40)
x <- seq(-8,-1,0.01)
z <- dtruncnorm(x,b=-1)
x[702] <- -1
z[702] <- 0
area <- 0.01*sum(z)
z <- z/area
plot(x,z,xlim=c(-4,2),type="l",ylim=c(0,1.6),xlab = "log of concentration",
     ylab = "probability density",main="Truncated normal density function\nfor the log of concentration, mean 0, standard deviation 1\ncensoring threshold is at -1 log units",cex.main=1.1)
polygon(x,z,density=40)

## ------------------------------------------------------------------------
eList <- makeAugmentedSample(eList)
plotConcQ(eList, qUnit = 4, randomCensored = TRUE)
# now do it all over again
eList <- makeAugmentedSample(eList)
plotConcQ(eList, qUnit = 4, randomCensored = TRUE)

## ------------------------------------------------------------------------
plotResidTime(eList, randomCensored = TRUE)
plotResidQ(eList, qUnit = 4, randomCensored = TRUE)

## ----augment1, eval=FALSE------------------------------------------------
#  eList <- makeAugmentedSample(eList)

## ---- fig.height = 9, fig.width = 8--------------------------------------
multiPlotDataOverview(eList, qUnit = 4, randomCensored = TRUE)
fluxBiasMulti(eList, qUnit = 4, fluxUnit = 9, randomCensored = TRUE)


## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(EGRET)
opts_chunk$set(
  echo=TRUE,
  warning = FALSE,
  message = FALSE
)

## ----install, eval=FALSE, echo=TRUE-------------------------------------------
#  remotes::install_github("USGS-R/EGRET")

## ----eval = TRUE, echo = TRUE, warning=FALSE, message=FALSE-------------------
library(EGRET)

load("ChainBridge.TP.RData") 
# now we will run the WRTDSKalman estimation (using the defaults for now)
eList_K <- WRTDSKalman(eList, niter = 200)
print(summary(eList_K$Daily))

## ----eval = TRUE, echo = TRUE-------------------------------------------------
AnnualResults <- setupYears(eList_K$Daily)
plotWRTDSKalman(eList_K)

## ----antable, echo=TRUE-------------------------------------------------------
prettyAnnual <- AnnualResults[,c("DecYear", "Q", 
                 "Conc", "GenConc", "Flux", "GenFlux")]
kable(prettyAnnual, digits = c(0, 0, 3, 3, 0, 0), caption = "Units are cubic meters per second, milligrams per Liter, and metric tons per year")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
plotTimeSlice(eList_K, start = 2016, end = 2016.5, conc = TRUE)
plotTimeSlice(eList_K, start = 2016, end = 2016.5, conc = FALSE)
plotTimeSlice(eList_K, start = 1996.5, end = 1997, conc = TRUE)
plotTimeSlice(eList_K, start = 1996.5, end = 1997, conc = FALSE)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
eList_2 <- WRTDSKalman(eList, rho = 0.85, niter = 200)
print(attr(eList_2$Daily,"rho"))
print(attr(eList_2$Daily,"niter"))
AnnualResults2 <- setupYears(eList_2$Daily)
AnnualResults2 <- AnnualResults2[,c("GenConc", "GenFlux")]
Ratios <- (AnnualResults2 - AnnualResults[, c("GenConc", "GenFlux")]) / AnnualResults[, c("GenConc", "GenFlux")]
row.names(Ratios) <- round(AnnualResults$DecYear, 0)
kable(Ratios*100, digits = 1, caption = "Percent difference with a change in rho from 0.90 to 0.85")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
eList3 <- WRTDSKalman(eList, niter = 200, seed = 1)
AnnualResults3 <- setupYears(eList3$Daily)
AnnualResults3 <- AnnualResults3[,c( "GenConc", "GenFlux")]
Ratios <- (AnnualResults3 - AnnualResults[, c("GenConc", "GenFlux")]) / AnnualResults[, c("GenConc", "GenFlux")]
row.names(Ratios) <- round(AnnualResults$DecYear, 0)
kable(Ratios*100, digits = 1, caption = "Percent difference with using a different random number seed")

## ----EVAL = TRUE, echo = TRUE-------------------------------------------------
plotConcHist(eList_K, plotAnnual = FALSE, plotGenConc = TRUE)
plotFluxHist(eList_K, plotAnnual = FALSE, plotGenFlux = TRUE, fluxUnit = 8)

## ----EVAL = TRUE, echo = TRUE-------------------------------------------------
monthlyResults <- calculateMonthlyResults(eList_K)
plot(monthlyResults$DecYear, monthlyResults$GenFlux/1000, type = "l", xaxs = "i", xlim = c(1980,2020), yaxs = "i", ylim = c(0,90), las = 1, tck = 0.02, xlab = "", ylab = "Flux, in metric tons per day", main = "Potomac River at Chain Bridge, Washington, DC\nTotal Phosphorus Flux, by Month")
Month <- monthlyResults$Month
Year <- monthlyResults$Year
DecYear <- monthlyResults$DecYear
GenFlux <- monthlyResults$GenFlux/1000
monthFluxOut <- data.frame(Month, Year, DecYear, GenFlux)
head(monthFluxOut)


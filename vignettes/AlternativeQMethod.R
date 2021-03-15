## ----echo = TRUE--------------------------------------------------------------
library(EGRET)
library(zoo) # zoo is a time series package, needed for rolling means

##################
sta <- "09234500"
param <- "70301"
startDate <- "1966-10-01"
endDate <- "2000-09-30"
INFO <- readNWISInfo(sta, param, interactive = FALSE)
Daily <- readNWISDaily(sta, "00060", startDate, endDate)
Sample <- readNWISSample(sta, param, startDate, endDate)
eList <- mergeReport(INFO, Daily, Sample)
INFO <- eList$INFO  # we will need this later
Sample <- eList$Sample  # we will need this later
multiPlotDataOverview(eList, logScaleConc = FALSE)
# let's also see what a little slice of the discharge record looks like
plotQTimeDaily(eList, yearStart = 1975, yearEnd = 1979, qLower = 0, qUnit = 2, lwd = 1)
eListRegular <- modelEstimation(eList)
fluxBiasMulti(eListRegular)
# let's see how the model worked out going into and coming out of a severe drought year
plotConcTimeDaily(eListRegular, yearStart = 1977.0, yearEnd = 1979.0)

## ----echo = TRUE--------------------------------------------------------------
errorStats(eListRegular)

## ----echo = TRUE--------------------------------------------------------------
staUp <- "09217000"
startUp <- "1961-10-01"
DailyUp <- readNWISDaily(staUp, "00060", startUp, endDate) 
# we can take a look at the record here
plot(DailyUp$Date, DailyUp$Q, type = "l", ylim = c(0, 500), yaxs = "i", tck = 0.02)

## ----echo = TRUE--------------------------------------------------------------
Daily <- eListRegular$Daily
Daily$Qa <- Daily$Q
DailyUpTemp <- DailyUp
DailyUpTemp$Q <- rollmean(DailyUp$Q,365, fill = NA, align = "right")
# note that fill = NA means that if we are trying to compute the rolling mean and there
# aren't enough prior values to do it we just put in an NA
# align = "right" means that we associate the rolling mean with the "right most" (final) date
DailyUpTemp <- subset(DailyUpTemp, Date >= startDate)
# let's check that DailyUpTemp is exactaly as long as Daily
length(DailyUpTemp$Q)
length(Daily$Q)
Daily$Q <- DailyUpTemp$Q
Daily$LogQ <- log(Daily$Q)
# now we create a new eList that uses this rolling mean discharge
eListRoll <- mergeReport(INFO, Daily, Sample)
# here is the time series of the 1-year rolling mean, plotted on the same scale as before
plot(eListRoll$Daily$Date, eListRoll$Daily$Q, type = "l", ylim = c(0, 500), yaxs = "i", tck = 0.02)

## ----echo = TRUE--------------------------------------------------------------
iqr <- IQR(eListRoll$Daily$LogQ)
iqr
# for comparison let's look at the IQR for the original model we estimated
IQR(eList$Daily$LogQ)
# now let's estimate the new model using the rolling mean in place of the daily discharge
eListRoll <- modelEstimation(eListRoll, windowQ = 0.5 * iqr, verbose = FALSE)
# let's see how this new model does at estimating the concentration for that 1977-1978 period
# that we looked at before
plotConcTimeDaily(eListRoll, yearStart = 1977.0, yearEnd = 1979.0)
# recall that with the original model the Rsq value was 0.583
errorStats(eListRoll)

## ----echo = TRUE--------------------------------------------------------------
Daily <- eList$Daily
Daily$Qa <- Daily$Q
DailyUpTemp <- DailyUp
DailyUpTemp$Q <- rollmean(DailyUp$Q, 182, fill = NA, align = "right")
DailyUpTemp <- subset(DailyUpTemp, Date >= startDate)
# let's check that DailyUpTemp is exactaly as long as Daily
length(DailyUpTemp$Q)
length(Daily$Q)
Daily$Q <- DailyUpTemp$Q
Daily$LogQ <- log(Daily$Q)
# now we create a new eList that uses this rolling mean discharge
eListRoll <- mergeReport(INFO, Daily, Sample)
# here is the time series of the half-year rolling mean, plotted on the same scale as before
plot(eListRoll$Daily$Date, eListRoll$Daily$Q, type = "l", ylim = c(0, 500), yaxs = "i", tck = 0.02)
iqr <- IQR(eListRoll$Daily$LogQ)
iqr
# now let's estimate the new model using the rolling mean in place of the daily discharge
eListRoll <- modelEstimation(eListRoll, windowQ = 0.5 * iqr, verbose = FALSE)
# let's see how this new model does at estimating the concentration for that 1977-1978 period
# that we looked at before
plotConcTimeDaily(eListRoll, yearStart = 1977.0, yearEnd = 1979.0)
# recall that with the original model the Rsq value was 0.583
errorStats(eListRoll)

## ----echo = TRUE--------------------------------------------------------------
Daily <- eList$Daily
Daily$Qa <- Daily$Q
DailyUpTemp <- DailyUp
DailyUpTemp$Q <- rollmean(DailyUp$Q, 730, fill = NA, align = "right")
DailyUpTemp <- subset(DailyUpTemp, Date >= startDate)
# let's check that DailyUpTemp is exactaly as long as Daily
length(DailyUpTemp$Q)
length(Daily$Q)
Daily$Q <- DailyUpTemp$Q
Daily$LogQ <- log(Daily$Q)
# now we create a new eList that uses this rolling mean discharge
eListRoll <- mergeReport(INFO, Daily, Sample)
# here is the time series of the 2-year rolling mean, plotted on the same scale as before
plot(eListRoll$Daily$Date, eListRoll$Daily$Q, type = "l", ylim = c(0, 500), yaxs = "i", tck = 0.02)
iqr <- IQR(eListRoll$Daily$LogQ)
iqr
# now let's estimate the new model using the rolling mean in place of the daily discharge
eListRoll <- modelEstimation(eListRoll, windowQ = 0.5 * iqr, verbose = FALSE)
# let's see how this new model does at estimating the concentration for that 1977-1978 period
# that we looked at before
plotConcTimeDaily(eListRoll, yearStart = 1977.0, yearEnd = 1979.0)
# recall that with the original model the Rsq value was 0.583
errorStats(eListRoll)

## ----echo = TRUE--------------------------------------------------------------
plotConcHist(eListRoll)
# but using plotFluxHist() would be meaningless, because the Q variable is the rolling mean and not the daily Q


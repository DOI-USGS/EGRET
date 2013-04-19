library(dataRetrival)
library(EGRET)
annualSeries <- exannualSeries
plotFlowSingle(8)

Sample <- exSample
INFO <- exINFO
plotResidTime()
plotConcQ()
plotConcPred()
plotConcTimeDaily(2001,2010)
plotConcTime(qUnit = 1, qLower = 100, qUpper = 10000, paLong = 3, paStart = 4)
plotConcTime()
plotFluxPred(fluxUnit = 7)
plotFluxPred(fluxUnit = 'poundsDay')

yearStart <- 2001
yearEnd <- 2010
AnnualResults <- exAnnualResults
INFO <- exINFO
plotFluxHist(yearStart, yearEnd, fluxUnit = 1)
plotFluxHist(yearStart, yearEnd, fluxUnit = 'kgDay')
plotConcHist(yearStart, yearEnd)
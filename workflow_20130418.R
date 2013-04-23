library(dataRetrival)
library(EGRET)
annualSeries <- exannualSeries
plotFlowSingle(8)

Sample <- exSample
INFO <- exINFO
plotResidTime()
plotConcQ()
plotConcPred()
plotConcTimeDaily(2001,2010, tinyPlot=FALSE)
plotConcTime(qUnit = 1, qLower = 100, qUpper = 10000, paLong = 3, paStart = 4)
plotConcTime()
plotFluxPred(fluxUnit = 7)
plotFluxPred(fluxUnit = 'poundsDay')
plotResidPred()
plotResidQ(qUnit=1)
plotQTimeDaily(1990,2000,qLower=10)
plotLogFluxPred(fluxUnit = 1)
plotLogFluxPred(fluxUnit = 'kgDay')
plotLogConcPred()
plotLogFluxQ(qUnit = 1, fluxUnit = 1)
plotLogFluxQ(fluxUnit = 'kgDay')
plotLogFluxQ()
plotLogConcQ(qUnit = 1)
plotLogConcQ(qUnit = 'thousandCfs')
plotFluxTimeDaily(2001,2009)

yearStart <- 2001
yearEnd <- 2010
AnnualResults <- exAnnualResults
INFO <- exINFO
plotFluxHist(yearStart, yearEnd, fluxUnit = 1)
plotFluxHist(yearStart, yearEnd, fluxUnit = 'kgDay')
plotConcHist(yearStart, yearEnd)


q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"
yearStart <- 2000
yearEnd <- 2010
Sample <- exSample
INFO <- exINFO
plotConcTimeSmooth(q1, q2, q3, centerDate, yearStart, yearEnd)

date1<-"2001-06-01"
date2<-"2009-06-01"
date3<-NA
qLow<-1
qHigh<-1000
Sample <- exSample
INFO <- exINFO
plotLogConcQSmooth(date1,date2,date3,qLow,qHigh)

date1<-"2001-06-01"
date2<-"2005-06-01"
date3<-"2010-06-01"
qLow<-1
qHigh<-1000
Sample <- exSample
INFO <- exINFO
plotConcQSmooth(date1,date2,date3,qLow,qHigh)

Daily <- exDaily
INFO <- exINFO
plotSDLogQ(window=3,printTitle=FALSE) 

INFO <- exINFO
annualSeries <- exannualSeries
plotFlowSingle(8,cex=0.7,lwd=1)
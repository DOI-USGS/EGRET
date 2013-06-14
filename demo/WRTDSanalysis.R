Daily <- ChopDaily
Sample <- ChopSample
INFO <- ChopINFO
surfaces <- exsurfaces

qBottom<-1
qTop<-5000
clevel<-seq(0,2,0.5)
maxDiff<-2
yearStart <- 2008
yearEnd <- 2010
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"

plotConcPred()

plotConcTimeDaily(2008, 2010)

plotConcQSmooth(date1, date2, date3, qBottom, qTop, 
                concMax=1.5,qUnit=1)

fluxBiasEight(qUnit=1)

plotContours(yearStart,yearEnd,qBottom,qTop, 
             contourLevels = clevel,qUnit=1)

plotDiffContours(year0=2000,yearEnd,
                 qBottom,qTop,maxDiff,qUnit=1)
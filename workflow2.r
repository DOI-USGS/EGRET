library(dataRetrieval)
library(EGRET)
Sample <- exSample
Daily <- exDaily
INFO <- exINFO
AnnualResults <- setupYears()
surfaces <- exsurfaces
qBottom<-0.1
qTop<-100
clevel<-seq(0,2,0.5)
maxDiff<-0.8
yearStart <- 2008
yearEnd <- 2010
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"

plotResidQ()
plotConcPred()
genericEGRETDotPlot(hLine=TRUE,x=Daily$Date, y=Daily$Q, 
                    xlim=c(0,2), ylim=c(0,2),
                    xlab="Estimated X", ylab="Estimated Y",
                    xTicks=c(0,max(Daily$Date)),yTicks=c(0,max(Daily$Q)/2,max(Daily$Q))
                    )


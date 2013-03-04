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

x <- Daily$Date
y <- Daily$Q
xlim <- c(min(x),max(x))
ylim <- c(min(y),1.05*max(y)))
xlab <- "Date"
ylab <- "Flow"
xTicks <- pretty(xlim)
yTicks <- pretty(ylim)
genericEGRETDotPlot(hLine=TRUE,x=x, y=y, 
                    xlim=xlim, ylim=ylim,
                    xlab=xlab, ylab=ylab,
                    xTicks=xTicks, yTicks=yTicks,
                    plotTitle="Test\ntest\ntest"
                    )
siteID <- "01491000" #Choptank River at Greensboro, MD
startDate <- "1979-10-01"
endDate <- "2011-09-30"
param<-"00631"
Daily <- getDVData(siteID,"00060",startDate,endDate)
INFO<- getMetaData(siteID,param,interactive=FALSE)
INFO$shortName <- "Choptank River"
Sample <- getSampleData(siteID,param,startDate,endDate)
Sample <- mergeReport()
plotLogFluxQ(cex.main=1.5)

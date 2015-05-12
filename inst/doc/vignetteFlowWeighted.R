## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(EGRET)
eList <- Choptank_eList
Daily <- eList$Daily


## ----fig.height=6, fig.width=8----------------------------
library(EGRET)
library(rkt)
eList <- Choptank_eList
Daily <- eList$Daily
AnnualResults <- setupYears(Daily) 
#  note, if you want some other period of analysis this can be done in the arguments to setupYears
plotConcHist(eList, plotFlowNorm = FALSE)
modConc <- lm(Conc~DecYear,data=AnnualResults) # linear regression
summary(modConc)
mannKendallConc <- rkt(AnnualResults$DecYear,AnnualResults$Conc) 
# Mann-Kendall trend
mannKendallConc

## ----fig.height=6, fig.width=8----------------------------
library(EGRET)
library(rkt)
eList <- Choptank_eList
Daily <- eList$Daily
plotFluxHist(eList, plotFlowNorm = FALSE)
modFlux <- lm(Flux~DecYear,data=AnnualResults)
summary(modFlux)
mannKendallFlux <- rkt(AnnualResults$DecYear,AnnualResults$Flux)
mannKendallFlux

## ----echo=TRUE, eval=FALSE--------------------------------
#  eList <- modelEstimation(eList)

## ----eval=TRUE, echo=TRUE, tidy=TRUE----------------------
library(EGRET)
library(rkt)
eList <- Choptank_eList

flowWeightedYears<-function (localDaily, paLong = 12, paStart = 10){
    numDays <- length(localDaily$MonthSeq)
    firstMonthSeq <- localDaily$MonthSeq[1]
    lastMonthSeq <- localDaily$MonthSeq[numDays]
    Starts <- seq(paStart, lastMonthSeq, 12)
    Ends <- Starts + paLong - 1
    StartEndSeq <- data.frame(Starts, Ends)
    StartEndSeq <- StartEndSeq[(StartEndSeq$Starts >= firstMonthSeq) & 
        (StartEndSeq$Ends <= lastMonthSeq), ]
    firstMonth <- StartEndSeq[1, 1]
    numYears <- length(StartEndSeq$Starts)
    DecYear <- rep(NA, numYears)
    Q <- rep(NA, numYears)
    Conc <- rep(NA, numYears)
    Flux <- rep(NA, numYears)
    FNConc <- rep(NA, numYears)
    FNFlux <- rep(NA, numYears)
    FWConc <- rep(NA, numYears)
    for (i in 1:numYears) {
        startMonth <- (i - 1) * 12 + firstMonth
        stopMonth <- startMonth + paLong - 1
        DailyYear <- localDaily[which(localDaily$MonthSeq %in% 
            startMonth:stopMonth), ]
        counter <- ifelse(is.na(DailyYear$ConcDay), 0, 1)
        if (length(counter) > 0) {
            good <- (sum(counter) > 25)
        }
        else {
            good <- FALSE
        }
        DecYear[i] <- mean(DailyYear$DecYear)
        Q[i] <- mean(DailyYear$Q)
        if (good) {
            Conc[i] <- mean(DailyYear$ConcDay, na.rm = TRUE)
            Flux[i] <- mean(DailyYear$FluxDay, na.rm = TRUE)
            FNConc[i] <- mean(DailyYear$FNConc, na.rm = TRUE)
            FNFlux[i] <- mean(DailyYear$FNFlux, na.rm = TRUE)
            FWConc[i] <- mean(DailyYear$ConcDay*DailyYear$Q, na.rm=TRUE)
            denom <- mean(DailyYear$ConcDay*DailyYear$Q/DailyYear$ConcDay,na.rm=TRUE)
            FWConc[i] <- FWConc[i] / denom        }
    }
    PeriodStart <- rep(paStart, numYears)
    PeriodLong <- rep(paLong, numYears)
    AnnualResults <- data.frame(DecYear, Q, Conc, FWConc, Flux, FNConc, 
        FNFlux, PeriodLong, PeriodStart)
    return(AnnualResults)
}
 
AnnualResults <- flowWeightedYears(eList$Daily)

## ----fig.height=6, fig.width=8, tidy=TRUE-----------------
library(EGRET)
library(rkt)
AnnualResults <- flowWeightedYears(eList$Daily)
modFWConc <- lm(FWConc~DecYear,data=AnnualResults) 
# linear regression
summary(modFWConc)
mannKendallFWConc <- rkt(AnnualResults$DecYear,AnnualResults$FWConc) 
# Mann-Kendall trend
mannKendallFWConc

## ---- fig.height=6, fig.width=8,tidy=TRUE-----------------
nYears <- length(AnnualResults$DecYear)
xlim <- c(AnnualResults$DecYear[1]-1,AnnualResults$DecYear[nYears]+1)
xTicks <- pretty(xlim)
ylim <- c(0,1.05*max(AnnualResults$Conc))
yTicks <- yPretty(ylim[2])
plotTitle = "Annual Mean Concentrations" 
# note that you can make more complex titles using
#    the approach used in the code for plotConcHist
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$Conc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="Year",ylab="Concentration in mg/L",plotTitle=plotTitle,xDate=TRUE)
abline(a=modConc$coef[1],b=modConc$coef[2],lwd=2)

## ---- fig.height=6, fig.width=8,tidy=TRUE-----------------
AnnualResults <- flowWeightedYears(eList$Daily)
nYears <- length(AnnualResults$DecYear)
xlim <- c(AnnualResults$DecYear[1]-1,AnnualResults$DecYear[nYears]+1)
xTicks <- pretty(xlim)
ylim <- c(0,1.05*max(AnnualResults$FWConc))
yTicks <- yPretty(ylim[2])
plotTitle = "Annual Flow-Weighted Mean Concentrations" 
# note that you can make more complex titles using
#    the approach used in the code for plotConcHist
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$FWConc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="Year",ylab="Concentration in mg/L",plotTitle=plotTitle,xDate=TRUE)
abline(a=modFWConc$coef[1],b=modFWConc$coef[2],lwd=2)


## ---- fig.height=6, fig.width=8,tidy=TRUE-----------------
AnnualResults <- flowWeightedYears(eList$Daily)
nYears <- length(AnnualResults$DecYear)
xlim <- c(AnnualResults$DecYear[1]-1,AnnualResults$DecYear[nYears]+1)
xTicks <- pretty(xlim)
yMax <- max(c(AnnualResults$Conc,AnnualResults$FWConc))
ylim <- c(0,1.05*yMax)
yTicks <- yPretty(ylim[2])
plotTitle = "Annual Mean Concentrations in Black\nFlow Weighted Mean in Red" 
# note that you can make more complex titles using
#    the approach used in the code for plotConcHist
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$Conc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="Year",ylab="Concentration in mg/L",plotTitle=plotTitle,xDate=TRUE)
abline(a=modConc$coef[1],b=modConc$coef[2],lwd=2)
par(new=TRUE)
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$FWConc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="",ylab="",plotTitle="",xDate=TRUE,col="red")
abline(a=modFWConc$coef[1],b=modFWConc$coef[2],lwd=2,col="red")

## ---- fig.height=6, fig.width=8,tidy=TRUE-----------------
lowConc <- loess(Conc~DecYear,data=AnnualResults,span=0.9)
lowFWConc <- loess(FWConc~DecYear,data=AnnualResults,span=0.9)
nYears <- length(AnnualResults$DecYear)
xlim <- c(AnnualResults$DecYear[1]-1,AnnualResults$DecYear[nYears]+1)
xTicks <- pretty(xlim)
yMax <- max(c(AnnualResults$Conc,AnnualResults$FWConc))
ylim <- c(0,1.05*yMax)
yTicks <- yPretty(ylim[2])
plotTitle = "Annual Mean Concentrations in Black\nFlow Weighted Mean in Red" 
# note that you can make more complex titles using
#    the approach used in the code for plotConcHist
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$Conc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="Year",ylab="Concentration in mg/L",plotTitle=plotTitle,xDate=TRUE)
par(new=TRUE)
genericEGRETDotPlot(AnnualResults$DecYear,lowConc$fit,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="",ylab="",plotTitle="",xDate=TRUE,type="l",lwd=2)
par(new=TRUE)
genericEGRETDotPlot(AnnualResults$DecYear,AnnualResults$FWConc,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="",ylab="",plotTitle="",xDate=TRUE,col="red")
par(new=TRUE)
genericEGRETDotPlot(AnnualResults$DecYear,lowFWConc$fit,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="",ylab="",plotTitle="",xDate=TRUE,type="l",lwd=2,col="red")

## ---- eval=TRUE,tidy=TRUE,fig.height=7,fig.width=5--------
packagePath <- system.file("extdata", package="EGRET")
filePath <- file.path(packagePath, "James.rds")
eList <- readRDS(filePath)

Daily <- eList$Daily
Daily$group <- ifelse(Daily$Date>="1992-10-01","Second","First")
title <- paste(eList$INFO$shortName,"\nDischarge for two periods\nWY 1971-1992 and 1993-2014")
boxplot(Daily$Q~Daily$group,log="y",main=title,xlab="",ylab="Discharge in cms")

## ----eval=TRUE, tidy=TRUE, fig.height=7, fig.width=5------
filePath <- file.path(packagePath, "Susquehanna.rds")
eList <- readRDS(filePath)

Daily <- eList$Daily
Daily$group <- ifelse(Daily$Date>="1998-10-01","Second","First")
title <- paste(eList$INFO$shortName,"\nDischarge for two periods\nWY 1985-1998 and 1999-2013")
boxplot(Daily$Q~Daily$group,log="y",main=title,xlab="",ylab="Discharge in cms") 


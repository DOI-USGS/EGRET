## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(EGRET)


## ---- eval = TRUE, echo = TRUE, tidy = TRUE---------------

setupSeasons <- function(localDaily, paLong, paStart){
  SeasonResults <- setupYearsPlus(localDaily, paLong = paLong, paStart = paStart)
  AnnualResults <- setupYearsPlus(localDaily, paLong = 12, paStart = paStart)
  numYears <- length(AnnualResults$DecYear)
  divide <- 1000000
  DecYear <- AnnualResults$DecYear
  Year <- trunc(DecYear)
  FluxYear <- AnnualResults$Flux*AnnualResults$Counts/divide
  FNFluxYear <- AnnualResults$FNFlux*AnnualResults$Counts/divide
  FluxSeason <- SeasonResults$Flux[1:numYears]*SeasonResults$Counts[1:numYears]/divide
  FNFluxSeason <- SeasonResults$FNFlux[1:numYears]*SeasonResults$Counts[1:numYears]/divide
  pctFlux <- ifelse(is.na(FluxYear)|is.na(FluxSeason),NA,100*FluxSeason/FluxYear)
  pctFNFlux <- ifelse(is.na(FNFluxYear)|is.na(FNFluxSeason),NA,100*FNFluxSeason/FNFluxYear)
  seasonPctResults <- data.frame(DecYear,Year,FluxYear,FNFluxYear,FluxSeason,FNFluxSeason,pctFlux,pctFNFlux)
  seasonLong <- rep(paLong,numYears)
  seasonStart <- rep(paStart,numYears)
  seasonPctResults <- data.frame(seasonPctResults,seasonLong,seasonStart)
  return(seasonPctResults)
}

setupYearsPlus <- function (localDaily, paLong = 12, paStart = 10){
  
# This is an augmented version of setupYears 
#  that also returns the number of good days in each year or season
  
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
  Counts <- rep(NA, numYears)
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
      Counts[i] <- sum(counter)
    }
  }
  PeriodStart <- rep(paStart, numYears)
  PeriodLong <- rep(paLong, numYears)
  AnnualResults <- data.frame(DecYear, Q, Conc, Flux, FNConc, 
                              FNFlux, PeriodLong, PeriodStart, Counts)
  return(AnnualResults)
}


## ---- eval = TRUE, echo = TRUE, tidy =TRUE----------------
library(EGRET)
eList <- Choptank_eList
Daily <- eList$Daily
seasonPctResults <- setupSeasons(Daily, paLong = 3, paStart = 12)

## ---- echo=TRUE, eval=TRUE, fig.width=20------------------
seasonPctResults

## ---- fig.height=6, fig.width=8,tidy=TRUE-----------------
nYears <- length(seasonPctResults$DecYear)
xlim <- c(seasonPctResults$DecYear[1]-1,seasonPctResults$DecYear[nYears]+1)
xTicks <- pretty(xlim)
ylim <- c(0,100)
yTicks <- seq(0,100,10)
plotTitle = paste("Seasonal Flux as a Percent of Annual Flux\n",eList$INFO$shortName,eList$INFO$paramShortName,"\nSolid line is percentage of flow normalized flux") 
genericEGRETDotPlot(seasonPctResults$DecYear,seasonPctResults$pctFlux,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="Year",ylab="Percentage of Annual Flux",plotTitle=plotTitle,xDate=TRUE,cex=1.5)
par(new=TRUE)
genericEGRETDotPlot(seasonPctResults$DecYear,seasonPctResults$pctFNFlux,xlim=xlim,ylim=ylim,xTicks=xTicks,yTicks=yTicks,xaxs="i",yaxs="i",xlab="",ylab="",plotTitle=plotTitle,xDate=TRUE,cex=1.5,type="l",col="green",lwd=2)

## ---- eval=TRUE, echo=TRUE--------------------------------
sumYears <- sum(seasonPctResults$FluxYear[21:31])
# This is the total flux for all years 
# in the period of interest in millions of kg
sumYears

sumSeasons <- sum(seasonPctResults$FluxSeason[21:31])
# This is the total seasonal flux for all years 
# of the period of interest in millions of kg 
sumSeasons 

avePct <- 100 * sumSeasons / sumYears
# This is the percentage of the total flux for the
# period of interest that was transported during the season of interest
avePct


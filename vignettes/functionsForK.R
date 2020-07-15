##########################
# functionsForK is a set of add-ons to EGRET
# for doing the WRTDS_K estimation
# All fuctions (except for one) written by
# Robert M. Hirsch, in 2019
# this file is December 17, 2019
#############################
# makeDailyK
#  This function takes an existing eList 
#  Including the estimated model (the surfaces object in the eList)
#  And produces the daily WRTDS_K estimates of concentration and flux
# it requires MASS, EGRET, lubridate, and dplyr
###############################
makeDailyK <- function(eList, rho = 0.90, niter = 200, seed = 376168){
  set.seed(seed)
  # this part is to set up the array of runs of missing values
  localEList <- cleanUp(eList)
  localDaily <- populateDailySamp(localEList$Daily, localEList$Sample)
  numDays <- length(localDaily$Date)
  numDaysP <- numDays + 1
  # set up DailyGen which will hold the daily generated flux values for all days and all iterations
  DailyGen <- rep(0, numDays * niter)
  dim(DailyGen) <- c(numDays, niter)
  #   x is a vector of the standardized residuals for each day
  #   most of the elements of x will be NA but those from sampled days will have values
  x <- localDaily$stdResid
  #    xP is x that has been padded with a 0 at the start and a 0 at the end
  #    thus it is a vector that always starts and ends with non-missing values
  xP <- c(0,x,0)
  zz <- rle(is.na(xP))
  #    zz$lengths is a vector of run lengths
  #    zz$values is a vector of the values: TRUE means it is a run of missings, 
  #       FALSE means it is a run of non-missing values
  nRuns <- length(zz$lengths)
  zends <- cumsum(zz$lengths)
  nRunsM <- nRuns - 1
  zstarts <- c(0,zends[1:nRunsM])
  # doGap is the indexs of the runs that are missing values (it is just the even integers)
  doGap <- seq(2,nRunsM,2)
  # numGap is the number of groups of missing values to be filled in
  numGap <- length(doGap)
  # now we are ready to do the iterations to generate the series
  for(iter in 1:niter){
    localEList <- cleanUp(eList)
    # this next step adds a trueConc column to Daily, and it is NA if there is no sample value
    # it also adds the stdResid column to Daily
    localDaily <- populateDailySamp(localEList$Daily, localEList$Sample)
    x <- localDaily$stdResid
    #    xxP is x that has been padded with a 0 at the start and a 0 at the end
    #    thus it is a vector that always starts and ends with non-missing values
    xxP <- c(0,x,0)
    # now we are going to loop through all the gaps that need to be filled in
    for(i in 1:numGap) {
      iGap<-doGap[i]
      startFill<-zstarts[iGap]
      endFill<-zends[iGap]+1
      nFill<-zz$length[iGap]+2
      xfill<-genmissing(xxP[startFill],xxP[endFill],rho,nFill)
      xxP[startFill:endFill]<-xfill}
    # now we need to strip out the padded days
    xResid <- xxP[2:numDaysP]
    xConc <- exp((xResid*localDaily$SE)+localDaily$yHat)
    DailyGen[,iter] <- xConc * localDaily$Q * 86.4
  }
  # now we take means over all the iterations
  GenMean <- rep(NA, numDays)
  Daily <- eList$Daily
  for(i in 1 : numDays) {GenMean[i] <- mean(DailyGen[i,])}
  Daily$GenFlux <- GenMean
  Daily$GenConc <- Daily$GenFlux / (Daily$Q * 86.4)
  attr(Daily, "niter") <- niter
  attr(Daily, "rho") <- rho
  return(Daily)
}
#
#

genmissing<-function(X1,XN,rho,N){
  # this code was done by Tim Cohn
  #  X1 is the value before the gap
  #  XN is the value after the gap
  #  rho is the lag one autocorrelation
  #  N is the length of the sequence including X1 and XN 
  #     it is two more than the gap length
  #   it requires the MASS package
  C<-t(chol(rho^abs(outer(1:N,1:N, "-"))[c(1,N,2:(N-1)),c(1,N,2:(N-1))]))
  (C%*%c(ginv(C[1:2,1:2])%*%c(X1,XN),rnorm(N-2)))[c(1,3:N,2)]
}
#
#

####################################
# This function cleans up a Sample data frame
# It randomly picks one sample out of the multiple samples on a given day
# and it makes an augmented record substituting a random value on
# those days with censored data
#  Note, it must have an eList with a valid surfaces matrix 
# and the Sample data frame in the eList needs to have yHat and SE already calculated
####################################
cleanUp <- function(eList){
  Sample <- random_subset(eList$Sample, "Julian")
  eListClean <- as.egret(eList$INFO, eList$Daily, Sample, eList$surfaces)
  eListClean <- makeAugmentedSample(eListClean)
  Sample <- eListClean$Sample
  Sample$Uncen <- 1
  Sample$ConcLow <- Sample$rObserved
  Sample$ConcHigh <- Sample$rObserved
  Sample$ConcAve <- Sample$rObserved
  eListClean <- as.egret(eList$INFO, eList$Daily, Sample, eList$surfaces)
  return(eListClean)
}
#
#
####################################
# function written by Laura De Cicco October 2019
#  requires library(dplyr)
# makes a copy of a data frame but when there are
# multiple values with the specified col_name it randomly
# picks one of them and drops the others
# useage newSample <- random_subset(Sample, Julian)
###################################
random_subset <- function(df, col_name){
  
  dup_index <- unique(c(which(duplicated(df[[col_name]], fromLast = FALSE)), 
                        which(duplicated(df[[col_name]], fromLast = TRUE))))
  
  if(length(dup_index) == 0){
    return(df)
  }
  
  dup_index <- dup_index[order(dup_index)]
  
  unique_groups <- unique(df[[col_name]][dup_index])
  
  slice_index <- sapply(unique_groups, function(x){
    sample(which(df[[col_name]] == x), size = 1)
  })
  
  df_dups <- df[slice_index, ] 
  df_no_dups <- df[-dup_index,]
  
  subDF <- rbind(df_no_dups, df_dups)
  subDF <- subDF[order(subDF[[col_name]]),]
  
  return(subDF)
}
#
#
###########################
populateDailySamp<-function(localDaily=Daily,localSample=Sample) {
  numDays<-length(localDaily$Julian)
  numSamp<-length(localSample$Julian)
  trueConc <- rep(NA,numDays)
  trueFlux <- rep(NA, numDays)
  stdResid <- rep(NA,numDays)
  DailyOffset<-localDaily$Julian[1]-1
  for(samp in 1:numSamp){iday<-localSample$Julian[samp]-DailyOffset
  trueConc[iday]<-localSample$ConcAve[samp]
  trueFlux[iday] <- trueConc[iday] * localDaily$Q[iday] * 86.4
  stdResid[iday]<-(log(trueConc[iday])-localDaily$yHat[iday])/localDaily$SE[iday]
  }
  retDaily<-data.frame(localDaily,trueConc,trueFlux, stdResid)
  return(retDaily)
}
#
setupYearsKalmanFlux <- function (localDaily, paLong = 12, paStart = 10) 
{
# note that fluxes returned are the sum of the daily fluxes
# the units on the fluxes are all metric tons 
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
  ConcDay <- rep(NA, numYears)
  GenConc <- rep(NA, numYears)
  FluxDay <- rep(NA, numYears)
  GenFlux <- rep(NA, numYears)
  for (i in 1:numYears) {
    startMonth <- (i - 1) * 12 + firstMonth
    stopMonth <- startMonth + paLong - 1
    DailyYear <- localDaily[which(localDaily$MonthSeq %in% 
                                    startMonth:stopMonth), ]
    counter <- ifelse(is.na(DailyYear$FluxDay), 0, 1)
    if (length(counter) > 0) {
      good <- (sum(counter) == length(counter))
    }
    else {
      good <- FALSE
    }
    DecYear[i] <- mean(DailyYear$DecYear)
    Q[i] <- mean(DailyYear$Q)
    if (good) {
      ConcDay[i] <- mean(DailyYear$ConcDay, na.rm = TRUE)
      GenConc[i] <- mean(DailyYear$GenConc, na.rm = TRUE)
      FluxDay[i] <- sum(DailyYear$FluxDay, na.rm = TRUE) / 1000
      GenFlux[i] <- sum(DailyYear$GenFlux, na.rm = TRUE) / 1000
    }
  }
  AnnualResults <- data.frame(DecYear, Q, ConcDay, GenConc, FluxDay, GenFlux)
  attr(AnnualResults,"paStart") <- paStart
  attr(AnnualResults,"paLong") <- paLong
  return(AnnualResults)
}
#
#
#
computeAnnual <- function(eList, Daily, paStart = 10, paLong = 12) {
  # it provides a printed list of the annual values and a set of plots
  # if you don't want that output 
  # you can get the same thing with AnnualResults <- setupYearsKalmanFlux()
  # This function creates an annual series of results
  # The annual results can be for a specific season, specified by paStart and paLong
  # The default is the water year
  # FluxDay is the traditional regression estimate of Flux
  # GenFlux is the Kalman Filter estimate
  # ConcDay is the traditional regression estimate of Concentration 
  # GenConc is the Kalman Filter estimate
  AnnualResults <- setupYearsKalmanFlux(Daily, paStart = paStart, paLong = paLong)
  # in the print out Q is the annual mean value in m^3/s
  # the two flux values are in metric tons kilograms (1000 kg)
  print(eList$INFO$shortName)
  print(eList$INFO$paramShortName)
  period <- paste("paStart is",paStart," paLong is",paLong, sep = " ")
  print(period)
  print(AnnualResults)
  yMax <- 1.1 * max(AnnualResults$FluxDay, AnnualResults$GenFlux)
  nYears <- length(AnnualResults[,1])
  # first a plot of just the WRTDS estimate
  xMin <- floor(AnnualResults[1,1])
  xMax <- ceiling(AnnualResults[nYears,1])
  xlim <- c(xMin,xMax)
  title1 <- paste(eList$INFO$shortName,eList$INFO$paramShortName,
                  "\nAnnual Flux Estimates: WRTDS in red, WRTDS-K in green\n",period,sep="  ")
  title2 <- paste(eList$INFO$shortName,eList$INFO$paramShortName,
                  "\nComparison of the two flux estimates\n",period,sep="  ")
  #
  plot(AnnualResults$DecYear, AnnualResults$FluxDay, pch = 20, cex = 1.3, xlim = xlim, xaxs = "i",
       ylim = c(0, yMax), yaxs = "i", xlab = "", ylab = "Annual flux, metric tons", 
       main = title1, las = 1, col = "red",
       tck = 0.02, cex.main = 1.1, cex.lab = 0.95)
  par(new = TRUE)
  plot(AnnualResults$DecYear, AnnualResults$GenFlux, pch = 20, cex = 1.4, col = "green", xlim = xlim, xaxs = "i",
       ylim = c(0, yMax), yaxs = "i", xlab = "", ylab = "", main = "", las = 1, tck = 0.02, axes = FALSE)
  # scatter plot
  plot(AnnualResults$FluxDay, AnnualResults$GenFlux, pch = 20, cex = 1.3, col = "red", xlim = c(0, yMax), xaxs = "i",
       ylim = c(0, yMax), las = 1, yaxs = "i", xlab = "WRTDS estimate of annual flux, in metric tons", ylab = 
         "WRTDS_K estimate of annual flux, in metric tons", cex.main = 1.1, cex.lab = 0.95, cex.axis = 1.2, 
       main = title2)
  abline(a = 0, b = 1)
  return(AnnualResults)
}
#
#
#
plotTimeSlice <- function(eList, Daily, start, end){
  Daily <- subset(Daily,DecYear >= start & DecYear <= end)
  concHigh <- 1.1 * max(Daily$trueConc,Daily$GenConc,Daily$ConcDay,na.rm = TRUE)
  concLow <- 0.9 * min(Daily$trueConc,Daily$GenConc,Daily$ConcDay,na.rm = TRUE)
  fluxHigh <- 1.1 * max(Daily$trueFlux,Daily$GenFlux,Daily$FluxDay,na.rm = TRUE)
  fluxLow <- 0.9 * min(Daily$trueFlux,Daily$GenFlux,Daily$FluxDay,na.rm = TRUE)
  # figure out which data symbol to use, red for uncensored, brown for censored
  eList$Sample$color <- ifelse(eList$Sample$Uncen == 1, "red", "cyan4")
  par(tck = 0.02, las = 1)
  # first concentration, then flux
  name <- paste(eList$INFO$shortName, eList$INFO$paramShortName, sep = " ")
  ratio <- mean(Daily$GenConc) / mean(Daily$ConcDay)
  fratio <- format(ratio, digits = 2)
  concTitle <- paste(name,"\nConcentrations, Black is WRTDS, Green is WRTDS_K\nData in red, (rl in blue if <), Ratio of means is", fratio, sep = " ")
  
  plot(Daily$DecYear, Daily$ConcDay, log = "y", type = "l", las = 1, xlim = c(start, end), 
       xaxs = "i", ylim = c(concLow,concHigh), yaxs = "i", xlab = "", cex.main = 0.9, 
       ylab = "Concentration, in milligrams per Liter",
       main = concTitle)
  par(new = TRUE)
  plot(eList$Sample$DecYear, eList$Sample$ConcHigh, log = "y", pch = 20, cex = 1.1, col = eList$Sample$color, 
       xlim = c(start, end), xaxs = "i", ylim = c(concLow,concHigh), yaxs = "i", xlab = "",
       ylab = "", main = "", axes = FALSE)
  par(new = TRUE)
  plot(Daily$DecYear, Daily$GenConc, log = "y", type = "l", xlim = c(start, end), 
       xaxs = "i", ylim = c(concLow,concHigh), yaxs = "i", xlab = "", col = "green", 
       ylab = "", main = "", axes = FALSE)
  # flux graph
  ratio <- mean(Daily$GenFlux) / mean(Daily$FluxDay)
  fratio <- format(ratio, digits = 2)
  fluxTitle <- paste(name,"\nFlux, Black is WRTDS, Green is WRTDS_K\nData in red, (rl in blue if <), Ratio of means is", fratio, sep = " ")
  plot(Daily$DecYear, Daily$FluxDay, log = "y", type = "l", xlim = c(start, end), 
       xaxs = "i", ylim = c(fluxLow,fluxHigh), yaxs = "i", xlab = "", las = 1,  
       ylab = "Flux, in kg per day", cex.main = 0.95,
       main = fluxTitle)
  par(new = TRUE)
  plot(eList$Sample$DecYear, eList$Sample$ConcHigh * eList$Sample$Q * 86.4, log = "y", pch = 20, 
       cex = 1.1, col = eList$Sample$color, 
       xlim = c(start, end), xaxs = "i", ylim = c(fluxLow,fluxHigh), yaxs = "i", xlab = "",
       ylab = "", main = "", axes = FALSE)
  par(new = TRUE)
  plot(Daily$DecYear, Daily$GenFlux, log = "y", type = "l", xlim = c(start, end), 
       xaxs = "i", ylim = c(fluxLow,fluxHigh), yaxs = "i", xlab = "", col = "green", 
       ylab = "", main = "", axes = FALSE)
}
#
#
#
# script for identifying if a Sample data frame has
# any days with multiple samples
# or any samples that are uncensored
specialCase <- function(eList) {
  Sample <- eList$Sample
  n <- length(Sample$Date)
  days <- unique(Sample$Julian)
  nDays <- length(days)
  mult <- if(n > nDays) TRUE else FALSE
  nUncen <- sum(Sample$Uncen)
  cen <- if(nUncen < n) TRUE else FALSE
  # when mult is TRUE, needs to go through the subsampling process each time
  # when cen is TRUE it needs to go through the random augmentation 
  special <- data.frame(mult, cen)
  return(special)
}
##########################################
#
# this is substitute code for plotConcHist
##########################################
plotConcHistK <- function (eList, DailyK, yearStart = NA, yearEnd = NA, concMax = NA, 
                           printTitle = TRUE, tinyPlot = FALSE, usgsStyle = FALSE, plotFlowNorm = TRUE, 
                           plotAnnual = TRUE, cex = 0.8, cex.axis = 1.1, cex.main = 1.1, 
                           lwd = 2, col = "black", col.pred = "green", customPar = FALSE, 
                           ...) 
{
  localDaily <- DailyK
  localDaily$ConcDay <- DailyK$GenConc
  localINFO <- getInfo(eList)
  if (all(c("paStart", "paLong") %in% names(localINFO))) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
  }
  else {
    paLong <- 12
    paStart <- 10
  }
  if (!(c("FNConc") %in% names(eList$Daily))) {
    stop("This function requires running modelEstimation on eList")
  }
  localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, 
                                   localDaily = localDaily)
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  periodName <- setSeasonLabel(localAnnualResults = localAnnualResults)
  if (hasFlex) {
    periodName <- paste(periodName, "*")
  }
  if (plotAnnual & plotFlowNorm) {
    title3 <- "\nMean Concentration(K) (dots) & Flow Normalized Concentration (line)"
  }
  else if (plotAnnual & !plotFlowNorm) {
    title3 <- "\nAnnual Mean Concentration(K)"
  }
  else if (!plotAnnual & plotFlowNorm) {
    title3 <- "\nFlow Normalized Concentration"
  }
  else {
    title3 <- "\n"
  }
  title <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
          "\n", periodName, title3)
  else ""
  if (is.na(yearStart)) {
    yearStart <- min(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], 
                     na.rm = TRUE)
  }
  if (is.na(yearEnd)) {
    yearEnd <- max(localAnnualResults$DecYear[!is.na(localAnnualResults$FNConc)], 
                   na.rm = TRUE)
  }
  xInfo <- generalAxis(x = localAnnualResults$DecYear, minVal = yearStart, 
                       maxVal = yearEnd, padPercent = 0, tinyPlot = tinyPlot)
  combinedY <- c(localAnnualResults$Conc, localAnnualResults$FNConc[localAnnualResults$DecYear > 
                                                                      xInfo$bottom & localAnnualResults$DecYear < xInfo$top])
  yInfo <- generalAxis(x = combinedY, minVal = 0, maxVal = concMax, 
                       padPercent = 5, tinyPlot = tinyPlot, units = localINFO$param.units, 
                       usgsStyle = usgsStyle)
  genericEGRETDotPlot(x = NA, y = NA, xTicks = xInfo$ticks, 
                      yTicks = yInfo$ticks, xDate = TRUE, xlim = c(xInfo$bottom, 
                                                                   xInfo$top), ylim = c(yInfo$bottom, yInfo$top), ylab = yInfo$label, 
                      col = col, cex = cex, plotTitle = title, cex.axis = cex.axis, 
                      cex.main = cex.main, tinyPlot = tinyPlot, customPar = customPar, 
                      ...)
  if (plotAnnual) {
    with(localAnnualResults, points(DecYear[DecYear > xInfo$bottom & 
                                              DecYear < xInfo$top], Conc[DecYear > xInfo$bottom & 
                                                                           DecYear < xInfo$top], col = col, cex = cex, pch = 20))
  }
  if (plotFlowNorm) {
    with(localAnnualResults, lines(DecYear[DecYear > xInfo$bottom & 
                                             DecYear < xInfo$top], FNConc[DecYear > xInfo$bottom & 
                                                                            DecYear < xInfo$top], col = col.pred, lwd = lwd))
  }
}
########################################
#
# substitute code for plotFluxHist
#########################################
plotFluxHistK <- function (eList, DailyK, yearStart = NA, yearEnd = NA, fluxUnit = 9, 
          fluxMax = NA, printTitle = TRUE, usgsStyle = FALSE, plotFlowNorm = TRUE, 
          plotAnnual = TRUE, tinyPlot = FALSE, col = "black", col.pred = "green", 
          cex = 0.8, cex.axis = 1.1, cex.main = 1.1, lwd = 2, customPar = FALSE, 
          ...) 
{
  localINFO <- getInfo(eList)
  localDaily <- DailyK
  localDaily$FluxDay <- localDaily$GenFlux
  if (sum(c("paStart", "paLong") %in% names(localINFO)) == 
      2) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
  }
  else {
    paLong <- 12
    paStart <- 10
  }
  if (!(c("FNFlux") %in% names(eList$Daily))) {
    stop("This function requires running modelEstimation on eList")
  }
  possibleGoodUnits <- c("mg/l", "mg/l as N", "mg/l as NO2", 
                         "mg/l as NO3", "mg/l as P", "mg/l as PO3", "mg/l as PO4", 
                         "mg/l as CaCO3", "mg/l as Na", "mg/l as H", "mg/l as S", 
                         "mg/l NH4")
  allCaps <- toupper(possibleGoodUnits)
  localUnits <- toupper(localINFO$param.units)
  if (!(localUnits %in% allCaps)) {
    warning("Expected concentration units are mg/l, \nThe INFO dataframe indicates:", 
            localINFO$param.units, "\nFlux calculations will be wrong if units are not consistent")
  }
  localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, 
                                   localDaily = localDaily)
  if (is.numeric(fluxUnit)) {
    fluxUnit <- fluxConst[shortCode = fluxUnit][[1]]
  }
  else if (is.character(fluxUnit)) {
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  if (tinyPlot) {
    ylabel <- fluxUnit@unitExpressTiny
  }
  else {
    ylabel <- ifelse(usgsStyle, fluxUnit@unitUSGS, fluxUnit@unitExpress)
  }
  unitFactorReturn <- fluxUnit@unitFactor
  numYears <- length(localAnnualResults$DecYear)
  yearStart <- if (is.na(yearStart)) 
    trunc(min(localAnnualResults$DecYear[!is.na(localAnnualResults$FNFlux)], 
              na.rm = TRUE))
  else yearStart
  yearEnd <- if (is.na(yearEnd)) 
    trunc(max(localAnnualResults$DecYear[!is.na(localAnnualResults$FNFlux)], 
              na.rm = TRUE)) + 1
  else yearEnd
  subAnnualResults <- localAnnualResults[localAnnualResults$DecYear >= 
                                           yearStart & localAnnualResults$DecYear <= yearEnd, ]
  annFlux <- unitFactorReturn * subAnnualResults$Flux
  fnFlux <- unitFactorReturn * subAnnualResults$FNFlux
  hasFlex <- c("segmentInfo") %in% names(attributes(eList$INFO))
  periodName <- setSeasonLabel(localAnnualResults = localAnnualResults)
  if (hasFlex) {
    periodName <- paste(periodName, "*")
  }
  if (plotAnnual & plotFlowNorm) {
    title3 <- "\nFlux Estimates(K) (dots) & Flow Normalized Flux (line)"
  }
  else if (plotAnnual & !plotFlowNorm) {
    title3 <- "\nAnnual Flux Estimates(K)"
  }
  else if (!plotAnnual & plotFlowNorm) {
    title3 <- "\nFlow Normalized Flux"
  }
  else {
    title3 <- "\n"
  }
  title <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
          "\n", periodName, title3)
  else ""
  xInfo <- generalAxis(x = subAnnualResults$DecYear, minVal = yearStart, 
                       maxVal = yearEnd, padPercent = 0, tinyPlot = tinyPlot)
  combinedY <- c(annFlux, fnFlux)
  yInfo <- generalAxis(x = combinedY, minVal = 0, maxVal = fluxMax, 
                       padPercent = 5, tinyPlot = tinyPlot)
  genericEGRETDotPlot(x = NA, y = NA, xTicks = xInfo$ticks, 
                      yTicks = yInfo$ticks, xDate = TRUE, xlim = c(xInfo$bottom, 
                                                                   xInfo$top), ylim = c(0, yInfo$top), col = col, ylab = ylabel, 
                      plotTitle = title, customPar = customPar, cex = cex, 
                      cex.axis = cex.axis, cex.main = cex.main, tinyPlot = tinyPlot, 
                      ...)
  if (plotAnnual) {
    with(subAnnualResults, points(subAnnualResults$DecYear[DecYear > 
                                                             xInfo$bottom & DecYear < xInfo$top], annFlux[DecYear > 
                                                                                                            xInfo$bottom & DecYear < xInfo$top], col = col, cex = cex, 
                                  pch = 20))
  }
  if (plotFlowNorm) {
    lines(subAnnualResults$DecYear, fnFlux, col = col.pred, 
          lwd = lwd)
  }
}
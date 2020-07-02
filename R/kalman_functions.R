#' WRTDS-K 
#' 
#' This function takes an existing eList 
#' Including the estimated model (the surfaces object in the eList)
#' And produces the daily WRTDS_K estimates of concentration and flux
#'
#' @rdname wrtdsK
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param rho numeric 
#' @param niter numeric
#' @param seed setSeed value. Defaults to 376168 This is used to make repeatable output.
#' @examples 
#' eList <- Choptank_eList
#' DailyK <- makeDailyK(eList)
#' summary(DailyK)
#' 
#' AnnualResults <- setupYears(DailyK)
#' 
makeDailyK <- function(eList, rho = 0.90, niter = 200, seed = 376168){
  
  message("This function is currently in development")
  
  set.seed(seed)
  # this part is to set up the array of runs of missing values
  localEList <- cleanUp(eList)
  localDaily <- populateDailySamp(localEList)
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
    localDaily <- populateDailySamp(localEList)
    x <- localDaily$stdResid
    #    xxP is x that has been padded with a 0 at the start and a 0 at the end
    #    thus it is a vector that always starts and ends with non-missing values
    xxP <- c(0,x,0)
    # now we are going to loop through all the gaps that need to be filled in
    for(i in 1:numGap) {
      iGap <- doGap[i]
      startFill <- zstarts[iGap]
      endFill <- zends[iGap]+1
      nFill<-zz$length[iGap]+2
      xfill <- genmissing(xxP[startFill],xxP[endFill],rho,nFill)
      xxP[startFill:endFill]<-xfill}
    # now we need to strip out the padded days
    xResid <- xxP[2:numDaysP]
    xConc <- exp((xResid*localDaily$SE)+localDaily$yHat)
    DailyGen[,iter] <- xConc * localDaily$Q * 86.4
  }
  # now we take means over all the iterations
  GenMean <- rep(NA, numDays)
  Daily <- eList$Daily
  
  for(i in 1 : numDays) {
    GenMean[i] <- mean(DailyGen[i,])
  }
  Daily$GenFlux <- GenMean
  Daily$GenConc <- Daily$GenFlux / (Daily$Q * 86.4)
  attr(Daily, "niter") <- niter
  attr(Daily, "rho") <- rho
  return(Daily)
}


#' @export
#' @rdname wrtdsK
#' @examples 
#' 
#' eList <- cleanUp(eList)
#' 
cleanUp <- function(eList){
  
  message("This function is currently in development")
  
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


#' @export
#' @rdname wrtdsK
#' @param df data frame
#' @param col_name column name
#' @param seed number to set seed for reproducibility
#' @examples 
#' eList <- Choptank_eList
#' Sample <- eList$Sample
#' random_subset(Sample, col_name = "Julian")
#' 
#' df <- data.frame(Julian = c(1,2,2,3,4,4,4,6),
#'                  y = 1:8)
#' random_subset(df, "Julian")
random_subset <- function(df, col_name){
  
  message("This function is currently in development")
  
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

#' @rdname wrtdsK
#' @export
#' @examples 
#' 
#' d2 <- populateDailySamp(eList)
#' 
#' 
populateDailySamp <- function(eList) {
  
  message("This function is currently in development")
  
  localSample <- eList$Sample
  localDaily <- eList$Daily
  
  localSample <- localSample[,c("Julian", "ConcAve")]
  names(localSample) <- c("Julian", "trueConc")
  
  retDaily <- merge(localDaily,
                    localSample, 
                    by = "Julian", all.x = TRUE)
  
  retDaily$trueFlux <- retDaily$trueConc * retDaily$Q * 86.4
  retDaily$stdResid <- (log(retDaily$trueConc)-retDaily$yHat)/retDaily$SE
    
  return(retDaily)
}

#' @param X1 the value before the gap
#' @param XN  value after the gap
#' @param rho the lag one autocorrelation
#' @param N  the length of the sequence including X1 and XN 
#' it is two more than the gap length
#' @rdname wrtdsK
#' @export
#'  
#' @examples 
#' 
#' 
genmissing <- function(X1, XN, rho, N){
  # this code was done by Tim Cohn

  message("This function is currently in development")
  
  C <- t(chol(rho^abs(outer(1:N,1:N, "-"))[c(1,N,2:(N-1)),c(1,N,2:(N-1))]))
  
  (C %*% c(MASS::ginv(C[1:2,1:2]) %*% 
             c(X1, XN), 
           stats::rnorm(N-2)))[c(1,3:N,2)]
}


specialCase <- function(eList) {
  Sample <- eList$Sample
  n <- length(Sample$Date)
  days <- unique(Sample$Julian)
  nDays <- length(days)
  mult <- n > nDays
  nUncen <- sum(Sample$Uncen)
  cen <- nUncen < n
  # when mult is TRUE, needs to go through the subsampling process each time
  # when cen is TRUE it needs to go through the random augmentation 
  special <- data.frame(mult, cen)
  return(special)
}

#' @export
#' @rdname wrtdsK
#' @param paStart A numeric value for the starting month of the Period of Analysis, default is 10
#' @param paLong A numeric value for the length of the Period of Analysis in months, default is 12
#' @examples 
#' computeAnnual(eList, DailyK)
computeAnnual <- function(eList, DailyK, paStart = 10, paLong = 12) {

  message("This function is currently in development")
  
  AnnualResults <- setupYears(DailyK, paStart = paStart, paLong = paLong)
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
  xMin <- floor(AnnualResults$DecYear[1])
  xMax <- ceiling(AnnualResults$DecYear[nYears])
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


#' @export
#' @param start numeric start of DecYear for plot
#' @param DailyK Daily data frame run through the WRTDS-K analysis
#' @param end numeric end of DecYear for plot
#' @rdname wrtdsK
#' @examples 
#' plotTimeSlice(eList, DailyK, start = 1990, end = 1991)
plotTimeSlice <- function(eList, DailyK, start, end){
  
  message("This function is currently in development")
  
  Daily <- subset(DailyK,DecYear >= start & DecYear <= end)
  concHigh <- 1.1 * max(Daily$ConcDay,Daily$GenConc,Daily$ConcDay,na.rm = TRUE)
  concLow <- 0.9 * min(Daily$ConcDay,Daily$GenConc,Daily$ConcDay,na.rm = TRUE)
  fluxHigh <- 1.1 * max(Daily$FluxDay,Daily$GenFlux,Daily$FluxDay,na.rm = TRUE)
  fluxLow <- 0.9 * min(Daily$FluxDay,Daily$GenFlux,Daily$FluxDay,na.rm = TRUE)
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

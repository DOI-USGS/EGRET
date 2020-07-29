#' WRTDS-Kalman
#' 
#' This function takes an existing eList 
#' Including the estimated model (the surfaces object in the eList)
#' And produces the daily WRTDSKalman estimates of concentration and flux
#'
#' @rdname wrtdsK
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param rho numeric the lag one autocorrelation. Default is 0.9.
#' @param niter number of iterations. Default is 200.
#' @param seed setSeed value. Defaults to 376168 This is used to make repeatable output.
#' @examples 
#' eList <- Choptank_eList
#' eList <- WRTDSKalman(eList)
#' summary(eList$Daily)
#' 
#' AnnualResults <- setupYears(eList$Daily)
#' head(AnnualResults)
WRTDSKalman <- function(eList, rho = 0.90, niter = 200, seed = 376168){
  
  message("This function is currently in development")
  
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
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
  
  eList$Daily <- Daily
  
  return(eList)
}

#' cleanUp eList
#' 
#' Takes an eList as the input. If there are duplicated dates in the Sample data frame, 
#' will randomly select one value for that date.
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes. 
#' @export
#' @return eList with duplicated dates in the Sample data frame randomly sampled.
#' @examples 
#' eList <- Choptank_eList
#' 
#' eList <- cleanUp(eList)
#' 
cleanUp <- function(eList){

  Sample <- randomSubset(eList$Sample, "Julian")
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


#' randomSubset
#' 
#' Calculates a random subset of the data based on repeated values from
#' a specified column. 
#' 
#' @export
#' @param df data frame. Must include a column named by the argument colName.
#' @param colName column name to check for duplicates
#' @param seed number to set seed for reproducibility
#' @examples 
#' df <- data.frame(Julian = c(1,2,2,3,4,4,4,6),
#'                  y = 1:8)
#' df
#' df_random <- randomSubset(df, "Julian")
#' df_random
randomSubset <- function(df, colName){

  dupIndex <- unique(c(which(duplicated(df[[colName]], fromLast = FALSE)), 
                        which(duplicated(df[[colName]], fromLast = TRUE))))
  
  if(length(dupIndex) == 0){
    return(df)
  }
  
  dupIndex <- dupIndex[order(dupIndex)]

  unique_groups <- unique(df[[colName]][dupIndex])
  
  sliceIndex <- sapply(unique_groups, function(x){
    sample(which(df[[colName]] == x), size = 1)
  })
  
  dfDuplicates <- df[sliceIndex, ] 
  dfNoDuplicates <- df[-dupIndex,]
  
  subDF <- rbind(dfNoDuplicates, dfDuplicates)
  subDF <- subDF[order(subDF[[colName]]),]

  return(subDF)
}

populateDailySamp <- function(eList) {

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

 
genmissing <- function(X1, XN, rho, N){
  # this code was done by Tim Cohn
  # @param X1 the value before the gap
  # @param XN  value after the gap
  # @param rho the lag one autocorrelation
  # @param N  the length of the sequence including X1 and XN 
  # it is two more than the gap length
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

#' plotWRTDSKalman
#' 
#' Two plots to check the flux estimates using the WRTDS_K vs classic WRTDS.
#' The first is annual flux over time, where the two fluxes are shown in different colors.
#' The second is WRTDS vs WRTDSKalman flux estimates. The graphs can be output
#' either on top of each other, or side by side using the \code{sideBySide} argument.
#' 
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes. This
#' eList must be run through \code{WRTDSKalman}.
#' @param sideBySide logical. If \code{TRUE}, the two plots will be plotted
#' side by side, otherwise, one by one vertically.
#' 
#' @export
#' @examples 
#' 
#' eList <- Choptank_eList
#' eList <- WRTDSKalman(eList)
#' plotWRTDSKalman(eList)
#' 
#' plotWRTDSKalman(eList, sideBySide = TRUE)
#' 
plotWRTDSKalman <- function(eList, sideBySide = FALSE) {

  message("This function is currently in development")

  if(!all((c("GenFlux","GenConc") %in% names(eList$Daily)))){
    stop("This function requires running WRTDSKalman on eList")
  }
  
  if(all(c("paStart","paLong") %in% names(eList$INFO))){
    paLong <- eList$INFO$paLong
    paStart <- eList$INFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
    
  AnnualResults <- setupYears(eList$Daily)
  
  yMax <- 1.1 * max(AnnualResults$Flux, AnnualResults$GenFlux)
  nYears <- length(AnnualResults[,1])
  # first a plot of just the WRTDS estimate
  xMin <- floor(AnnualResults$DecYear[1])
  xMax <- ceiling(AnnualResults$DecYear[nYears])
  xlim <- c(xMin,xMax)
  if(sideBySide){
    title1 <- "Annual Flux Estimates: WRTDS in red, WRTDS-K in green"
    title2 <- "Comparison of the two flux estimates"
    mainTitle <- paste(eList$INFO$shortName,eList$INFO$paramShortName,"\n",
                       setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong))
    par(mfrow=c(1,2), oma=c(0,0,2,0))
  } else {
    title1 <- paste(eList$INFO$shortName,eList$INFO$paramShortName,
                    "\nAnnual Flux Estimates: WRTDS in red, WRTDS-K in green\n",
                    setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong), sep="  ")
    title2 <- paste(eList$INFO$shortName,eList$INFO$paramShortName,
                    "\nComparison of the two flux estimates\n",
                    setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong),sep="  ")
  }

  genericEGRETDotPlot(AnnualResults$DecYear, AnnualResults$Flux,
                      plotTitle =  title1, 
                      xlim = xlim, xaxs = "i",
                      ylim = c(0, yMax),  cex.main = 0.9,
                      xlab = "", ylab = "Annual flux, metric tons",
                      col = "red", cex = 1.4)
  points(AnnualResults$DecYear, AnnualResults$GenFlux, 
         col = "green", pch = 20, cex = 1.4)
  
  # scatter plot
  genericEGRETDotPlot(AnnualResults$Flux, AnnualResults$GenFlux, 
                      cex = 1.3, col = "red", 
                      xlim = c(0, yMax), 
                      ylim = c(0, yMax), 
                      xlab = "WRTDS estimate of annual flux, in metric tons", 
                      ylab = "WRTDSKalman estimate of annual flux, in metric tons", 
                      cex.main = 0.9, 
                      plotTitle = title2)
  abline(a = 0, b = 1)

  if(sideBySide){
    mtext(mainTitle, line = -1, side = 3, outer = TRUE, cex= 1)
  }
}


#' plotTimeSlice
#' 
#' Plot of either concentration or flux over time showing both the WRTDS and WRTDSKalman estimates.
#' 
#' 
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes. This
#' eList must be run through \code{WRTDSKalman}.
#' @param conc logical. If \code{TRUE}, plot concentration, otherwise plot flux.
#' @param start numeric start of DecYear for plot. If \code{NA}, plot will start at the earliest date in the record.
#' @param end numeric end of DecYear for plot. If \code{NA}, plot will end at the latest date in the record.
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{printFluxUnitCheatSheet}}
#' @examples 
#' plotTimeSlice(eList, start = 1990, end = 1991, conc = TRUE)
#' 
#' plotTimeSlice(eList, start = 1990, end = 1991, conc = FALSE)
#' 
#' plotTimeSlice(eList, start = NA, end = 1991, conc = FALSE)
#' 
plotTimeSlice <- function(eList, start = NA, end = NA, conc = TRUE, 
                          fluxUnit = 3, usgsStyle = FALSE){
  
  message("This function is currently in development")

  if(!all((c("GenFlux","GenConc") %in% names(eList$Daily)))){
    stop("This function requires running WRTDSKalman on eList")
  }
  
  eList <- makeAugmentedSample(eList)
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  if(!is.na(start)){
    Daily <- Daily[Daily$DecYear >= start,]
    
    Sample <- Sample[Sample$DecYear >= start,]    
  } else {
    start <- min(c(Daily$DecYear, Sample$DecYear))
  }
  
  if(!is.na(end)){
    Daily <- Daily[Daily$DecYear <= end,]
    
    Sample <- Sample[Sample$DecYear <= end,]    
  } else {
    end <- max(c(Daily$DecYear, Sample$DecYear))
  }


 # figure out which data symbol to use, red for uncensored, brown for censored
  Sample$color <- ifelse(Sample$Uncen == 1, "red", "cyan4")

  # first concentration, then flux
  name <- paste(eList$INFO$shortName, eList$INFO$paramShortName)

  possibleGoodUnits <- c("mg/l","mg/l as N", "mg/l as NO2", "mg/L",
                         "mg/l as NO3","mg/l as P","mg/l as PO3","mg/l as PO4","mg/l as CaCO3",
                         "mg/l as Na","mg/l as H","mg/l as S","mg/l NH4" )
  
  allCaps <- toupper(possibleGoodUnits)
  localUnits <- toupper(eList$INFO$param.units)
  
  if(!(localUnits %in% allCaps)){
    warning("Expected concentration units are mg/l, \nThe INFO dataframe indicates:",eList$INFO$param.units,
            "\nFlux calculations will be wrong if units are not consistent")
  }
  
  if(conc){
    ratio <- mean(Daily$GenConc) / mean(Daily$ConcDay)
    fratio <- format(ratio, digits = 2)
    
    y1 <- Daily$ConcDay
    y2 <- Daily$GenConc
    y3 <- Sample$rObserved

    plotTitle <- paste(name,"\nConcentrations, Black is WRTDS, Green is WRTDSKalman\nData in red, (rl in blue if <), Ratio of means is", fratio)
    
  } else {

    if (is.numeric(fluxUnit)){
      fluxUnit <- fluxConst[shortCode=fluxUnit][[1]]    
    } else if (is.character(fluxUnit)){
      fluxUnit <- fluxConst[fluxUnit][[1]]
    }

    fluxFactor <- fluxUnit@unitFactor
    
    ratio <- mean(Daily$GenFlux) / mean(Daily$FluxDay)
    fratio <- format(ratio, digits = 2)
    
    y1 <- Daily$FluxDay * fluxFactor
    y2 <- Daily$GenFlux * fluxFactor
    y3 <- Sample$rObserved * Sample$Q * fluxFactor *86.40

    if(usgsStyle){
      yLab <- fluxUnit@unitUSGS[[1]]
    } else {
      yLab <- fluxUnit@unitExpress[[1]]
    }
    
    plotTitle <- paste(name,"\nFlux, Black is WRTDS, Green is WRTDSKalman\nData in red, (rl in blue if <), Ratio of means is", fratio)
  }

  high_y <- max(y1, y2, y3, na.rm = TRUE)
  low_y <- min(y1, y2, y3, na.rm = TRUE)
  
  yInfo <- generalAxis(x = y1, 
                       minVal = low_y, 
                       maxVal = high_y, padPercent = 10,
                       units = eList$INFO$param.units,
                       logScale = TRUE, concentration = conc)

  genericEGRETDotPlot(Daily$DecYear,
                      y1, log = "y", type = "l", 
                      xlim = c(start, end),
                      ylim = c(yInfo$bottom,yInfo$top),
                      yTicks = yInfo$ticks, 
                      xlab = "", cex.main = 0.9,
                      ylab = ifelse(conc, yInfo$label, yLab),
                      plotTitle = plotTitle)
  lines(Daily$DecYear, y2,
        col = "green")
  points(Sample$DecYear, y3, 
         pch = 20, cex = 1.1, col = Sample$color)

}

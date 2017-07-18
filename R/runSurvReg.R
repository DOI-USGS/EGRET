#' Run the weighted survival regression for a set of estimation points (defined by DecYear and Log(Q))
#'
#'   This function runs the survival regression which is the concentration estimation method of WRTDS. 
#'    It uses sample data from the data frame Sample. 
#'    It does the estimation for a set of data points defined by two vectors: estPtYear and estPtLQ. 
#'    It returns an array of results for the estimation points.  
#'    The array returned contains yHat, SE and ConcHat (in that order). 
#'
#' @param Sample dataframe created for EGRET analysis
#' @param estPtYear numeric vector of Decimal Year values at the estimation points
#' @param estPtLQ numeric vector of ln(Q) values at the estimation points, must be the same length as estPtYear 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param verbose logical specifying whether or not to display progress message
#' @param interactive logical deprecated. Use 'verbose' instead
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param numDays number of days in the Daily record
#' @param DecLow number specifying minimum decimal year
#' @param DecHigh number specifying maximum decimal year
#' @param run.parallel logical to run bootstrapping in parallel or not
#' @keywords water-quality statistics
#' @importFrom survival survreg
#' @importFrom survival Surv
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @return resultSurvReg numeric array containing the yHat, SE, and ConcHat values array dimensions are (numEstPts,3)
#' @export
#' @examples
#' eList <- Choptank_eList
#' estPtYear<-c(2001.0,2005.0,2009.0)
#' estPtLQ<-c(1,1,1)
#' Sample <- getSample(eList)
#' numDays <- Sample$Julian[nrow(Sample)] - Sample$Julian[1] + 1
#' DecLow <- Sample$DecYear[1]
#' DecHigh <- Sample$DecYear[nrow(Sample)]
#' resultSurvReg <- runSurvReg(estPtYear,estPtLQ,
#'                             numDays,DecLow,DecHigh,Sample,
#'                             run.parallel = FALSE)
runSurvReg<-function(estPtYear,estPtLQ,numDays,DecLow,DecHigh,Sample, 
                     windowY=7, windowQ=2, windowS=0.5,
                     minNumObs=100, minNumUncen=50, verbose = TRUE,interactive=NULL,
                     edgeAdjust=TRUE, run.parallel = FALSE) {
  
  if(!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  
  localSample <- Sample
  numSamples <- length(localSample$DecYear)
  
  numEstPt<-length(estPtYear)
  
  printUpdate <- floor(seq(1,numEstPt,numEstPt/100))
  endOfLine <- seq(10,100,10)
  
  if (minNumUncen >= sum(localSample$Uncen)) stop('minNumUncen is greater than total number of samples')
  if (minNumObs >= nrow(localSample)) stop('minNumObs is greater than total number of samples')
  
  warningFlag <- 0
  n <- NULL
  if(run.parallel){
    wrtds_return_list <- foreach(n = 1:numEstPt, .packages=c('EGRET')) %dopar% {
                      wrtds_returns <- run_WRTDS(estPtYear[n], estPtLQ[n],
                                         localSample,DecLow,DecHigh,
                                         minNumObs,minNumUncen,
                                         windowY, windowQ, windowS,
                                         edgeAdjust)
                    }
    
    warningFlag <- sum(sapply(wrtds_return_list, function(x) x[["warningFlag"]]))
    resultSurvReg <- t(sapply(wrtds_return_list, function(x) x[["survReg"]]))
    
  } else {
    resultSurvReg<-array(0,c(numEstPt,3))
    if (verbose) cat("Survival regression (% complete):\n")
    
    for (i in 1:numEstPt) {
      wrtds_return <- run_WRTDS(estPtYear[i], estPtLQ[i],
                localSample,DecLow,DecHigh,
                minNumObs,minNumUncen,
                windowY, windowQ, windowS,
                edgeAdjust)
  
      if (i %in% printUpdate & verbose) {
        cat(floor(i*100/numEstPt),"\t")
        if (floor(i*100/numEstPt) %in% endOfLine) cat("\n")
      }
      warningFlag <- warningFlag + wrtds_return$warningFlag
      resultSurvReg[i,] <- wrtds_return$survReg
    }
  }
  
  if (warningFlag > 0){
    message("\nIn model estimation, the survival regression function was run ", numEstPt, " times (for different combinations of discharge and time).  In ", warningFlag, " of these runs it did not properly converge. This does not mean that the model is unacceptable, but it is a suggestion that there may be something odd about the data set. You may want to check for outliers, repeated values on a single date, or something else unusual about the data.")
  }

  if (verbose) cat("\nSurvival regression: Done")

  return(resultSurvReg)
}



#' @keywords internal
#' @importFrom survival survreg
run_WRTDS <- function(estY, estLQ,
                      localSample,DecLow,DecHigh,
                      minNumObs,minNumUncen,
                      windowY, windowQ, windowS,
                      edgeAdjust){
  # This loop takes us through all the estimation points
  # We always reset the window widths to their starting values, because they may
  #   have been widened in the process
  tempWindowY<-windowY
  tempWindowQ<-windowQ
  tempWindowS<-windowS
  
  distLow <- estY-DecLow
  distHigh <- DecHigh-estY
  
  survReg <- c(NA, NA, NA)
  warningFlag <- 0
  
  if(all(is.na(c(distLow,distHigh)))){
    return(list(survReg=survReg, warningFlag=warningFlag))
  }
  
  distTime <- min(distLow,distHigh)
  
  if (edgeAdjust & !is.na(distTime)) {
    tempWindowY <- if(distTime>tempWindowY) tempWindowY else ((2 * tempWindowY) - distTime)
  } 

  k <- 1
  
  repeat{
    #  We subset the sample frame by time, to narrow the set of data to run through in the following steps
    
    Sam <- localSample[abs(localSample$DecYear-estY) <= tempWindowY,]
    diffY<-abs(Sam$DecYear-estY)
    weightY<-triCube(diffY,tempWindowY)
    weightQ<-triCube(Sam$LogQ-estLQ,tempWindowQ)
    diffUpper<-ceiling(diffY)
    diffLower<-floor(diffY)
    diffSeason<-pmin(abs(diffUpper-diffY),abs(diffY-diffLower))
    weightS<-triCube(diffSeason,tempWindowS)
    Sam$weight<-weightY*weightQ*weightS
    Sam<-subset(Sam,weight>0)
    numPosWt<-length(Sam$weight)
    numUncen<-sum(Sam$Uncen)
    tempWindowY<-tempWindowY*1.1
    tempWindowQ<-tempWindowQ*1.1
    k <- k + 1
    if(k > 10000) message("Problems converging")
    # the next line is designed so that if the user sets windowS so it includes
    # data from all seasons, the widening process leaves it alone    	
    tempWindowS<-if(windowS<=0.5) min(tempWindowS*1.1,0.5) else windowS
    if(numPosWt>=minNumObs&numUncen>=minNumUncen | k > 10000) break
  }
  
  # now we are ready to run Survival Regression
  weight<-Sam$weight
  aveWeight<-sum(weight)/numPosWt
  weight<-weight/aveWeight
  Sam <- data.frame(Sam)
  
  x <- tryCatch({
    survModel<-survreg(Surv(log(ConcLow),log(ConcHigh),type="interval2") ~ 
                         DecYear+LogQ+SinDY+CosDY,data=Sam,weights=weight,dist="gaus")
    
    
  }, warning=function(w) {
    return(NA)
  }, error=function(e) {
    message(e, "Error")
    return(NULL)
  })
  
  if(exists("survModel")) {
    newdf<-data.frame(DecYear=estY,LogQ=estLQ,SinDY=sin(2*pi*estY),CosDY=cos(2*pi*estY))
    #   extract results at estimation point
    yHat<-predict(survModel,newdf)
    SE<-survModel$scale
    bias<-exp((SE^2)/2)
    survReg[1]<-yHat
    survReg[2]<-SE
    survReg[3]<-bias*exp(yHat)
  } 
  
  if(all(is.na(x))){
    warningFlag <- 1
  }
  return(list(survReg=survReg, warningFlag=warningFlag))
}

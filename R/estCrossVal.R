#' Jack-Knife cross validation of the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This function fits the WRTDS model n times (where n is the number of observations).  
#' For each fit, the data value being estimated is eliminated from the record. 
#' This gives predictions that do not depend on knowing the actual result for that day. 
#' Thus it provides for a more "honest" estimate of model performance than a traditional 
#' error analysis that uses all the data. 
#'
#' @param Sample data frame containing the sample values, default is Sample
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param DecLow number specifying minimum decimal year
#' @param DecHigh number specifying maximum decimal year
#' @param verbose logical specifying whether or not to display progress message
#' @keywords water-quality statistics
#' @return SampleCrossV data frame containing the sample data augmented by the results of the cross-validation exercise
#' @export
#' @examples
#' eList <- Choptank_eList
#' Sample <- getSample(eList)
#' Daily <- getDaily(eList)
#' numDays <- length(Daily$DecYear)
#' DecLow <- Daily$DecYear[1]
#' DecHigh <- Daily$DecYear[numDays]
#' \dontrun{
#' SampleCrossV <- estCrossVal(numDays,DecLow,DecHigh,Sample)
#' }
estCrossVal<-function(DecLow,DecHigh, Sample, windowY = 7, windowQ = 2, 
                      windowS = 0.5, minNumObs = 100, minNumUncen = 50,
                      edgeAdjust=TRUE, verbose = TRUE){
  #  this function fits the WRTDS model making an estimate of concentration for every day
  #    But, it uses leave-one-out-cross-validation
  #    That is, for the day it is estimating, it leaves that observation out of the data set
  #      It returns a Sample data frame with three added columns
  #      yHat, SE, and ConcHat
  
  localSample <- Sample
  originalColumns <- names(localSample)
  numObs<-nrow(localSample)
  yHat<-rep(0,numObs)
  SE<-rep(0,numObs)
  ConcHat<-rep(0,numObs)
  iCounter<-seq(1,numObs)
  if(verbose) cat("\n estCrossVal % complete:\n")

  colToKeep <- c("ConcLow","ConcHigh","Uncen","DecYear","SinDY","CosDY","LogQ")
  SampleCrossV <- localSample[,which(originalColumns %in% colToKeep)]

  SampleCV<-data.frame(SampleCrossV,iCounter,yHat,SE,ConcHat)

  printUpdate <- floor(seq(1,numObs,numObs/100))
  endOfLine <- seq(10,100,10)

  for(i in 1:numObs) {
    if(i %in% printUpdate & verbose) {
      cat(floor(i*100/numObs),"\t")
      if (floor(i*100/numObs) %in% endOfLine) cat("\n")
    }

    SampleMinusOne<-SampleCV[SampleCV$iCounter!=i,]
  
    result<-runSurvReg(SampleCrossV$DecYear[i],SampleCrossV$LogQ[i],DecLow,DecHigh,SampleMinusOne,
                       windowY,windowQ,windowS,minNumObs,minNumUncen,
                       edgeAdjust=edgeAdjust, verbose=FALSE, run.parallel = FALSE)
    
    yHat[i]<-result[1]
    SE[i]<-result[2]
    ConcHat[i]<-result[3]

  }	

  localSample$yHat <- yHat
  localSample$SE <- SE
  localSample$ConcHat <- ConcHat
  SampleCrossV <- localSample
  return(SampleCrossV)
}

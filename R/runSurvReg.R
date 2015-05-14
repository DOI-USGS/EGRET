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
#' @param interactive logical specifying whether or not to display progress message
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param numDays number of days in the Daily record
#' @param DecLow number specifying minimum decimal year
#' @param DecHigh number specifying maximum decimal year
#' @keywords water-quality statistics
#' @import survival
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
#' resultSurvReg <- runSurvReg(estPtYear,estPtLQ,numDays,DecLow,DecHigh,Sample)
runSurvReg<-function(estPtYear,estPtLQ,numDays,DecLow,DecHigh,Sample, 
                     windowY=7, windowQ=2, windowS=0.5,
                     minNumObs=100, minNumUncen=50, interactive=TRUE,
                     edgeAdjust=TRUE) {

  localSample <- Sample
  numSamples <- length(localSample$DecYear)
  
  numEstPt<-length(estPtYear)
  resultSurvReg<-array(0,c(numEstPt,3))
  
  printUpdate <- floor(seq(1,numEstPt,numEstPt/100))
  endOfLine <- seq(10,100,10)
  
  if (minNumUncen >= sum(localSample$Uncen)) stop('minNumUncen is greater than total number of samples')
  if (minNumObs >= nrow(localSample)) stop('minNumObs is greater than total number of samples')
  
  if (interactive) cat("Survival regression (% complete):\n")
  
#   options(warn=1) #warn=0 is default
  warningFlag <- 0
  
  for (i in 1:numEstPt) {
    
    # This loop takes us through all the estimation points
    # We always reset the window widths to their starting values, because they may
    #   have been widened in the process
    tempWindowY<-windowY
    tempWindowQ<-windowQ
    tempWindowS<-windowS
    estY<-estPtYear[i]
    distLow <- estY-DecLow
    distHigh <- DecHigh-estY
    distTime <- min(distLow,distHigh)
    
    if (edgeAdjust)  tempWindowY <- if(distTime>tempWindowY) tempWindowY else ((2 * tempWindowY) - distTime)
    
    estLQ<-estPtLQ[i]
    
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
    
#     survModel<-survreg(Surv(log(ConcLow),log(ConcHigh),type="interval2") ~ 
#                         DecYear+LogQ+SinDY+CosDY,data=Sam,weights=weight,dist="gaus")
    
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
      resultSurvReg[i,1]<-yHat
      resultSurvReg[i,2]<-SE
      resultSurvReg[i,3]<-bias*exp(yHat)
    } else {
      resultSurvReg[i,1]<-NA
      resultSurvReg[i,2]<-NA
      resultSurvReg[i,3]<-NA
    }
    
    if (i %in% printUpdate & interactive) {
      cat(floor(i*100/numEstPt),"\t")
      if (floor(i*100/numEstPt) %in% endOfLine) cat("\n")
    }

    if(all(is.na(x))){
      warningFlag <- warningFlag + 1
    }


  }

  if (warningFlag > 0 && interactive){
    
    message("\nIn model estimation, the survival regression function was run ", numEstPt, " times (for different combinations of discharge and time).  In ", warningFlag, " of these runs it did not properly converge. This does not mean that the model is unacceptable, but it is a suggestion that there may be something odd about the data set. You may want to check for outliers, repeated values on a single date, or something else unusual about the data.")
  }
#   options(warn=0) 
  if (interactive) cat("\nSurvival regression: Done")

  return(resultSurvReg)
}

#' Run the weighted survival regression for a set of estimation points (defined by DecYear and Log(Q))
#'
#'   This function runs the survival regression which is the concentration estimation method of WRTDS. 
#'    It uses sample data from the data frame Sample. 
#'    It does the estimation for a set of data points defined by two vectors: estPtYear and estPtLQ. 
#'    It returns an array of results for the estimation points.  
#'    The array returned contains yHat, SE and ConcHat (in that order). 
#'
#' @param localSample string specifying the name of the data frame containing the sample values, default is Sample
#' @param estPtYear numeric vector of Decimal Year values at the estimation points
#' @param estPtLQ numeric vector of ln(Q) values at the estimation points, must be the same length as estPtYear 
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 10
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @keywords water-quality statistics
#' @import survival
#' @return resultSurvReg numeric array containing the yHat, SE, and ConcHat values array dimensions are (numEstPts,3)
#' @export
#' @examples
#' estPtYear<-c(2001.0,2005.0,2009.0)
#' estPtLQ<-c(5,5,5)
#' runSurvReg(localSample=exSampleStart,estPtYear,estPtLQ)
runSurvReg<-function(localSample = Sample,estPtYear,estPtLQ,windowY=10,windowQ=2,windowS=0.5,minNumObs=100,minNumUncen=50,message=TRUE) {
  # runs survival regression model
  # Sample is the Sample data frame being used
  # estPtYear is a vector of DecYear values where the model will be estimated
  # estPtLQ is a vector of LogQ values where the model will be estimated (must be same as estPtYear length)
  # windows are the half window widths for DecYear, LogQ and, Season (respectively)
  # minNumObs is the minimum number of observations that must have non-zero weights
  #        if the survival regression is going to be run
  # minNumUncen is the minimum number of uncensored observations that must have non-zero weights
  #        if the survival regression is going to be run
  #   function returns of dimensions (3 by the number of estimation points)
  #        first column is predicted concentration in log space (called yHat)
  #        second column is the standard error (which is used to compute the bias correction)
  #        third column is the predicted concentration in real space (called ConcHat)
  library(survival)
  numEstPt<-length(estPtYear)
  resultSurvReg<-array(0,c(numEstPt,3))
  
  printUpdate <- floor(seq(1,numEstPt,numEstPt/100))
  
  if (message) cat("Survival regression (% complete):\n")

  for (i in 1:numEstPt) {
    
    # This loop takes us through all the estimation points
    # We always reset the window widths to their starting values, because they may
    #   have been widened in the process
    tempWindowY<-windowY
    tempWindowQ<-windowQ
    tempWindowS<-windowS
    estY<-estPtYear[i]
    estLQ<-estPtLQ[i]

    repeat{ # while loop took ever-so-slightly longer...not sure why
      #  We subset the sample frame by time, to narrow the set of data to run through in the following steps
      Sam<-subset(localSample,abs(DecYear-estY)<=tempWindowY)
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
      tempWindowS<-min(tempWindowS*1.1,0.5) 
      if(numPosWt >= minNumObs & numUncen >= minNumUncen) break
    }

#     While option:
#     numUncen <- 0
#     numPosWt <- 0
#     while (numPosWt < minNumObs | numUncen < minNumUncen){  
#     }
    
    # now we are ready to run Survival Regression
    weight<-Sam$weight
    aveWeight<-sum(weight)/numPosWt
    weight<-weight/aveWeight
    
    survModel<-survreg(Surv(log(ConcLow),log(ConcHigh),type="interval2")~DecYear+LogQ+SinDY+CosDY,data=Sam,weights=weight,dist="gaus")

    new<-data.frame(DecYear=estY,LogQ=estLQ,SinDY=sin(2*pi*estY),CosDY=cos(2*pi*estY))
    #   extract results at estimation point

    yHat<-predict(survModel,new)

    SE<-survModel$scale
    bias<-exp((SE^2)/2)
    resultSurvReg[i,1]<-yHat
    resultSurvReg[i,2]<-SE
    resultSurvReg[i,3]<-bias*exp(yHat)
    
    if (i %in% printUpdate & message) cat(floor(i*100/numEstPt),"\t")
    
  }
  
  if (message) cat("\nSurvival regression: Done")

  return(resultSurvReg)
}
#' Create randomized residuals and observations for data sets that have some censored data
#'
#'  This function is used to add two columns to the Sample data frame: rResid and rObserved.
#'  rResid is the randomized residual value computed in log concentration units, and rObserved
#'  is the randomized 'observed' value of concentration in concentration units.
#'  Both of these are computed for all censored samples ("less than values").
#'  They are created for purposes of plotting and are not used in any computations in EGRET.
#'
#' @param eList named list with at least the Sample dataframe
#' @details
#' The WRTDS model must be estimated before this function can be run.  The random value that is generated lies between the reporting limit and zero and is distributed as a truncated log-normal distribution, with parameters derived from the fitted WRTDS model.
#' These random values are never used in any computations in EGRET but are used for purposes of plotting the data set or residuals.  When plotted in other functions they are shown as open circles.  
#' @keywords water-quality statistics
#' @examples 
#' choptankAugmented <- makeAugmentedSample(Choptank_eList)
#' @export
#' @return eList named list with modified Sample data frame.
makeAugmentedSample <- function(eList){
  
  if(all(c("SE","yHat") %in% names(eList$Sample))){
    localSample <- eList$Sample
    numSamples <- length(localSample$Uncen)
    a <- ifelse(localSample$Uncen==0&!is.na(localSample$ConcLow),log(localSample$ConcLow)-localSample$yHat,-Inf)
    b <- ifelse(localSample$Uncen==1,+Inf,log(localSample$ConcHigh) - localSample$yHat)
    mean <- ifelse(localSample$Uncen==1,log(localSample$ConcHigh) - localSample$yHat,0)
    sd <- ifelse(localSample$Uncen==1,0,localSample$SE)
    localSample$rResid <- truncnorm::rtruncnorm(numSamples,a,b,mean,sd)
    localSample$rObserved <- exp(localSample$rResid + localSample$yHat)
    
    eList	<- as.egret(eList$INFO, eList$Daily, localSample, eList$surfaces)
  } else {
    message("Pseudo only supported after running modelEstimation")
  }
  return(eList)
}

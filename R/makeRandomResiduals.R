#' Create Randomized Residuals and Observations
#'
#'  This function is used to add two columns to the Sample data frame: rResid and rObserved.
#'  rResid is the randomized residual value computed in log concentration units, and rObserved
#'  is the randomized 'observed' value of concentration in concentration units.
#'
#' @param eList named list with at least the Daily dataframe
#' @keywords water-quality statistics
#' @return eList named list with modified Daily data frame.
#' @export
#' @importFrom truncnorm rtruncnorm
#' @examples
#' eList <- Choptank_eList
#' eList <- makeAugmentedSample(eList)
makeAugmentedSample <- function(eList){

  localSample <- eList$Sample
  numSamples<-length(localSample$Uncen)
  a <- ifelse(localSample$Uncen==0&!is.na(localSample$ConcLow),log(localSample$ConcLow)-localSample$yHat,-Inf)
  b <- ifelse(localSample$Uncen==1,+Inf,log(localSample$ConcHigh) - localSample$yHat)
  mean <- ifelse(localSample$Uncen==1,log(localSample$ConcHigh) - localSample$yHat,0)
  sd <- ifelse(localSample$Uncen==1,0,localSample$SE)
  localSample$rResid<-truncnorm::rtruncnorm(numSamples,a,b,mean,sd)
  localSample$rObserved <- exp(localSample$rResid + localSample$yHat)
  
  eList	<- as.egret(eList$INFO, eList$Daily, localSample, eList$surfaces)
  return(eList)
}

# makeRandomResiduals <- function(eList){
#   localSample <- eList$Sample
#   numSamples<-length(localSample$Uncen)
#   a <- ifelse(localSample$Uncen==0&!is.na(localSample$ConcLow),log(localSample$ConcLow)-localSample$yHat,-Inf)
#   b <- ifelse(localSample$Uncen==1,+Inf,log(localSample$ConcHigh) - localSample$yHat)
#   mean <- ifelse(localSample$Uncen==1,log(localSample$ConcHigh) - localSample$yHat,0)
#   sd <- ifelse(localSample$Uncen==1,0,localSample$SE)
#   localSample$rResid<-truncnorm::rtruncnorm(numSamples,a,b,mean,sd)
#   eList	<- as.egret(eList$INFO, eList$Daily, localSample, eList$surfaces)
#   return(eList)
# }
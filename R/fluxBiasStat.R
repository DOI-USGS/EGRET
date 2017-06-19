#'  Compute the flux bias statistic: (mean of estimated flux - mean of observed flux)  / mean of observed flux
#'
#'  Computes three versions of the flux bias: 
#'   The first where all censored values are set to their miniumum. 
#'   The second where all censored values are set to their maximum. 
#'   The third which is the average of the other two. 
#'      In practice there is rarely a noticable difference among them.
#'
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @keywords water-quality statistics, bias
#' @export
#' @return fluxBias a vector of three numerical values, a lower bound, upper bound and an average estimate of the ratio of (mean estimated flux - mean observed flux) / mean estimated flux.  Typically one should use fluxBias[3]
#' @examples
#' eList <- Choptank_eList
#' Sample <- getSample(eList)
#' fluxBias <- fluxBiasStat(Sample) 
fluxBiasStat <- function(localSample) {
  sumLow <- sum(localSample$ConcLow * localSample$Q, na.rm = TRUE)
  sumHigh <- sum(localSample$ConcHigh * localSample$Q, na.rm = TRUE)
  sumEst <- sum(localSample$ConcHat * localSample$Q, na.rm = TRUE)
  bias1 <- (sumEst - sumHigh) / sumEst
  bias2 <- (sumEst - sumLow) / sumEst
  bias3 <- (bias1 + bias2) / 2
  fluxBias <- data.frame(bias1, bias2, bias3)
  return(fluxBias)
}
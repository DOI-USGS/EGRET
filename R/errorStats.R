#' Error statistics
#'
#' This function takes a fitted WRTDS model and computes error statistics
#' the residuals used here are cross-validation residuals, 
#' which will be slightly larger than regular regression residuals
#' in the case of censored data, the residuals are computed from random residuals
#' computed from makeAugmentedSample(), the function returns a list of error statistics
#' and also prints them to the console
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @export
#' @importFrom stats var
#' @return erStats a numeric vector consisting of the following statistics
#' RsqLC the R squared value for predictions of ln(Concentration)
#' RsqLF the R squared value for predictions of ln(Flux)
#' rmse the root mean squared error for ln(Concentration), same value would apply for Flux
#' sepPercent the standard error of prediction for Concentration, expressed in percent
#'     same value would apply for Flux
#' @examples 
#' eList <- Choptank_eList
#' errorStats(eList)
errorStats <- function(eList) {

  eListR <- makeAugmentedSample(eList)
  Sample <- eListR$Sample
  n <- length(Sample$Dates)
  Sample$Pred <- log(Sample$rObserved) - Sample$rResid
  Sample$trueFlux <- Sample$rObserved * Sample$Q * 86.4
  Sample$trueLogF <- log(Sample$trueFlux)
  VarLC <- var(log(Sample$rObserved))
  VarResid <- var(Sample$rResid)
  RsqLC <- (VarLC - VarResid) / VarLC
  VarLF <- var(Sample$trueLogF)
  RsqLF <- (VarLF - VarResid) / VarLF
  SepC <- 100 * sqrt(exp(VarResid) - 1)
  SepF <- 100 * sqrt(exp(VarResid) - 1)
  rmse <- sqrt(VarResid)
  RsqLC <- format(RsqLC, digits = 3)
  RsqLF <- format(RsqLF, digits = 3)
  rmse <- format(rmse, digits = 3)
  SepC <- format(SepC, digits = 3)
  erStats <- data.frame(RsqLC, RsqLF, rmse, SepC)
  colnames(erStats) <- c("RsqLogC", "RsqLogF", "rmse", "sepPercent")
  cat("\n Root Mean Squared Error in natural log units = ", rmse)
  cat("\n Rsquared for natural log of concentration    = ", RsqLC)
  cat("\n Rsquared for natural log of flux             = ", RsqLF)
  cat("\n Standard error of estimate =",  SepC, "%")
  return(erStats)
}
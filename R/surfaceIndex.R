#'  Compute the 6 parameters needed to lay out the grid for the surfaces computed in estSurfaces
#'
#'     The code here is a repetition of the first part of the code for estSurfaces
#'
#' @param Daily data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return surfaceIndexParameters a numeric vector of length 6, defining the grid for the surfaces
#' @export
#' @examples
#' eList <- Choptank_eList
#' Daily <- getDaily(eList)
#' surfaceIndex(Daily)
surfaceIndex<-function(Daily){
  # this function contains the same code that comes at the start of
  # estSurfaces, it just computes the parameters of the grid 
  # used for the surfaces so that they can be stored for future use
  # the first index is discharge, layed out in 14 equally spaced levels of log(Q)
  # the second index is time, layed out as 16 increments of the calendar year, starting January 1.
  #  Note: I don't think this is the smartest way to do this, but I'm not sure what to do here
  #  I don't like trying to have the same code twice
  #
  
  localDaily <- Daily
  
  bottomLogQ<-min(localDaily$LogQ) - 0.05
  topLogQ<-max(localDaily$LogQ) + 0.05
  stepLogQ<-(topLogQ-bottomLogQ)/13
  vectorLogQ<-seq(bottomLogQ,topLogQ,stepLogQ)
  stepYear<-1/16
  bottomYear<-floor(min(localDaily$DecYear))
  topYear<-ceiling(max(localDaily$DecYear))
  vectorYear<-seq(bottomYear,topYear,stepYear)
  nVectorYear<-length(vectorYear)
  surfaceIndexParameters<-c(bottomLogQ,stepLogQ,14,bottomYear,stepYear,nVectorYear)
  return(surfaceIndexParameters)
}
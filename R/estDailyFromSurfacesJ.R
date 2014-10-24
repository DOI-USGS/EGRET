#' Estimates all daily values of Concentration, Flux, Flow Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these four time series
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augmented version of the Daily data frame, which is returned. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and the surface matrix
#' @keywords water-quality statistics
#' @return localDaily string specifying the name of the data frame containing the daily values and these estimates
#' @export
#' @importFrom fields interp.surface
#' @examples
#' eList <- Choptank_eList
#' #################################################
#' # This is usually done in modelEstimation:
#' Daily <- eList$Daily
#' surfaceIndexParameters<-surfaceIndex(Daily)
#' INFO <- eList$INFO
#' INFO$bottomLogQ<-surfaceIndexParameters[1]
#' INFO$stepLogQ<-surfaceIndexParameters[2]
#' INFO$nVectorLogQ<-surfaceIndexParameters[3]
#' INFO$bottomYear<-surfaceIndexParameters[4]
#' INFO$stepYear<-surfaceIndexParameters[5]
#' INFO$nVectorYear<-surfaceIndexParameters[6]
#' eList$INFO <- INFO
#' #################################################
#' Daily <- estDailyFromSurfaces(eList)
estDailyFromSurfaces <- function(eList) {

  localDaily <- daily(eList)
  localINFO <- info(eList)
  localsurfaces <- surfaces(eList)
  
  # First argument in calls below is the "known" x-y-z surface, second argument is matrix of 
  # "target" x-y points.
  LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  localDaily$yHat <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,1]), 
                                    loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$SE <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,2]), 
                                  loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$ConcDay <- interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$FluxDay <- localDaily$ConcDay * localDaily$Q * 86.4
  
  # Calculate "flow-normalized" concentration and flux:
  
  # First, bin the LogQ values by day-of-year.
  allLogQsByDayOfYear <- split(localDaily$LogQ, localDaily$Day)

  
  allLogQsByDayOfYear[['59']] <- c(unlist(allLogQsByDayOfYear['59']),   # Bob's convention
                                     unlist(allLogQsByDayOfYear['60']))
  allLogQsByDayOfYear['60'] <- allLogQsByDayOfYear['59']
  
  # Using the above data structure as a "look-up" table, list all LogQ values that occured on every
  # day of the entire daily record. When "unlisted" into a vector, these will become the "x" values 
  # for the interpolation.
  allLogQsReplicated <- allLogQsByDayOfYear[localDaily$Day]
  
  # Replicate the decimal year field for each day of the record to correspond to all the LogQ 
  # values listed for that day. These are the "y" values for the interpolation.
  allDatesReplicated <- rep(localDaily$DecYear, lapply(allLogQsReplicated, length))
  
  # Interpolate.
  allConcReplicated <- interp.surface( obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(	unlist(x=allLogQsReplicated),
                                                       y=allDatesReplicated))
  allFluxReplicated <- allConcReplicated * exp(unlist(allLogQsReplicated)) * 86.4
  
  # Finally bin the collective results by days (the decimal year), and calculate the desired means.
  localDaily$FNConc <- tapply(allConcReplicated, allDatesReplicated, "mean")
  localDaily$FNFlux <- tapply(allFluxReplicated, allDatesReplicated, "mean")

  
  return(localDaily)
}

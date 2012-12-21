#' Estimates all daily values of Concentration, Flux, Flow Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these four time series
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augmented version of the Daily data frame, which is returned.
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @param localINFO string specifying the name of the data frame containing the meta-data, default is INFO
#' @param localsurfaces string specifying the name of the array containing the three surfaces, default is surfaces
#' @keywords water-quality statistics
#' @return localDaily string specifying the name of the data frame containing the daily values and these esimates
#' @export
#' @examples
#' \dontrun{estDailyFromSurfaces(localDaily = exDailyStart, localINFO = exINFOStart)}
estDailyFromSurfaces <- function(localDaily = Daily, localINFO = INFO, localsurfaces = surfaces) 
{
  # Do the interpolation from "surfaces" to daily values:
  library("fields")
  
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
  allLogQsByDayOfYear[['365']] <- c(unlist(allLogQsByDayOfYear['365']), 	# Bob's convention
                                    unlist(allLogQsByDayOfYear['366']))
  allLogQsByDayOfYear['366'] <- allLogQsByDayOfYear['365']
  
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

#' Estimates all daily values of Concentration, Flux, Flow-Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these four time series
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augmented version of the Daily data frame, which is returned as part of an EGRET object. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and the surface matrix
#' @keywords water-quality statistics
#' @return egret object with altered Daily dataframe
#' @export
#' @importFrom fields interp.surface
#' @examples
#' eList <- Choptank_eList
#' #################################################
#' # This is usually done in modelEstimation:
#' Daily <- getDaily(eList)
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
#' \dontrun{
#' Daily <- estDailyFromSurfaces(eList)
#' }
estDailyFromSurfaces <- function(eList) {

  localDaily <- getSurfaceEstimates(eList)
  # Calculate "flow-normalized" concentration and flux:
  allLogQsByDayOfYear <- bin_Qs(localDaily)
  
  concFlux_list <- getConcFluxFromSurface(eList, allLogQsByDayOfYear, localDaily)
  
  # Finally bin the collective results by days (the decimal year), and calculate the desired means.
  localDaily$FNConc <-  as.numeric(tapply(concFlux_list[["allConcReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))
  localDaily$FNFlux <-  as.numeric(tapply(concFlux_list[["allFluxReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))

  
  return(localDaily)
}

#' getConcFluxFromSurface
#' 
#' @param allLogQsByDayOfYear
#' @param Daily data frame
#' 
getConcFluxFromSurface <- function(eList, allLogQsByDayOfYear, localDaily){
  localINFO <- getInfo(eList)
  localsurfaces <- getSurfaces(eList)
  
  # First argument in calls below is the "known" x-y-z surface, second argument is matrix of 
  # "target" x-y points.
  LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  
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
  allFluxReplicated <- allConcReplicated * exp(as.numeric(unlist(allLogQsReplicated))) * 86.4
  
  return(list(allFluxReplicated=allFluxReplicated, 
              allConcReplicated=allConcReplicated, 
              allDatesReplicated=allDatesReplicated))
  
}

#' getSurfaceEstimates
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @return Daily dataframe with yHat, SE, ConcDay and FluxDay calulated
getSurfaceEstimates <- function(eList){
  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  localsurfaces <- getSurfaces(eList)
  LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  
  localDaily$yHat <- fields::interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,1]), 
                                    loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$SE <- fields::interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,2]), 
                                  loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$ConcDay <- fields::interp.surface(obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(localDaily$LogQ, localDaily$DecYear))
  localDaily$FluxDay <- as.numeric(localDaily$ConcDay * localDaily$Q * 86.4)
  
  return(localDaily)
}

#' bin_Qs
#' 
#' Bin the LogQ values by day-of-year.
#' @param Daily data frame
#' 
bin_Qs <- function(Daily){

  allLogQsByDayOfYear <- split(Daily$LogQ, Daily$Day)
  
  allLogQsByDayOfYear[['59']] <- c(unlist(allLogQsByDayOfYear['59']),   # Bob's convention
                                   unlist(allLogQsByDayOfYear['60']))
  allLogQsByDayOfYear['60'] <- allLogQsByDayOfYear['59']
  
  return(allLogQsByDayOfYear)
  
}

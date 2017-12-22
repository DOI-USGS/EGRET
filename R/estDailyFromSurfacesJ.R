#' Estimates all daily values of Concentration, Flux, Flow-Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces estimated in estSurfaces to estimate these four time series
#'    in addition to the time series for standard error and yHat (estimated log concentration). 
#'    The results are stored in an augmented version of the Daily data frame, which is returned as part of an EGRET object. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and the surface matrix
#' @param localsurfaces surface over-riding the one stored in eList
#' @param localDaily data frame
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
estDailyFromSurfaces <- function(eList, localsurfaces = NA, localDaily = NA) {

  localDaily <- getSurfaceEstimates(eList, localsurfaces=localsurfaces, localDaily = localDaily)
  # Calculate "flow-normalized" concentration and flux:
  allLogQsByDayOfYear <- bin_Qs(localDaily)
  
  concFlux_list <- getConcFluxFromSurface(eList, allLogQsByDayOfYear, localDaily = localDaily, localsurfaces=localsurfaces)
  
  # Finally bin the collective results by days (the decimal year), and calculate the desired means.
  localDaily$FNConc <-  as.numeric(tapply(concFlux_list[["allConcReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))
  localDaily$FNFlux <-  as.numeric(tapply(concFlux_list[["allFluxReplicated"]], concFlux_list[["allDatesReplicated"]], "mean"))

  return(localDaily)
}

#' getConcFluxFromSurface
#' 
#' @param eList
#' @param allLogQsByDayOfYear
#' @param localDaily data frame
#' @param localsurfaces
#' 
getConcFluxFromSurface <- function(eList, allLogQsByDayOfYear, localDaily, localsurfaces = NA){
  
  if(all(is.na(localsurfaces))){
    localsurfaces <- getSurfaces(eList)
  }
  
  # First argument in calls below is the "known" x-y-z surface, second argument is matrix of 
  # "target" x-y points.
  if("LogQ" %in% names(attributes(localsurfaces))){
    LogQ <- attr(localsurfaces, "LogQ")
  } else {
    localINFO <- getInfo(eList)
    LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  }
  
  if("Year" %in% names(attributes(localsurfaces))){
    Year <- attr(localsurfaces, "Year")
  } else {
    localINFO <- getInfo(eList)
    Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  }
  
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
#' @param localsurfaces
#' @param localDaily data frame
#' @return Daily dataframe with yHat, SE, ConcDay and FluxDay calulated
getSurfaceEstimates <- function(eList, localsurfaces=NA, localDaily = NA){
  
  if(all(is.na(localDaily))){
    localDaily <- getDaily(eList)
  } 
  
  if(all(is.na(localsurfaces))){
    localsurfaces <- getSurfaces(eList)    
  }

  if("LogQ" %in% names(attributes(localsurfaces))){
    LogQ <- attr(localsurfaces, "LogQ")
  } else {
    localINFO <- getInfo(eList)
    LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
  }
  
  if("Year" %in% names(attributes(localsurfaces))){
    Year <- attr(localsurfaces, "Year")
  } else {
    localINFO <- getInfo(eList)
    Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
  }
  
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
#' @param localDaily data frame
#' 
bin_Qs <- function(localDaily){

  allLogQsByDayOfYear <- split(localDaily$LogQ, localDaily$Day)
  
  allLogQsByDayOfYear[['59']] <- c(unlist(allLogQsByDayOfYear['59']),   # Bob's convention
                                   unlist(allLogQsByDayOfYear['60']))
  allLogQsByDayOfYear['60'] <- allLogQsByDayOfYear['59']
  
  return(allLogQsByDayOfYear)
  
}

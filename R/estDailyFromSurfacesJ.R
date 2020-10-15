#' Estimates all daily values of Concentration, Flux, Flow-Normalized Concentration, and Flow Normalized Flux
#'
#'   Uses the surfaces matrix estimated in estSurfaces to estimate 6 daily time series
#'   and appends them to the Daily data frame.  The time series are (in order):
#'   yHat, the estimated natural log of concentration, dimensionless
#'   SE, the standard error of the natural log of concentration
#'   ConcDay, the estimated concentration in mg/L
#'   FluxDay, the estimated flux in kg/day
#'   FNConc, the flow-normalized concentration in mg/L
#'   FNFlux, the flow-normalized flux in kg/day
#'
#'    The results are stored in an augmented version of the Daily data frame, which is returned as part of an EGRET object. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and the surface matrix
#' @param localsurfaces surface over-riding the one stored in eList.  Default is NA.
#' @param localDaily data frame to override eList$Daily.  Default is NA.
#' @keywords water-quality statistics
#' @rdname estDailyFromSurfaces
#' @return egret object with altered Daily dataframe
#' @export
#' @examples
#' eList <- Choptank_eList
#' #################################################
#' # This is usually done in modelEstimation:
#' Daily <- getDaily(eList)
#' surfaceIndexParameters<-surfaceIndex(Daily)
#' INFO <- eList$INFO
#' INFO$bottomLogQ<-surfaceIndexParameters[['bottomLogQ']]
#' INFO$stepLogQ<-surfaceIndexParameters[['stepLogQ']]
#' INFO$nVectorLogQ<-surfaceIndexParameters[['nVectorLogQ']]
#' INFO$bottomYear<-surfaceIndexParameters[['bottomYear']]
#' INFO$stepYear<-surfaceIndexParameters[['stepYear']]
#' INFO$nVectorYear<-surfaceIndexParameters[['nVectorYear']]
#' eList$INFO <- INFO
#' #################################################
#' \donttest{
#' Daily <- estDailyFromSurfaces(eList)
#' }
estDailyFromSurfaces <- function(eList, localsurfaces = NA, localDaily = NA) {
  
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  
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
#' @param allLogQsByDayOfYear list
#' @rdname estDailyFromSurfaces
#' @export
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
    if(all(c("bottomLogQ","stepLogQ","nVectorLogQ") %in% names(localINFO))){
      LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
    } else {
      surfaceIndexParameters<-surfaceIndex(eList$Daily)
      bottomLogQ<-surfaceIndexParameters[['bottomLogQ']]
      stepLogQ<-surfaceIndexParameters[['stepLogQ']]
      nVectorLogQ<-surfaceIndexParameters[['nVectorLogQ']]
      LogQ <- seq(bottomLogQ, by=stepLogQ, length.out=nVectorLogQ)
    }
    
  }
  
  if("Year" %in% names(attributes(localsurfaces))){
    Year <- attr(localsurfaces, "Year")
  } else {
    localINFO <- getInfo(eList)
    if(all(c("bottomYear","stepYear","nVectorYear") %in% names(localINFO))){
      Year <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    } else {
      surfaceIndexParameters <- surfaceIndex(eList$Daily)
      bottomYear <- surfaceIndexParameters[['bottomYear']]
      stepYear <- surfaceIndexParameters[['stepYear']]
      nVectorYear <- surfaceIndexParameters[['nVectorYear']]
      Year <- seq(bottomYear, by=stepYear, length.out=nVectorYear)
    }
  }
  
  # Using the above data structure as a "look-up" table, list all LogQ values that occured on every
  # day of the entire daily record. When "unlisted" into a vector, these will become the "x" values 
  # for the interpolation.
  allLogQsReplicated <- allLogQsByDayOfYear[localDaily$Day]
  
  # Replicate the decimal year field for each day of the record to correspond to all the LogQ 
  # values listed for that day. These are the "y" values for the interpolation.
  allDatesReplicated <- rep(localDaily$DecYear, lapply(allLogQsReplicated, length))
  
  # Interpolate.
  allConcReplicated <- fields::interp.surface( obj=list(x=LogQ,y=Year,z=localsurfaces[,,3]), 
                                       loc=data.frame(	unlist(x=allLogQsReplicated),
                                                       y=allDatesReplicated))
  allFluxReplicated <- allConcReplicated * exp(as.numeric(unlist(allLogQsReplicated))) * 86.4
  
  return(list(allFluxReplicated=allFluxReplicated, 
              allConcReplicated=allConcReplicated, 
              allDatesReplicated=allDatesReplicated))
  
}

#' getSurfaceEstimates
#' 
#' @rdname estDailyFromSurfaces
#' @return Daily dataframe with yHat, SE, ConcDay and FluxDay calulated
#' @export
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
#' @rdname estDailyFromSurfaces
#' @export
bin_Qs <- function(localDaily){

  allLogQsByDayOfYear <- split(localDaily$LogQ, localDaily$Day)
  
  allLogQsByDayOfYear[['59']] <- c(unlist(allLogQsByDayOfYear['59']),   # Bob's convention
                                   unlist(allLogQsByDayOfYear['60']))
  allLogQsByDayOfYear['60'] <- allLogQsByDayOfYear['59']
  
  return(allLogQsByDayOfYear)
  
}

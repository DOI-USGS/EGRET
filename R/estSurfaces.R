#' Estimate the three surfaces (for yHat, SE and ConcHat) as a function of DecYear and logQ and store in the three-dimensional object called surfaces
#'
#' This function uses weighted survival regression to estimate three surfaces that cover the complete range
#' of DecYear and log(Q) values in the Daily data set. 
#' These surfaces are:
#'   (1) is the estimated log concentration (yHat), 
#'   (2) is the estimated standard error (SE), 
#'   (3) is the estimated concentration (ConcHat). 
#' They are mapped as an array that covers the complete space of daily discharge and time. 
#' The first index is discharge, layed out in 14 equally spaced levels of log(Q).
#' The second index is time, layed out as 16 increments of the calendar year, starting January 1.
#'  It returns the 3 dimensional array called surfaces.
#'  This array will be used to estimate these 3 quantities for any given day in the daily values record. 
#'
#' @param eList named list with at least the Sample and Daily dataframes
#' @param surfaceStart Date object for start of surface slice (or character starting date for data retrieval in the form YYYY-MM-DD). Default is NA .
#' @param surfaceEnd Date object for end of surface slice (or character starting date for data retrieval in the form YYYY-MM-DD). Default is NA .
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record. Default is TRUE.
#' @param verbose logical specifying whether or not to display progress message
#' @param interactive logical deprecated. Use 'verbose' instead
#' @param run.parallel logical to run bootstrapping in parallel or not
#' @param localSample data frame to override eList$Sample. Default is NA .
#' @keywords water-quality statistics
#' @return surfaces array containing the three surfaces estimated, array is 3 dimensional
#' @export
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' surfaces <- estSurfaces(eList)
#' 
#' surfaceStart <- "1984-10-01"
#' surfaceEnd <- "1986-09-30"
#' surfaces_1 <- estSurfaces(eList, surfaceStart, surfaceEnd)
#' 
#' wall_sample <- head(eList$Sample, n=500)
#' 
#' surface_wall <- estSurfaces(eList, localSample = wall_sample)
#' 
#' }
estSurfaces<-function(eList, surfaceStart=NA, surfaceEnd=NA, localSample=NA,
                      windowY=7,windowQ=2,windowS=0.5,
                      minNumObs=100,minNumUncen=50,edgeAdjust=TRUE,
                      verbose = TRUE, interactive=NULL,
                      run.parallel = FALSE){
  # this function estimates the 3 surfaces based on the Sample data
  # one is the estimated log concentration (yHat)
  # the second is the estimated standard error (SE)
  # the third is the estimated concentration (ConcHat)
  # they are mapped as an array that covers the complete space of daily discharge and time
  # the first index is discharge, layed out in 14 equally spaced levels of log(Q)
  # the second index is time, layed out as 16 increments of the calendar year, starting January 1.
  # it returns the data frame called surfaces 
  #
  if(!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  
  if(all(is.na(localSample))){
    localSample <- eList$Sample
  }

  highLow <- decimalHighLow(localSample)
  
  DecHigh <- highLow[["DecHigh"]]
  DecLow <- highLow[["DecLow"]]
  
  surfaceInfo <- surfaceIndex(localDaily)
  vectorYear <- surfaceInfo[['vectorYear']]
  vectorLogQ <- surfaceInfo[['vectorLogQ']]
  
  LogQ <- seq(surfaceInfo[['bottomLogQ']], by=surfaceInfo[['stepLogQ']], length.out=surfaceInfo[['nVectorLogQ']])
  
  if(is.na(surfaceStart) && is.na(surfaceEnd)){

    nVectorYear<-length(vectorYear)
    estPtYear<-rep(vectorYear,each=14)
    
    Year <- seq(surfaceInfo[['bottomYear']], by=surfaceInfo[['stepYear']], length.out=surfaceInfo[['nVectorYear']])
    
  } else {

    sliceIndex <- which(vectorYear >= decimalDate(as.Date(surfaceStart)) & vectorYear <= 
                          decimalDate(as.Date(surfaceEnd)))
    Year <- vectorYear[c(sliceIndex[1]-1, sliceIndex, tail(sliceIndex, n = 1)+1)]
    
    nVectorYear <- length(Year)
    estPtYear <- rep(Year,each=14)

  }
  
  estPtLogQ<-rep(vectorLogQ,nVectorYear)

  resultSurvReg<-runSurvReg(estPtYear,estPtLogQ,DecLow,DecHigh,localSample,
                            windowY,windowQ,windowS,minNumObs,minNumUncen,
                            edgeAdjust=edgeAdjust,verbose = verbose,run.parallel=run.parallel)
  
  surfaces<-array(0,dim=c(14,nVectorYear,3))

  for(iQ in 1:14) {
    for(iY in 1:nVectorYear){ 
      k<-(iY-1)*14+iQ
      surfaces[iQ,iY,]<-resultSurvReg[k,]
    }
  }

  attr(surfaces, "surfaceIndex") <- surfaceInfo
  attr(surfaces, "LogQ") <- LogQ
  attr(surfaces, "Year") <- Year
  
  return(surfaces)
}

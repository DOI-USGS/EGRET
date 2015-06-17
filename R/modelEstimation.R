.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}

#' Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)
#'
#' This one function does a jack-knife cross-validation of a WRTDS model, fits the surface
#' (concentration as a function of discharge and time), 
#' estimates daily values of concentration and flux, and flow normalized values. 
#' It returns a named list with the following dataframes: Daily, INFO, Sample, and the matrix: surfaces.
#'
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @keywords water-quality statistics
#' @export
#' @return eList named list with Daily, Sample, and INFO dataframes, along with the surfaces matrix.
#' Any of these values can be NA, not all EGRET functions will work with missing parts of the named list eList.
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#'  
#' #Run an estimation adjusting windowQ from default:
#' eList <- modelEstimation(eList, windowQ=5)
#' }
modelEstimation<-function(eList, 
                          windowY=7, windowQ=2, windowS=0.5,
                          minNumObs=100,minNumUncen=50, 
                          edgeAdjust=TRUE){

  eList <- setUpEstimation(eList=eList, windowY=windowY, windowQ=windowQ, windowS=windowS,
                  minNumObs=minNumObs, minNumUncen=minNumUncen,edgeAdjust=edgeAdjust)

  cat("\n first step running estCrossVal may take about 1 minute")
  Sample1<-estCrossVal(length(eList$Daily$DecYear),eList$Daily$DecYear[1],
                       eList$Daily$DecYear[length(eList$Daily$DecYear)], 
                       eList$Sample, 
                       windowY=windowY, windowQ=windowQ, windowS=windowS,
                       minNumObs=minNumObs, minNumUncen=minNumUncen,edgeAdjust=edgeAdjust)
  
  eList$Sample <- Sample1
  
  cat("\nNext step running  estSurfaces with survival regression:\n")
  surfaces1 <- estSurfaces(eList, 
                         windowY=windowY, windowQ=windowQ, windowS=windowS,
                         minNumObs=minNumObs, minNumUncen=minNumUncen,edgeAdjust=edgeAdjust)

  eList$surfaces <- surfaces1
  
  Daily1<-estDailyFromSurfaces(eList)
  
  eList$Daily <- Daily1
  
  return(eList)
  
}



#' setUpEstimation
#' 
#' Set up the INFO data frame for a modelEstimation
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The modified method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords water-quality statistics
#' @export
#' @return eList named list with Daily, Sample, and INFO dataframes.
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' 
setUpEstimation<-function(eList, 
                          windowY=7, windowQ=2, windowS=0.5,
                          minNumObs=100,minNumUncen=50, 
                          edgeAdjust=TRUE, interactive=TRUE){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(!all(c("Q","LogQ") %in% names(localSample))){
    eList <- mergeReport(INFO=localINFO, Daily = localDaily, Sample = localSample, interactive=interactive)
  }
  
  if(any(localSample$ConcLow[!is.na(localSample$ConcLow)] == 0)){
    stop("modelEstimation cannot be run with 0 values in ConcLow. An estimate of the reporting limit needs to be included. See fixSampleFrame to adjust the Sample data frame")
  }
  
  numDays <- length(localDaily$DecYear)
  DecLow <- localDaily$DecYear[1]
  DecHigh <- localDaily$DecYear[numDays]
    
  surfaceIndexParameters<-surfaceIndex(localDaily)
  localINFO$bottomLogQ<-surfaceIndexParameters[1]
  localINFO$stepLogQ<-surfaceIndexParameters[2]
  localINFO$nVectorLogQ<-surfaceIndexParameters[3]
  localINFO$bottomYear<-surfaceIndexParameters[4]
  localINFO$stepYear<-surfaceIndexParameters[5]
  localINFO$nVectorYear<-surfaceIndexParameters[6]
  localINFO$windowY<-windowY
  localINFO$windowQ<-windowQ
  localINFO$windowS<-windowS
  localINFO$minNumObs<-minNumObs
  localINFO$minNumUncen<-minNumUncen
  localINFO$numDays <- numDays
  localINFO$DecLow <- DecLow
  localINFO$DecHigh <- DecHigh
  localINFO$edgeAdjust <- edgeAdjust
  
  eList$INFO <- localINFO
  
  return(eList)
  
}

#' Flexible Flow Normalization
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param nSegments integer number of flow normalized segments to create
#' @param segStart integer vector of start years (water) for each FN conc/flux segment
#' @param segEnd integer vector of end years (water) for each FN conc/flux segment
#' @param dStart Date vector of start days for each flow segment
#' @param dEnd Date vector of end days for each flow segment
#' 
#' @export
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' nSegments <- 2
#' segStart <- c(1985,2000)
#' segEnd <- c(2001,2010)
#' dStart <- as.Date(c("1988-10-01","2002-05-15"))
#' dEnd <- as.Date(c("1998-09-30","2009-09-30"))
#' eList <- flexFN(eList,nSegments,segStart,segEnd,dStart,dEnd)
#' plotFluxHist(eList)
flexFN <- function(eList, nSegments, segStart, segEnd,dStart,dEnd){
  
  dateInfo <- data.frame(segStart,segEnd,dStart,dEnd)
  
  if(nrow(dateInfo) != nSegments){
    stop("Length of segStart and segEnd must equal nSegments")
  }
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Sample$WaterYear <- as.integer(Sample$DecYear)
  Sample$WaterYear[Sample$Month >= 10] <- Sample$WaterYear[Sample$Month >= 10] +1
  
  Daily$WaterYear <- as.integer(Daily$DecYear)
  Daily$WaterYear[Daily$Month >= 10] <- Daily$WaterYear[Daily$Month >= 10] +1
  
  yStartSample <- min(Sample$WaterYear)
  yEndSample <- max(Sample$WaterYear)
  
  yStartDaily <- min(Daily$WaterYear)
  yEndDaily <- max(Daily$WaterYear)
  
  checkWY <- function(dates, yStart, yEnd){
    segWY <- as.integer(format(dates,"%Y"))
    segMonth <- as.integer(format(dates,"%m"))
    segWY[segMonth > 9] <- segWY[segMonth > 9]+1
    
    if(segWY[1] < yStart | segWY[length(segWY)] > yEnd){
      stop("Flow segment dates outside range")
    }
    return(segWY)
  }
  
  if(segStart[1] < yStartSample | segEnd[length(segEnd)] > yEndSample){
    stop("Sample segment years outside range")
  }
  
  if(nSegments>1){
    for(i in (seq(nSegments-1)+1)){
      if(segStart[i] != segEnd[i-1] -1){
        stop("segStart and segEnd need to be continuous")
      }
    }
  }

  dStartWY <- checkWY(dStart, yStartDaily, yEndDaily)  
  dEndWY <- checkWY(dEnd, yStartDaily, yEndDaily)

  if(any(as.integer(dEnd - dStart) < 365)){
    stop("Each flow normaliziation section must span at least 365 days")
  }

  DailyFN <- estDailyFromSurfaces(eList)
  DailyFN$FNConc <- NULL
  DailyFN$FNFlux <- NULL
  
  newList <- as.egret(eList$INFO,DailyFN,Sample,eList$surfaces)
  
  for(i in seq(nSegments)){
    dailyReturn <- estFNsegs(newList,segStart[i],segEnd[i],dStart[i],dEnd[i])
    DailyFN <- merge(DailyFN, dailyReturn[,c("Date","FNConc","FNFlux")],by=c("Date"),all.x=TRUE)
  }
  FNConcCols <- grep("FNConc",names(DailyFN))
  FNConcCols <- names(DailyFN)[FNConcCols]

  FNFluxCols <- grep("FNFlux",names(DailyFN))
  FNFluxCols <- names(DailyFN)[FNFluxCols]
  
  DailyFN$FNConc <- rowSums(DailyFN[,FNConcCols],na.rm = TRUE)
  DailyFN$FNFlux <- rowSums(DailyFN[,FNFluxCols],na.rm = TRUE)
  
  DailyFN[rowSums(is.na(DailyFN[,FNConcCols]))==length(FNConcCols),"FNConc"] <- NA
  DailyFN[rowSums(is.na(DailyFN[,FNFluxCols]))==length(FNFluxCols),"FNFlux"] <- NA
  
  DailyFN <- DailyFN[,!(names(DailyFN) %in% c(FNConcCols,FNFluxCols))]
  
  INFO <- eList$INFO
  INFO$shortName <- paste0(INFO$shortName,"*")
  INFO$nSegments <- nSegments
  
  attr(INFO,"segmentInfo") <- dateInfo
  
  newList <- as.egret(INFO,DailyFN,Sample,eList$surfaces)
  
  return(newList)
  
}


#' Segment estimates
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param segStart_i integer vector of start years (water) for each FN conc/flux segment
#' @param segEnd_i integer vector of end years (water) for each FN conc/flux segment
#' @param dStart_i Date vector of start days for each flow segment
#' @param dEnd_i Date vector of end days for each flow segment
#' 
#' @export
#' @examples
#' eList <- Choptank_eList
#' eList <- setUpEstimation(eList)
#' nSegments <- 2
#' segStart <- c(1985,2000)
#' segEnd <- c(2001,2010)
#' dStart <- as.Date(c("1988-10-01","2002-05-15"))
#' dEnd <- as.Date(c("1998-09-30","2009-09-30"))
#' eList <- estFNsegs(eList,segStart[1],segEnd[1],dStart[1],dEnd[1])
estFNsegs <- function(eList, segStart_i,segEnd_i,dStart_i,dEnd_i){
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  Daily <- Daily[Daily$Date >= dStart_i & Daily$Date <= dEnd_i,]
  
  Sample$WaterYear <- as.integer(Sample$DecYear)
  Sample$WaterYear[Sample$Month >= 10] <- Sample$WaterYear[Sample$Month >= 10] +1
  Sample <- Sample[Sample$WaterYear >= segStart_i & Sample$WaterYear <= segEnd_i,]
  
  newList <- as.egret(eList$INFO,Daily,Sample,eList$surfaces)
  
  newDaily <- estDailyFromSurfaces(newList)
  return(newDaily)
}
  



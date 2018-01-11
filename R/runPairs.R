#' runPairs
#' 
#' runPairs description
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param year1 integer year1
#' @param year2 integer year2
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param wall logical set up a "wall" on the Sample data
#' @param lastDaySample1 character in YYYY-MM-DD
#' @param firstDaySample2 character in YYYY-MM-DD
#' @param lastDaySample2 character in YYYY-MM-DD
#' @param firstQDate1 character in YYYY-MM-DD
#' @param lastQDate1 character in YYYY-MM-DD
#' @param firstQDate2 character in YYYY-MM-DD
#' @param lastQDate2 character in YYYY-MM-DD 
#' @param windowSide integer number of automatically generated span sections, 
#' default is 7. If NA, cod will use 
#' @param \dots additional parameters
#' 
#' 
#' @examples 
#' eList <- Choptank_eList
#' year1 <- 1985
#' year2 <- 2010
#' 
#' pairOut <- runPairs(eList, year1, year2)
runPairs <- function(eList, year1, year2, 
                     minNumObs = 100, minNumUncen = 50,
                     windowSide = 7, dataInfo = NA,
                     wall = FALSE, lastDaySample1 = NA, 
                     firstDaySample2 = NA, lastDaySample2 = NA,
                     firstQDate1 = NA, lastQDate1 = NA,
                     firstQDate2 = NA, lastQDate2 = NA, ...){
  
  localDaily <- eList$Daily
  localSample <- eList$Sample
  
  firstQDate0 <- localDaily$Date[1]
  lastQDate0 <- localDaily$Date[length(localDaily$Date)]

  firstDaySample1 <- localSample$Date[1]

  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }

  startEnd1 <- startEnd(paStart, paLong, year1)
  startEnd2 <- startEnd(paStart, paLong, year2)
  # need an error catch here, stop the execution if any one or more of these conditions exist
  #  it needs to tell user that the specified years in the pair run beyond the limits of the data
  if(as.Date(startEnd1[["startDate"]]) < as.Date(firstQDate1)){
    stop("")
  }
  if(as.Date(startEnd1[["endDate"]]) > as.Date(lastQDate1)) {
    stop()
  }
  if(as.Date(startEnd2[["startDate"]]) < as.Date(firstQDate2)){
    stop()
  } 
  if(as.Date(startEnd2[["endDate"]]) > as.Date(lastQDate2)){
    stop()  
  }
  
  
  Sample1 <- localSample[localSample$Date >= firstDaySample1 & 
                           localSample$Date <= lastDaySample1,]
  Sample2 <- localSample[localSample$Date >= firstDaySample2 &
                           localSample$Date <= lastDaySample2,]
  
  surfaces1 <- estSurfaces(eList,surfaceStart = startEnd1[1], surfaceEnd = startEnd1[2], localSample = Sample1,
                           minNumObs = minNumObs, minNumUncen = minNumUncen, verbose = FALSE)
  surfaces2 <- estSurfaces(eList,surfaceStart = startEnd2[1], surfaceEnd = startEnd2[2], localSample = Sample2,
                           minNumObs = minNumObs, minNumUncen = minNumUncen, verbose = FALSE)

  Daily1 <- localDaily[localDaily$Date >= firstQDate1 &
                         localDaily$Date <=  lastQDate1,]
  Daily2 <- localDaily[localDaily$Date >= firstQDate2 &
                         localDaily$Date <=  lastQDate2,]
  
  DailyRS1FD1 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, localDaily = Daily1)
  annualFlex <- setupYears(DailyRS1FD1, paLong = paLong, paStart = paStart)
  c11 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f11 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  
  DailyRS2FD2 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, localDaily = Daily2)
  annualFlex <- setupYears(DailyRS2FD2, paLong = paLong, paStart = paStart)
  c22 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f22 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  
  Daily0 <- localDaily[localDaily$Date >= firstQDate0 &
                         localDaily$Date <=  lastQDate0,]
  DailyRS1FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, localDaily = Daily0)
  annualFlex <- setupYears(DailyRS1FD0, paLong = paLong, paStart = paStart)
  c10 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f10 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  
  DailyRS2FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, localDaily = Daily0)
  annualFlex <- setupYears(DailyRS2FD0, paLong = paLong, paStart = paStart)
  c20 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f20 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  cDeltaTotal <- c22 - c11
  cRSpart <- c20 - c10
  cFDpart <- cDeltaTotal - cRSpart
  fDeltaTotal <- f22 - f11
  fRSpart <- f20 - f10
  fFDpart <- fDeltaTotal - fRSpart
  pairResults <- as.data.frame(matrix(ncol = 7, nrow = 2))
  colnames(pairResults) <- c("DeltaTotal","RSpart","FDpart",
                             "x10","x11","x20","x22")
  rownames(pairResults) <- c("Conc","Flux")
  pairResults[1,] <- c(cDeltaTotal, cRSpart, cFDpart, c10, c11, c20, c22)
  pairResults[2,] <- 0.00036525 * c(fDeltaTotal, fRSpart, fFDpart, f10, f11, f20, f22)
  # first row is concentration, in mg/L
  # second row is flux in 10^6 kg / year
  attr(pairResults, "yearPair") <- c(paStart, paLong, year1, year2)
  attr(pairResults, "FDblocks") <- c(firstQDate1, lastQDate1, firstQDate2, lastQDate2)
  
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(paStart, paLong)
  cat("\n  ", periodName, "\n")
  if(wall) cat("\n Sample data set was partitioned with a wall at ", as.character(lastDaySample1), "\n")
  cat("\n Change estimates ",year2," minus ", year1,"\n")
  totChange <- format(pairResults[1,1], digits = 3)
  totChangePct <- format(100 * ((c22-c11)/c11), digits = 2)
  cat("\n For concentration: total change is ",totChange,"mg/L" )
  cat("\n expressed as Percent Change is ", totChangePct,"%")
  pctRS <- format(100 * (cRSpart/cDeltaTotal), digits = 2)
  pctFD <- format(100 * (cFDpart/cDeltaTotal), digits = 2)
  cat("\n RS percent of total ", pctRS,"%,    FD percent of total ", pctFD,"% \n\n")
  totChange <- format(pairResults[2,1], digits = 3)
  totChangePct <- format(100 * ((f22-f11)/f11), digits = 2)
  cat("\n For flux: total change is ",totChange,"million kg/year" )
  cat("\n expressed as Percent Change is ", totChangePct,"%")
  pctRS <- format(100 * (fRSpart/fDeltaTotal), digits = 2)
  pctFD <- format(100 * (fFDpart/fDeltaTotal), digits = 2)
  cat("\n RS percent of total ", pctRS,"%,    FD percent of total ", pctFD,"% \n\n")
  print(pairResults)
  return(pairResults)
}
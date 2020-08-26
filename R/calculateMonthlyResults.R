#' Calculates monthly mean values of Q, Conc, Flux, FNConc, and FNFlux for the entire record.  
#'
#' Computes the monthly mean values of discharge, concentration, flux, flow-normalized concentration and flow-normalized flux (Q, Conc, Flux, FNConc, and FNFlux) in SI units 
#' Note that the Flux and FNFlux values are average flux values (not totals). For discharge they are in m3/s, concentration is mg/L, and flux is kg/day.
#' It returns a data frame containing month, year, decimal year, and mean values of DecYear, Q, Conc, Flux, FNConc, and FNFlux.
#'
#' @param eList named list with at least the Daily dataframes
#' @keywords water-quality statistics
#' @return MonthlyResults data frame of numeric values describing the monthly average values
#' @export
#' @examples
#' eList <- Choptank_eList
#' monthlyResults <- calculateMonthlyResults(eList)
calculateMonthlyResults<-function(eList){
  
  localDaily <- getDaily(eList)

  UseIt <- aggregate(localDaily$ConcDay, by=list(localDaily$MonthSeq), function(x) sum(!is.na(x)))$x

  Q <- aggregate(localDaily$Q, by=list(localDaily$MonthSeq), mean)$x
  DecYear <- aggregate(localDaily$DecYear, by=list(localDaily$MonthSeq), mean)$x
  Year <- trunc(DecYear)
  Month <- aggregate(localDaily$Month, by=list(localDaily$MonthSeq), mean)$x
  Conc <- aggregate(localDaily$ConcDay, by=list(localDaily$MonthSeq), mean)$x
  Flux <- aggregate(localDaily$FluxDay, by=list(localDaily$MonthSeq), mean)$x
  FluxTotal <- aggregate(localDaily$FluxDay, by=list(localDaily$MonthSeq), sum)$x
  FNConc <- aggregate(localDaily$FNConc, by=list(localDaily$MonthSeq), mean)$x
  FNFlux <- aggregate(localDaily$FNFlux, by=list(localDaily$MonthSeq), mean)$x
  FNFluxTotal <- aggregate(localDaily$FNFlux, by=list(localDaily$MonthSeq), sum)$x
  
  kalman <- all(c("GenConc","GenFlux") %in% names(localDaily))
  
  if(kalman){
    GenConc <- aggregate(localDaily$GenConc, by=list(localDaily$MonthSeq), mean)$x
    GenFlux <- aggregate(localDaily$GenFlux, by=list(localDaily$MonthSeq), mean)$x
    GenFluxTotal <- aggregate(localDaily$GenFlux, 
                              by = list(localDaily$MonthSeq), sum)$x

    MonthlyResults <- data.frame(Month, Year, 
                                 DecYear, Q,
                                 Conc, 
                                 Flux, FluxTotal, 
                                 FNConc, 
                                 FNFlux, FNFluxTotal,
                                 GenConc, GenFlux,
                                 GenFluxTotal)
    
  } else {
    MonthlyResults <- data.frame(Month, Year, 
                                 DecYear, Q,
                                 Conc, Flux, FluxTotal, 
                                 FNConc, FNFlux, FNFluxTotal)  
  }
  
  MonthlyResults <- MonthlyResults[(UseIt > 15),]
  
  return(MonthlyResults)
}
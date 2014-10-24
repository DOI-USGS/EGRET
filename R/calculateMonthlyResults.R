#' Calculates monthly values of Q, Conc, Flux, FNConc, and FNFlux for the entire record
#'
#' Computes the monthly mean values of discharge, concentration, flux, flow-normalized concentration and flow-normalized flux (Q, Conc, Flux, FNConc, and FNFlux) in SI units 
#' (For discharge they are in m3/s, concentration is mg/L, and flux is kg/day).
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
  
  localDaily <- daily(eList)

  UseIt <- aggregate(localDaily$ConcDay, by=list(localDaily$MonthSeq), function(x) sum(!is.na(x)))$x

  Q <- aggregate(localDaily$Q, by=list(localDaily$MonthSeq), mean)$x
  DecYear <- aggregate(localDaily$DecYear, by=list(localDaily$MonthSeq), mean)$x
  Year <- trunc(DecYear)
  Month <- aggregate(localDaily$Month, by=list(localDaily$MonthSeq), mean)$x
  Conc <- aggregate(localDaily$ConcDay, by=list(localDaily$MonthSeq), mean)$x
  Flux <- aggregate(localDaily$FluxDay, by=list(localDaily$MonthSeq), mean)$x
  FNConc <- aggregate(localDaily$FNConc, by=list(localDaily$MonthSeq), mean)$x
  FNFlux <- aggregate(localDaily$FNFlux, by=list(localDaily$MonthSeq), mean)$x
  
  MonthlyResults <- data.frame(Month, Year, DecYear, Q, Conc, 
                               Flux, FNConc, FNFlux)
  MonthlyResults <- MonthlyResults[(UseIt > 15),]
  
  return(MonthlyResults)
}
#'    Calculates monthly values of Discharge, Concentration, Flux, Flow Normalized Concentration and Flow Normalized Flux for the entire record
#'
#'     Computes the monthly average values of these five quantities (Q, Conc, Flux, FNConc, and FNFlux).
#'     It also saves (for each month) the month sequence number (months starting with January, 1850) and the average value of DecYear.
#'     It returns a data frame containing MonthSeq and average values of DecYear, Q, Conc, Flux, FNConc, and FNFlux.
#'
#' @param localDaily string specifying the name of the data frame containing the daily values, default is Daily
#' @keywords water-quality statistics
#' @return MonthlyResults data frame of numeric values describing the monthly average values
#' @export
#' @examples
#' Daily <- ChopDaily
#' monthlyResults <- calculateMonthlyResults()
calculateMonthlyResults<-function(localDaily = Daily){
  # this creates a data frame of monthly results from Daily
  # it requires that there be at least 15 valid values in the month
  # to compute those results
  # it returns the data frame called MonthlyResults
  #
  firstMonthSeq<-localDaily$MonthSeq[1]
  numDays<-length(localDaily$MonthSeq)
  lastMonthSeq<-localDaily$MonthSeq[numDays]
  numMonths<-lastMonthSeq-firstMonthSeq+1
  DecYear<-rep(NA,numMonths)
  Q<-rep(NA,numMonths)
  MonthSeq<-seq(firstMonthSeq,lastMonthSeq)
  Conc<-rep(NA,numMonths)
  Flux<-rep(NA,numMonths)
  FNConc<-rep(NA,numMonths)
  FNFlux<-rep(NA,numMonths)
  for(i in 1:numMonths) {
    thisMonthSeq<-MonthSeq[i]
    DailyM<-subset(localDaily,MonthSeq==thisMonthSeq)
    UseIt<-ifelse(is.na(DailyM$ConcDay),0,1)
    Keep<-if(sum(UseIt)>15) TRUE else FALSE
    Q[i]<-if(Keep) mean(DailyM$Q) else NA
    DecYear[i]<-if(Keep) mean(DailyM$DecYear) else NA
    Conc[i]<-if(Keep) mean(DailyM$ConcDay) else NA
    Flux[i]<-if(Keep) mean(DailyM$FluxDay) else NA
    FNConc[i]<-if(Keep) mean(DailyM$FNConc) else NA
    FNFlux[i]<-if(Keep) mean(DailyM$FNFlux) else NA
  }
  MonthlyResults<-data.frame(MonthSeq,DecYear,Q,Conc,Flux,FNConc,FNFlux)
  return(MonthlyResults)	
}
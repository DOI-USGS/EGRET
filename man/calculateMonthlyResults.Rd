\name{calculateMonthlyResults}
\alias{calculateMonthlyResults}
\title{Calculates monthly values of Discharge, Concentration, Flux, Flow Normalized Concentration and Flow Normalized Flux for the entire record}
\usage{
calculateMonthlyResults(localDaily = Daily)
}
\arguments{
  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}
}
\value{
MonthlyResults data frame of numeric values describing the
monthly average values
}
\description{
Computes the monthly average values of these five
quantities (Q, Conc, Flux, FNConc, and FNFlux).  It also
saves (for each month) the month sequence number (months
starting with January, 1850) and the average value of
DecYear.  It returns a data frame containing MonthSeq and
average values of DecYear, Q, Conc, Flux, FNConc, and
FNFlux.
}
\examples{
Daily <- ChopDaily
monthlyResults <- calculateMonthlyResults()
}
\keyword{statistics}
\keyword{water-quality}


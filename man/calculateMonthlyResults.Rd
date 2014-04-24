\name{calculateMonthlyResults}
\alias{calculateMonthlyResults}
\title{Calculates monthly values of Q, Conc, Flux, FNConc, and FNFlux for the entire record}
\usage{
calculateMonthlyResults(localDaily = Daily)
}
\arguments{
  \item{localDaily}{data frame containing the daily values,
  default is Daily}
}
\value{
MonthlyResults data frame of numeric values describing the
monthly average values
}
\description{
Computes the monthly mean values of discharge,
concentration, flux, flow-normalized concentration and
flow-normalized flux (Q, Conc, Flux, FNConc, and FNFlux) in
SI units (For discharge they are in m3/s, concentration is
mg/L, and flux is kg/day). It returns a data frame
containing month, year, decimal year, and mean values of
DecYear, Q, Conc, Flux, FNConc, and FNFlux.
}
\examples{
Daily <- ChopDaily
monthlyResults <- calculateMonthlyResults()
}
\keyword{statistics}
\keyword{water-quality}


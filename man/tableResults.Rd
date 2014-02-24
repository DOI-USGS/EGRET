\name{tableResults}
\alias{tableResults}
\title{Table of annual results for discharge, concentration and flux}
\usage{
tableResults(localAnnualResults = AnnualResults, localINFO = INFO,
  qUnit = 2, fluxUnit = 9, returnDataFrame = FALSE)
}
\arguments{
  \item{localAnnualResults}{data frame that contains the
  results, default name is AnnualResults}

  \item{localINFO}{data frame that contains the metadata,
  default name is INFO}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{fluxUnit}{object of fluxUnit class.
  \code{\link{fluxConst}}, or numeric represented the short
  code, or character representing the descriptive name.}

  \item{returnDataFrame}{logical.  If a dataframe is
  required, set this to TRUE.  Otherwise, the default is
  FALSE.}
}
\value{
results dataframe, if returnDataFrame=TRUE

dataframe with year, discharge, concentration,
flow-normalized concentration, flux, and flow-normalized
concentration columns.
}
\description{
Produce an ASCII table showing: year, mean discharge, mean
concentration, flow-normalized concentration, mean flux,
and flow-normalized flux. Uses results stored in
AnnualResults and INFO data frames.
}
\examples{
Daily <- ChopDaily
AnnualResults <- setupYears()
INFO <- ChopINFO
tableResults(fluxUnit = 1)
tableResults(fluxUnit = 'kgDay', qUnit = 'cms')
returnedTable <- tableResults(fluxUnit = 1, returnDataFrame = TRUE)
}
\keyword{statistics}
\keyword{water-quality}


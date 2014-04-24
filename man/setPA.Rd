\name{setPA}
\alias{setPA}
\title{Sets up the period of analysis to be used in makeAnnualSeries}
\usage{
  setPA(paStart = 10, paLong = 12, window = 30,
    localINFO = INFO)
}
\arguments{
  \item{paStart}{A numeric value for the starting month of
  the Period of Analysis, default is 10}

  \item{paLong}{A numeric value for the length of the
  Period of Analysis in months, default is 12}

  \item{window}{A numeric value for the half-width of a
  smoothing window for annual streamflow values, default is
  30}

  \item{localINFO}{data frame that contains the metadata,
  default is INFO}
}
\value{
  localInfo A data frame containing the metadata
}
\description{
  Part of the flowHistory system. Period of analysis is
  defined by the starting month (paStart) and length in
  months (paLong). paStart and paLong are constrained to be
  integers from 1 to 12. for example, a water year would be
  paStart = 10 and paLong = 12. for example, the winter
  season, defined by Dec,Jan,Feb would be paStart = 12 and
  paLong =3.
}
\examples{
INFO <- ChopINFO
setPA(paStart=12, paLong=3)
}
\keyword{statistics}
\keyword{streamflow}


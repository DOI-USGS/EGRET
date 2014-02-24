\name{estDailyWithoutNormalization}
\alias{estDailyWithoutNormalization}
\title{Estimates all daily values of Concentration, and Flux}
\usage{
estDailyWithoutNormalization(localDaily = Daily, localINFO = INFO,
  localsurfaces = surfaces)
}
\arguments{
  \item{localDaily}{data frame containing the daily values,
  default is Daily}

  \item{localINFO}{data frame containing the meta-data,
  default is INFO}

  \item{localsurfaces}{string specifying the name of the
  array containing the three surfaces, default is surfaces}
}
\value{
localDaily string specifying the name of the data frame
containing the daily values and these esimates
}
\description{
Uses the surfaces estimated in estSurfaces to estimate
these two time series in addition to the time series for
standard error and yHat (estimated log concentration).  The
results are stored in an augemented version of the Daily
data frame, which is returned.  This code is identical to
estDailyFromSurfaces but it lacks the flow normalization
process.  The exclusion of the flow-normalization process
saves a large amount of computer time.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
surfaces <- exsurfaces
Daily <- estDailyWithoutNormalization()
}
\keyword{statistics}
\keyword{water-quality}


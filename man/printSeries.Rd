\name{printSeries}
\alias{printSeries}
\title{Print annual results for a given streamflow statistic}
\usage{
  printSeries(istat, qUnit = 1, runoff = FALSE,
    localINFO = INFO, localAnnualSeries = annualSeries)
}
\arguments{
  \item{istat}{A numeric value for the flow statistic to be
  graphed (possible values are 1 through 8)}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{runoff}{logical variable, if TRUE the streamflow
  data are converted to runoff values in mm/day}

  \item{localINFO}{A character string specifying the name
  of the metadata data frame}

  \item{localAnnualSeries}{A character string specifying
  the name of a data frame containing the annual series}
}
\description{
  Part of the flowHistory system. The index of the flow
  statistics is istat.  These statistics are: (1) 1-day
  minimum, (2) 7-day minimum, (3) 30-day minimum, (4)
  median (5) mean, (6) 30-day maximum, (7) 7-day maximum,
  and (8) 1-day maximum.  User must have run setPA and
  makeAnnualSeries before this function.
}
\examples{
INFO <- exINFO
annualSeries <- exannualSeries
printSeries(5)
}
\keyword{statistics}
\keyword{streamflow}


\name{tableFlowChange}
\alias{tableFlowChange}
\title{Prints table of change metrics for a given streamflow statistic}
\usage{
  tableFlowChange(istat, localAnnualSeries = annualSeries,
    localINFO = INFO, qUnit = 1, runoff = FALSE,
    yearPoints = NA)
}
\arguments{
  \item{istat}{A numeric value for the flow statistic to be
  graphed (possible values are 1 through 8)}

  \item{localINFO}{A character string specifying the name
  of the metadata data frame}

  \item{localAnnualSeries}{A character string specifying
  the name of a data frame containing the annual series}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{runoff}{logical variable, if TRUE the streamflow
  data are converted to runoff values in mm/day}

  \item{yearPoints}{A vector of numeric values, specifying
  the years at which change metrics are to be calculated,
  default is NA (which allows the function to set these
  automatically), yearPoints must be in ascending order}
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
INFO <- setPA(paStart=12, paLong=3)
annualSeries <- exannualSeries
tableFlowChange(istat=5,yearPoints=c(2001,2005,2009))
}
\keyword{statistics}
\keyword{streamflow}


\name{plotFourStats}
\alias{plotFourStats}
\title{Makes four graphs of annual streamflow statistics on a single page}
\usage{
  plotFourStats(localINFO = INFO,
    localAnnualSeries = annualSeries, yearStart = NA,
    yearEnd = NA, printTitle = TRUE, runoff = FALSE,
    qUnit = 1, cex.lab = 1.4, cex.axis = 1.2, cex = 0.6,
    ...)
}
\arguments{
  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, defoult name is INFO}

  \item{localAnnualSeries}{string specifying the name of
  the data frame that contains the annual series of
  statistics, default is annualSeries}

  \item{yearStart}{A numeric value for year in which the
  graph should start, default is NA, which indicates that
  the graph should start with first annual value}

  \item{yearEnd}{A numeric value for year in which the
  graph should end, default is NA, which indicates that the
  graph should end with last annual value}

  \item{printTitle}{logical variable, if TRUE title is
  printed, if FALSE title is not printed, default is TRUE}

  \item{runoff}{logical variable, if TRUE the streamflow
  data are converted to runoff values in mm/day}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{cex.lab}{number}

  \item{cex.axis}{number}

  \item{cex}{number}
}
\description{
  Part of the flowHistory system.  The four statistics are
  1-day maximum, annual mean, annual median, and annual
  7-day minimum.  Prior to running this code user must have
  run setPA and makeAnnualSeries.
}
\examples{
INFO <- exINFO
annualSeries <- exannualSeries
plotFourStats()
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


\name{plotFour}
\alias{plotFour}
\title{Makes four graphs of streamflow statistics on a single page}
\usage{
  plotFour(localINFO = INFO,
    localAnnualSeries = annualSeries, localDaily = Daily,
    yearStart = NA, yearEnd = NA, printTitle = TRUE,
    runoff = FALSE, qUnit = 1, window = 15, cex = 0.6,
    cex.lab = 1.4, cex.axis = 1.2)
}
\arguments{
  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, defoult name is INFO}

  \item{localAnnualSeries}{string specifying the name of
  the data frame that contains the annual series of
  statistics, default is annualSeries}

  \item{localDaily}{string specifying the name of the data
  frame that contains the daily values, default is Daily}

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

  \item{window}{numeric which is the full width, in years,
  of the time window over which the standard deviation is
  computed, default = 15}

  \item{cex}{number}

  \item{cex.lab}{number}

  \item{cex.axis}{number}
}
\description{
  Part of the flowHistory system.  The four statistics are
  1-day maximum, annual mean, annual 7-day minimum, and the
  running standard deviation of the log daily discharge
  values.  Prior to running this code user must have run
  setPA and makeAnnualSeries.
}
\examples{
INFO <- exINFO
annualSeries <- exannualSeries
Daily <- exDaily
plotFour(window=2)
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


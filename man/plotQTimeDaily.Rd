\name{plotQTimeDaily}
\alias{plotQTimeDaily}
\title{Plot of the discharge time series}
\usage{
  plotQTimeDaily(startYear, endYear, localDaily = Daily,
    localINFO = INFO, qLower = NA, qUnit = 1,
    tinyPlot = FALSE, printTitle = TRUE)
}
\arguments{
  \item{startYear}{numeric indicating the starting year for
  the graph}

  \item{endYear}{numeric indicating the ending year for the
  graph (should be a time in decimal years that is after
  the last observations to be plotted)}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qLower}{numeric specifying the lower bound on
  discharges that are to be plotted, must be in the units
  specified by qUnit, default is NA (lower bound is zero)}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.  Default is qUnit=1
  (cubic feet per second)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be short and wide, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}
}
\description{
  Part of flowHistory component. Allows discharge record to
  only show those discharges above a given threshold
}
\examples{
Daily <- exDaily
INFO <- exINFO
plotQTimeDaily(1990,2000,qLower=10)
}
\keyword{graphics}
\keyword{streamflow}


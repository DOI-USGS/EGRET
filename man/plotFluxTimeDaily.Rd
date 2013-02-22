\name{plotFluxTimeDaily}
\alias{plotFluxTimeDaily}
\title{Plot of the time series of daily flux estimates and the sample values for the days that were sampled}
\usage{
  plotFluxTimeDaily(startYear, endYear,
    localSample = Sample, localDaily = Daily,
    localINFO = INFO, tinyPlot = FALSE, fluxUnit = 3,
    fluxMax = NA, printTitle = TRUE)
}
\arguments{
  \item{startYear}{numeric specifying the starting date
  (expressed as decimal years, for example 1989.0) for the
  plot}

  \item{endYear}{numeric specifiying the ending date for
  the plot}

  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be short and wide, default is FALSE.}

  \item{fluxUnit}{number representing in pre-defined
  fluxUnit class array. \code{\link{fluxConst}}}

  \item{fluxMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}
}
\description{
  This plot is useful for visual examination of the ability
  of the WRTDS, or other model, to fit the data, as seen in
  a time-series perspective.
}
\examples{
Sample <- exSample
Daily <- exDaily
INFO <- exINFO
plotFluxTimeDaily(2001,2009)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


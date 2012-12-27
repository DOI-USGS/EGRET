\name{plotConcTimeDaily}
\alias{plotConcTimeDaily}
\title{Plot of the time series of daily concentration estimates and the sample values for the days that were sampled}
\usage{
  plotConcTimeDaily(startYear, endYear,
    localSample = Sample, localDaily = Daily,
    localINFO = INFO, tinyPlot = FALSE, concMax = NA,
    printTitle = TRUE)
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

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}
}
\description{
  This plot is useful for visual examination of the ability
  of the WRTDS, or other model, to fit the data, seen in a
  time-series perspective. The graph is most useful when it
  covers a period of just a few years and not the complete
  record but a complete record can be done by repeated use
  over a series of segments.
}
\examples{
Sample <- exSampleEnd
Daily <- exDailyEnd
INFO <- exINFOEnd
plotConcTimeDaily(2001,2010)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


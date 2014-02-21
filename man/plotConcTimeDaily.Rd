\name{plotConcTimeDaily}
\alias{plotConcTimeDaily}
\title{Plot of the time series of daily concentration estimates and the sample values for the days that were sampled}
\usage{
plotConcTimeDaily(startYear = NA, endYear = NA, localSample = Sample,
  localDaily = Daily, localINFO = INFO, tinyPlot = FALSE, concMax = NA,
  printTitle = TRUE, cex = 0.8, cex.axis = 1.1, cex.main = 1.1,
  customPar = FALSE, col = "black", lwd = 1, ...)
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

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function
  (for example, adjusting margins with
  par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses
  the best margins depending on tinyPlot.}

  \item{col}{color of points on plot, see ?par 'Color
  Specification'}

  \item{lwd}{number line width}

  \item{\dots}{arbitrary functions sent to the generic
  plotting function.  See ?par for details on possible
  parameters}
}
\description{
This plot is useful for visual examination of the ability
of the WRTDS, or other model, to fit the data, seen in a
time-series perspective. The graph is most useful when it
covers a period of just a few years and not the complete
record but a complete record can be done by repeated use
over a series of segments.

Although there are a lot of optional arguments to this
function, most are set to a logical default. If your
workspace contains an INFO, Daily, and Sample dataframes,
then the following R code will produce a plot:
\code{plotConcTimeDaily()}
}
\examples{
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
plotConcTimeDaily()
plotConcTimeDaily(startYear=1998,endYear=2001)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


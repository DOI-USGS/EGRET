\name{plotFluxTimeDaily}
\alias{plotFluxTimeDaily}
\title{Plot of the time series of daily flux estimates and the sample values for the days that were sampled}
\usage{
  plotFluxTimeDaily(startYear, endYear,
    localSample = Sample, localDaily = Daily,
    localINFO = INFO, tinyPlot = FALSE, fluxUnit = 3,
    fluxMax = NA, printTitle = TRUE, cex = 0.8,
    cex.axis = 1.1, cex.main = 1.1, customPar = FALSE,
    col = "black", lwd = 1, ...)
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

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  This plot is useful for visual examination of the ability
  of the WRTDS, or other model, to fit the data, as seen in
  a time-series perspective.
}
\examples{
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
plotFluxTimeDaily(2001,2009)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


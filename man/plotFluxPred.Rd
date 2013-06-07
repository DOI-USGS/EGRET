\name{plotFluxPred}
\alias{plotFluxPred}
\title{Graph of observed versus estimated flux}
\usage{
  plotFluxPred(localSample = Sample, localINFO = INFO,
    fluxUnit = 3, fluxMax = NA, printTitle = TRUE,
    oneToOneLine = TRUE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, tinyPlot = FALSE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{fluxUnit}{number representing entry in pre-defined
  fluxUnit class array. \code{\link{fluxConst}}}

  \item{fluxMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{oneToOneLine}{inserts 1:1 line}

  \item{tinyPlot}{logical variable if TRUE plot is designed
  to be small, if FALSE it is designed for page size,
  default is FALSE (not fully implemented yet)}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Data come from a data frame named Sample which contains
  the Sample Data. The metadata come from a data frame
  named INFO.
}
\examples{
Sample <- exSample
INFO <- exINFO
plotFluxPred(fluxUnit = 7)
plotFluxPred(fluxUnit = 'poundsDay')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


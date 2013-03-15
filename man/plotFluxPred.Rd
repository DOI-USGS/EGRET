\name{plotFluxPred}
\alias{plotFluxPred}
\title{Graph of observed versus estimated flux}
\usage{
  plotFluxPred(localSample = Sample, localINFO = INFO,
    fluxUnit = 3, fluxMax = NA, tinyPlot = FALSE,
    printTitle = TRUE, ...)
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

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{...}{arbitrary graphical parameters that will be
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


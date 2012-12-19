\name{plotLogFluxQ}
\alias{plotLogFluxQ}
\title{Sample data plot: observed log flux vs log discharge}
\usage{
  plotLogFluxQ(localSample = Sample, localINFO = INFO,
    qUnit = 2, fluxUnit = 3, tinyPlot = FALSE,
    fluxMax = NA, fluxMin = NA, printTitle = TRUE)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration and discharge data,
  default name is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{fluxUnit}{object of fluxUnit class.
  \code{\link{fluxConst}}, or numeric represented the short
  code, or character representing the descriptive name.}

  \item{tinyPlot}{logical variable if TRUE plot is designed
  to fit into a multi-plot array, default is FALSE}

  \item{fluxMax}{numeric specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{fluxMin}{numeric specifying the minimum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}
}
\description{
  Concentration and discharge data used to compute flux
  come from a data frame named Sample which contains the
  sample data. The metadata come from a data frame named
  INFO.
}
\examples{
plotLogFluxQ(localSample = exSampleEnd, localINFO = exINFOEnd, qUnit = 1, fluxUnit = 1)
plotLogFluxQ(localSample = exSampleEnd, localINFO = exINFOEnd, fluxUnit = 'kgDay')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


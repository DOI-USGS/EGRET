\name{plotLogFluxPred}
\alias{plotLogFluxPred}
\title{Sample data plot:  observed log flux vs predicted log flux}
\usage{
  plotLogFluxPred(localSample = Sample, localINFO = INFO,
    fluxUnit = 3, fluxMax = NA, tinyPlot = FALSE,
    printTitle = TRUE, oneToOneLine = TRUE, cex = 0.8,
    cex.axis = 1.1, cex.main = 1.1, customPar = FALSE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{fluxUnit}{object of qUnit class.
  \code{\link{qConst}}}

  \item{fluxMax}{numeric specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{oneToOneLine}{inserts 1:1 line}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{customPar}{logical defaults to FALSE. If TRUE, par
  should be set by user, if FALSE, EGRET chooses best
  graphical parameters.}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Concentration data come from a data frame named Sample
  which contains the sample data. The metadata come from a
  data frame named INFO.
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
plotLogFluxPred(fluxUnit = 1)
plotLogFluxPred(fluxUnit = 'kgDay')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


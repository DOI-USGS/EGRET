\name{plotLogFluxPred}
\alias{plotLogFluxPred}
\title{Sample data plot:  observed log flux vs predicted log flux}
\usage{
  plotLogFluxPred(localSample = Sample, localINFO = INFO,
    fluxUnit = 3, fluxMax = NA, tinyPlot = TRUE,
    printTitle = TRUE, oneToOneLine = TRUE, ...)
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
Sample <- exSample
INFO <- exINFO
plotLogFluxPred(fluxUnit = 1)
plotLogFluxPred(fluxUnit = 'kgDay')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


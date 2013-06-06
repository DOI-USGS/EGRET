\name{plotResidPred}
\alias{plotResidPred}
\title{Plot of the residuals from WRTDS versus the estimated values (all in log concentration units)}
\usage{
  plotResidPred(localSample = Sample, localINFO = INFO,
    stdResid = FALSE, tinyPlot = FALSE, printTitle = TRUE,
    cex = 0.8, cex.axis = 1.1, cex.main = 1.1, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{stdResid}{logical variable, if TRUE it uses the
  standardized residual, if FALSE it uses the actual,
  default is FALSE}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{cex}{number}

  \item{cex.axis}{number}

  \item{cex.main}{number}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  This function produces a plot of the residuals from
  WRTDS, expressed in natural log concentration units
  versus the estimated values, also in natural log
  concentration units.  These estimates are the log-space
  estimates prior to bias-correction. The function provides
  an alternative for viewing the standardized residuals,
  where the each residual is divided by its estimated
  standard error.
}
\examples{
Sample <- exSample
INFO <- exINFO
plotResidPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


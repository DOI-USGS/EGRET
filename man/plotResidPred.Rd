\name{plotResidPred}
\alias{plotResidPred}
\title{Plot of the residuals from WRTDS versus the estimated values (all in log concentration units)}
\usage{
  plotResidPred(localSample = Sample, localINFO = INFO,
    stdResid = FALSE, tinyPlot = FALSE, printTitle = TRUE,
    cex = 0.8, cex.axis = 1.1, cex.main = 1.1,
    customPar = FALSE, ...)
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

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for x and y
  labels relative to the current setting of cex}

  \item{customPar}{logical defaults to FALSE. If TRUE, par
  should be set by user, if FALSE, EGRET chooses best
  graphical parameters.}

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
Sample <- ChopSample
INFO <- ChopINFO
plotResidPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


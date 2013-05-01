\name{plotResidQ}
\alias{plotResidQ}
\title{Plot of the residuals from WRTDS (in log concentration units) versus the discharge}
\usage{
  plotResidQ(localSample = Sample, localINFO = INFO,
    qUnit = 2, tinyPlot = FALSE, stdResid = FALSE,
    printTitle = TRUE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{stdResid}{logical variable, if TRUE it uses the
  standardized residual, if FALSE it uses the actual,
  default is FALSE}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  This function produces a plot of the residuals from
  WRTDS, expressed in natural log concentration units
  versus the discharge shown on a log scale. The function
  also provides an alternative for viewing the standardized
  residuals, where the each residual is divided by its
  estimated standard error
}
\examples{
Sample <- exSample
INFO <- exINFO
plotResidQ(qUnit=1)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


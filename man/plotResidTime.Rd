\name{plotResidTime}
\alias{plotResidTime}
\title{Plot of the residuals from WRTDS (in log concentration units) versus time}
\usage{
  plotResidTime(localSample = Sample, localINFO = INFO,
    stdResid = FALSE, printTitle = TRUE)
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

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}
}
\description{
  This function produces a plot of the residuals from
  WRTDS, expressed in natural log concentration units
  versus time. It also provides an alternative for viewing
  the standardized residuals, where the each residual is
  divided by its estimated standard error.
}
\examples{
Sample <- exSampleEnd
INFO <- exINFOEnd
plotResidTime()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


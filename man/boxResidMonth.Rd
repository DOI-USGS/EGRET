\name{boxResidMonth}
\alias{boxResidMonth}
\title{A box plot of WRTDS residuals by month}
\usage{
  boxResidMonth(localSample = Sample, localINFO = INFO,
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
  This function produces a boxplot of the residuals from
  WRTDS, expressed in natural log concentration units. It
  provides an alternative for viewing the standardized
  residuals, where the each residual is divided by its
  estimated standard error. The monthly boxplot widths are
  proportional to the square root of the sample size. The
  residuals for a censored value are determined as the
  difference between the natural log of the average of the
  upper and lower.  bounds on the sample value, minus the
  log space estimate of concentration.
}
\examples{
Sample <- exSample
INFO <- exINFO
boxResidMonth()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


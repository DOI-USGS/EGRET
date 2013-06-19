\name{boxResidMonth}
\alias{boxResidMonth}
\title{A box plot of WRTDS residuals by month}
\usage{
  boxResidMonth(localSample = Sample, localINFO = INFO,
    stdResid = FALSE, printTitle = TRUE, las = 2,
    cex = 0.8, cex.axis = 1.1, cex.main = 1.1,
    font.main = 2, tinyPlot = FALSE, customPar = FALSE,
    ...)
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

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{las}{numeric in {0,1,2,3}; the style of axis
  labels}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{font.main}{font to be used for plot main titles}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
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
Sample <- ChopSample
INFO <- ChopINFO
boxResidMonth()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


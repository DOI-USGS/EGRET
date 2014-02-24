\name{boxResidMonth}
\alias{boxResidMonth}
\title{A box plot of WRTDS residuals by month}
\usage{
boxResidMonth(localSample = Sample, localINFO = INFO, stdResid = FALSE,
  las = 1, printTitle = TRUE, cex = 0.8, cex.axis = 1.1,
  cex.main = 1.1, font.main = 2, tinyPlot = FALSE, customPar = FALSE,
  ...)
}
\arguments{
  \item{localSample}{data frame that contains the
  concentration data, default name is Sample}

  \item{localINFO}{data frame that contains the metadata,
  default name is INFO}

  \item{stdResid}{logical variable, if TRUE it uses the
  standardized residual, if FALSE it uses the actual,
  default is FALSE}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{font.main}{font to be used for plot main titles}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function}

  \item{las}{numeric in {0,1,2,3}; the style of axis
  labels}

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
upper and lower.  bounds on the sample value, minus the log
space estimate of concentration.

Although there are a lot of optional arguments to this
function, most are set to a logical default. If your
workspace contains an INFO and Sample dataframes, then the
following R code will produce a plot:
\code{boxResidMonth()}
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
# Water year:
INFO <- setPA()
boxResidMonth()
# Graphs consisting of Jun-Aug
INFO <- setPA(paStart=6,paLong=3)
boxResidMonth()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


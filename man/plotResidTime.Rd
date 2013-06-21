\name{plotResidTime}
\alias{plotResidTime}
\title{Plot of the residuals from WRTDS (in log concentration units) versus time}
\usage{
  plotResidTime(localSample = Sample, localINFO = INFO,
    stdResid = FALSE, printTitle = TRUE, hLine = TRUE,
    tinyPlot = FALSE, col = "black", lwd = 1, cex = 0.8,
    cex.axis = 1.1, cex.main = 1.1, customPar = FALSE, ...)
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

  \item{hLine}{inserts horizontal line at zero}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function
  (for example, adjusting margins with
  par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses
  the best margins depending on tinyPlot.}

  \item{col}{color of points on plot, see ?par 'Color
  Specification'}

  \item{lwd}{number line width}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  This function produces a plot of the residuals from
  WRTDS, expressed in natural log concentration units
  versus time. It also provides an alternative for viewing
  the standardized residuals, where the each residual is
  divided by its estimated standard error.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO and Sample dataframes, then
  the following R code will produce a plot:
  \code{plotResidTime()}
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
plotResidTime()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{plotFluxQ}
\alias{plotFluxQ}
\title{Sample data plot: observed log flux vs log discharge}
\usage{
  plotFluxQ(localSample = Sample, localINFO = INFO,
    qUnit = 2, fluxUnit = 3, tinyPlot = FALSE,
    fluxMax = NA, fluxMin = NA, col = "black", lwd = 1,
    printTitle = TRUE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, customPar = FALSE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration and discharge data,
  default name is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{fluxUnit}{object of fluxUnit class.
  \code{\link{fluxConst}}, or numeric represented the short
  code, or character representing the descriptive name.}

  \item{tinyPlot}{logical variable if TRUE plot is designed
  to fit into a multi-plot array, default is FALSE}

  \item{fluxMax}{numeric specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{fluxMin}{numeric specifying the minimum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

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
  Concentration and discharge data used to compute flux
  come from a data frame named Sample which contains the
  sample data. The metadata come from a data frame named
  INFO.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO and Sample dataframes, then
  the following R code will produce a plot:
  \code{plotLogFluxQ()}
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
plotFluxQ(qUnit = 1, fluxUnit = 1)
plotFluxQ(fluxUnit = 'kgDay')
plotFluxQ()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


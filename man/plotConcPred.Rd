\name{plotConcPred}
\alias{plotConcPred}
\title{Plot of Observed Concentration versus Estimated Concentration}
\usage{
  plotConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, logScale = FALSE, printTitle = TRUE,
    tinyPlot = FALSE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, customPar = FALSE, col = "black",
    lwd = 1, ...)
}
\arguments{
  \item{localSample}{data frame that contains the
  concentration data, default name is Sample}

  \item{localINFO}{data frame that contains the metadata,
  default name is INFO}

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{logScale}{logical, default TRUE, TRUE indicates y
  axis is in log scale, "xy" indicates both x and y in log
  scale, "x" is only x}

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
  Data come from a data frame named Sample which contains
  the Sample Data. The metadata come from a data frame
  named INFO.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO and Sample dataframes, then
  the following R code will produce a plot:
  \code{plotConcPred()}
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
# Water year:
INFO <- setPA()
plotConcPred()
plotConcPred(logScale=TRUE)
# Graphs consisting of Jun-Aug
INFO <- setPA(paStart=6,paLong=3)
plotConcPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{plotLogConcPred}
\alias{plotLogConcPred}
\title{Graph of bserved versus estimated concentration as a log-log graph}
\usage{
  plotLogConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, tinyPlot = FALSE, printTitle = TRUE,
    cex = 0.8, col = "black", lwd = 1, cex.axis = 1.1,
    cex.main = 1.1, oneToOneLine = TRUE, customPar = FALSE,
    ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multi-plot
  figure, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{oneToOneLine}{inserts 1:1 line}

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
  \code{plotLogConcPred()}
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
plotLogConcPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


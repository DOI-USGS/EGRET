\name{plotConcQ}
\alias{plotConcQ}
\title{Plot of Observed Concentration versus Discharge}
\usage{
  plotConcQ(localSample = Sample, localINFO = INFO,
    qUnit = 2, tinyPlot = FALSE, logScale = FALSE,
    concMax = NA, concMin = NA, printTitle = TRUE,
    cex = 0.8, cex.axis = 1.1, cex.main = 1.1,
    rmSciX = FALSE, rmSciY = FALSE, customPar = FALSE,
    col = "black", lwd = 1, ...)
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

  \item{logScale}{logical if TRUE x and y plotted in log
  axis}

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{concMin}{numeric if you want to specify the minimum
  concentration value to display, you can do so with the
  argument concMin, otherwise it will be automatic}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{rmSciX}{logical defaults to FALSE, changes x label
  from scientific to fixed}

  \item{rmSciY}{logical defaults to FALSE, changes y label
  from scientific to fixed}

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
  the sample data. The metadata come from a data frame
  named INFO. Discharge is plotted on a log scale.
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
plotConcQ()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


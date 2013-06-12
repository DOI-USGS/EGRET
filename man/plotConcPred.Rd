\name{plotConcPred}
\alias{plotConcPred}
\title{Plot of Observed Concentration versus Estimated Concentration}
\usage{
  plotConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, logScale = FALSE, printTitle = TRUE,
    tinyPlot = FALSE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, ...)
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
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{...}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Data come from a data frame named Sample which contains
  the Sample Data. The metadata come from a data frame
  named INFO.
}
\examples{
Sample <- exSample
INFO <- exINFO
plotConcPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


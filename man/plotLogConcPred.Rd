\name{plotLogConcPred}
\alias{plotLogConcPred}
\title{Graph of bserved versus estimated concentration as a log-log graph}
\usage{
  plotLogConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, tinyPlot = FALSE, printTitle = TRUE,
    cex.main = 1, oneToOneLine = TRUE, ...)
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

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{oneToOneLine}{inserts 1:1 line}

  \item{\dots}{arbitrary graphical parameters that will be
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
plotLogConcPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{boxConcMonth}
\alias{boxConcMonth}
\title{Box plot of the water quality data by month}
\usage{
  boxConcMonth(localSample = Sample, localINFO = INFO,
    printTitle = TRUE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, las = 1, tinyPlot = FALSE,
    customPar = FALSE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame, default name is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multi-plot
  figure, default is FALSE.}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function}

  \item{las}{numeric in {0,1,2,3}; the style of axis
  labels}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Data come from a data frame named Sample which contains
  the Sample Data. The metadata come from a data frame
  named INFO. These data frames must have been created by
  the dataRetrieval package.
}
\examples{
Sample <- ChopSample
INFO <- ChopINFO
boxConcMonth()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


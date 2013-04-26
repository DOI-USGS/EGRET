\name{boxConcMonth}
\alias{boxConcMonth}
\title{Box plot of the water quality data by month}
\usage{
  boxConcMonth(localSample = Sample, localINFO = INFO,
    printTitle = TRUE, cex = 0.8, cex.axis = 0.9,
    cex.main = 1, las = 2, font.main = 2, ...)
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
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{las}{numeric in {0,1,2,3}; the style of axis
  labels}

  \item{font.main}{font to be used for plot main titles}

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
Sample <- exSample
INFO <- exINFO
boxConcMonth()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


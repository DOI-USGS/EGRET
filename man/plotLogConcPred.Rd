\name{plotLogConcPred}
\alias{plotLogConcPred}
\title{Graph of bserved versus estimated concentration as a log-log graph}
\usage{
  plotLogConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, tinyPlot = FALSE, printTitle = TRUE)
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
}
\description{
  Data come from a data frame named Sample which contains
  the Sample Data. The metadata come from a data frame
  named INFO.
}
\examples{
Sample <- exSampleEnd
INFO <- exINFOEnd
plotLogConcPred()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


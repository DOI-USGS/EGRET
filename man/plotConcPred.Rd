\name{plotConcPred}
\alias{plotConcPred}
\title{Plot of Observed Concentration versus Estimated Concentration}
\usage{
  plotConcPred(localSample = Sample, localINFO = INFO,
    concMax = NA, printTitle = TRUE, ...)
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


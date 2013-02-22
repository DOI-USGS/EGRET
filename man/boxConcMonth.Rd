\name{boxConcMonth}
\alias{boxConcMonth}
\title{Box plot of the water quality data by month}
\usage{
  boxConcMonth(localSample = Sample, localINFO = INFO,
    printTitle = TRUE)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame, default name is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}
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


\name{boxQTwice}
\alias{boxQTwice}
\title{Two box plots side-by-side, discharge on sample days, and discharge on all days}
\usage{
  boxQTwice(localSample = Sample, localDaily = Daily,
    localINFO = INFO, printTitle = TRUE, qUnit = 2)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}
}
\description{
  This function is used to compare the distribution of
  discharges in the sample data set and the discharges in
  the full daily data set. Data come from three data frames
  created by the dataRetrieval package. These are Sample,
  Daily, and INFO.
}
\examples{
Sample <- exSampleStart
Daily <- exDailyStart
INFO <- exINFOStart
boxQTwice()
boxQTwice(qUnit=1)
boxQTwice(qUnit='cfs')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


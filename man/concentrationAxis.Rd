\name{concentrationAxis}
\alias{concentrationAxis}
\title{Axis generation for concentration}
\usage{
  concentrationAxis(ConcLow, ConcHigh, ConcAve, ConcMax,
    ConcMin, padPercent = 5)
}
\arguments{
  \item{ConcLow}{vector specifying ConcLow}

  \item{ConcHigh}{vector}

  \item{ConcAve}{vector}

  \item{ConcMax}{number}

  \item{ConcMin}{number}

  \item{padPercent}{number}
}
\description{
  Concentration axis tick generation
}
\examples{
Sample <- exSample
ConcMin <- min(Sample$ConcAve)
ConcMax <- max(Sample$ConcAve)
with(Sample, concentrationAxis(ConcLow, ConcHigh, ConcAve, ConcMax, ConcMin))
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


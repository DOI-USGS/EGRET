\name{dischargeYAxis}
\alias{dischargeYAxis}
\title{Axis generation for log discharge}
\usage{
  dischargeYAxis(qMax, qActual, qUnit, runoff = FALSE,
    tinyPlot = FALSE, yPlotMin = 0, padPercent = 5)
}
\arguments{
  \item{qMax}{number}

  \item{qActual}{vector}

  \item{yPlotMin}{number}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{padPercent}{number}

  \item{tinyPlot}{logical}

  \item{runoff}{logical}
}
\description{
  Discharge axis tick generation
}
\examples{
Daily <- exDaily
qUnit <- 1
qActual <- Daily$Q
qMax <- max(qActual)
dischargeYAxis(qMax,qActual,qUnit)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


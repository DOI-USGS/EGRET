\name{dischargeLogAxis}
\alias{dischargeLogAxis}
\title{Axis generation for log discharge}
\usage{
  dischargeLogAxis(x, qUnit, padPercent = 5,
    tinyPlot = FALSE)
}
\arguments{
  \item{x}{vector specifying discharge}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{padPercent}{number}

  \item{tinyPlot}{logical}
}
\description{
  Discharge axis tick generation
}
\examples{
Daily <- exDaily
qUnit <- 1
dischargeLogAxis(Daily$Q,qUnit)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


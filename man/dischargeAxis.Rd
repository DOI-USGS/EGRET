\name{dischargeAxis}
\alias{dischargeAxis}
\title{Axis generation for discharge}
\usage{
  dischargeAxis(x, qUnit, tinyPlot = FALSE)
}
\arguments{
  \item{x}{vector specifying discharge}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{tinyPlot}{logical}
}
\description{
  Discharge axis tick generation
}
\examples{
Daily <- exDaily
qUnit <- 1
dischargeAxis(Daily$Q,qUnit)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


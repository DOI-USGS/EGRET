\name{generalAxis}
\alias{generalAxis}
\title{Axis generation for log discharge}
\usage{
<<<<<<< HEAD
  generalAxis(x, max, min, log = FALSE, tinyPlot = FALSE,
    padPercent = 5)
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
=======
  generalAxis(x, maxVal, minVal, logScale = FALSE,
    tinyPlot = FALSE, padPercent = 5)
}
\arguments{
  \item{x}{vector to create scale about}

  \item{maxVal}{number maximum value on returned scale}

  \item{minVal}{number minimum value on returned scale}

  \item{logScale}{logical whether or not to return a log
  scale}

  \item{tinyPlot}{logical}

  \item{padPercent}{number used to pad the max and min if
  not specified}
>>>>>>> canon_master/master
}
\description{
  Discharge axis tick generation
}
\examples{
Daily <- exDaily
x <- Daily$Q
max <- max(x)
min <- 0
generalAxis(x, max, min)
min <- min(x)
generalAxis(x, max, min, log=TRUE)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{plotQTimeDaily}
\alias{plotQTimeDaily}
\title{Plot of the discharge time series}
\usage{
  plotQTimeDaily(startYear, endYear, localDaily = Daily,
    localINFO = INFO, qLower = NA, qUnit = 1,
    tinyPlot = FALSE, printTitle = TRUE, lwd = 3,
    col = "red", cex.main = 1.2, cex.lab = 1.2,
    customPar = FALSE, ...)
}
\arguments{
  \item{startYear}{numeric indicating the starting year for
  the graph}

  \item{endYear}{numeric indicating the ending year for the
  graph (should be a time in decimal years that is after
  the last observations to be plotted)}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qLower}{numeric specifying the lower bound on
  discharges that are to be plotted, must be in the units
  specified by qUnit, default is NA (lower bound is zero)}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.  Default is qUnit=1
  (cubic feet per second)}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be short and wide, default is FALSE.}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}

  \item{lwd}{line width, a positive number, defaulting to
  1}

  \item{col}{specification for the default plotting color}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.lab}{magnification to be used for x and y
  labels relative to the current setting of cex}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function
  (for example, adjusting margins with
  par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses
  the best margins depending on tinyPlot.}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Part of flowHistory component. Allows discharge record to
  only show those discharges above a given threshold
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
plotQTimeDaily(1990,2000,qLower=10)
}
\keyword{graphics}
\keyword{streamflow}


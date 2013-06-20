\name{plotFourStats}
\alias{plotFourStats}
\title{Makes four graphs of annual streamflow statistics on a single page}
\usage{
  plotFourStats(localINFO = INFO,
    localAnnualSeries = annualSeries, yearStart = NA,
    yearEnd = NA, printTitle = TRUE, runoff = FALSE,
    qUnit = 1, cex.lab = 1.4, cex.axis = 1.2, cex = 0.6,
    col = "black", lwd = 1, ...)
}
\arguments{
  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, defoult name is INFO}

  \item{localAnnualSeries}{string specifying the name of
  the data frame that contains the annual series of
  statistics, default is annualSeries}

  \item{yearStart}{A numeric value for year in which the
  graph should start, default is NA, which indicates that
  the graph should start with first annual value}

  \item{yearEnd}{A numeric value for year in which the
  graph should end, default is NA, which indicates that the
  graph should end with last annual value}

  \item{printTitle}{logical variable, if TRUE title is
  printed, if FALSE title is not printed, default is TRUE}

  \item{runoff}{logical variable, if TRUE the streamflow
  data are converted to runoff values in mm/day}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{col}{color of points on plot, see ?par 'Color
  Specification'}

  \item{lwd}{number line width}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Part of the flowHistory system.  The four statistics are
  1-day maximum, annual mean, annual median, and annual
  7-day minimum.  Prior to running this code user must have
  run setPA and makeAnnualSeries.
}
\examples{
INFO <- ChopINFO
Daily <- ChopDaily
annualSeries <- makeAnnualSeries()
plotFourStats()
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


\name{plotFlowSingle}
\alias{plotFlowSingle}
\title{Creates a plot of a time series of a particular flow statistic and a lowess smooth of that flow statistic}
\usage{
  plotFlowSingle(istat, yearStart = NA, yearEnd = NA,
    localINFO = INFO, localAnnualSeries = annualSeries,
    qMax = NA, printTitle = TRUE, tinyPlot = FALSE,
    runoff = FALSE, qUnit = 1, printStaName = TRUE,
    printPA = TRUE, printIstat = TRUE, cex = 0.8,
    cex.axis = 1.1, cex.main = 1.1, lwd = 2, ...)
}
\arguments{
  \item{istat}{A numeric value for the flow statistic to be
  graphed (possible values are 1 through 8)}

  \item{yearStart}{A numeric value for year in which the
  graph should start, default is NA, which indicates that
  the graph should start with first annual value}

  \item{yearEnd}{A numeric value for year in which the
  graph should end, default is NA, which indicates that the
  graph should end with last annual value}

  \item{localINFO}{A character string specifying the name
  of the metadata data frame}

  \item{localAnnualSeries}{A character string specifying
  the name of a data frame containing the annual series}

  \item{qMax}{A numeric value for the maximum value to be
  used for y-axis of graph, default is NA means that graph
  is self-scaling}

  \item{printTitle}{logical variable, if TRUE title is
  printed, if FALSE title is not printed, default is TRUE}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{runoff}{logical variable, if TRUE the streamflow
  data are converted to runoff values in mm/day}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{printStaName}{logical variable, if TRUE station
  name is printed in title, if FALSE not printed, default
  is TRUE}

  \item{printPA}{logical variable, if TRUE Period of
  Analysis information is printed in title, if FALSE not
  printed, default is TRUE}

  \item{printIstat}{logical variable, if TRUE print the
  statistic name is printed in title, if FALSE not printed,
  default is TRUE}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{lwd}{number}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  A part of the flowHistory system. The index of the flow
  statistics is istat.  These statistics are: (1) 1-day
  minimum, (2) 7-day minimum, (3) 30-day minimum, (4)
  median (5) mean, (6) 30-day maximum, (7) 7-day maximum,
  and (8) 1-day maximum
}
\examples{
INFO <- ChopINFO
Daily <- ChopDaily
annualSeries <- makeAnnualSeries()
plotFlowSingle(1)
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


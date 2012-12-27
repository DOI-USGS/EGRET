\name{plotDiffContours}
\alias{plotDiffContours}
\title{Plots the difference between two years from a contour plot created by plotContours}
\usage{
  plotDiffContours(year0, year1, qBottom, qTop, maxDiff,
    whatSurface = 3, localsurfaces = surfaces,
    localINFO = INFO, localDaily = Daily, qUnit = 2,
    span = 60, pval = 0.05, printTitle = TRUE, vert1 = NA,
    vert2 = NA, horiz = NA, flowDuration = TRUE)
}
\arguments{
  \item{year0}{numeric value for the calendar year that is
  the first year of the pair of years for the analysis,
  should be a whole number}

  \item{year1}{numeric value for the calendar year that is
  the second year of the pair of years for the analysis,
  should be a whole number}

  \item{qBottom}{numeric value for the bottom edge of the
  graph, expressed in the units of discharge that are being
  used (as specified in qUnit)}

  \item{qTop}{numeric value for the top edge of the graph,
  expressed in the units of discharge that are being used
  (as specified in qUnit)}

  \item{maxDiff}{numeric value which is the absolute value
  of the largest change in concentration that will be shown
  on the figure}

  \item{whatSurface}{numeric value, can only accept 1, 2,
  or 3; whatSurface=1 is yHat (log concentration),
  whatSurface=2 is SE (standard error of log
  concentration), and whatSurface=3 is ConcHat (unbiased
  estimate of concentration), default = 3}

  \item{localsurfaces}{string specifying the name of the
  matrix that contains the estimated surfaces, default is
  surfaces}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{localDaily}{string specifying the name of the data
  frame that contains the daily data, default name is
  Daily}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{span}{numeric, it is the half-width (in days) of
  the smoothing window for computing the flow duration
  information, default = 60}

  \item{pval}{numeric, the probability value for the lower
  flow frequency line on the graph}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed}

  \item{vert1}{numeric, the location in time for a black
  vertical line on the figure, yearStart<vert1<yearEnd,
  default is NA (vertical line is not drawn)}

  \item{vert2}{numeric, the location in time for a black
  vertical line on the figure, yearStart<vert2<yearEnd,
  default is NA (vertical line is not drawn)}

  \item{horiz}{numeric, the location in discharge for a
  black horizontal line on the figure, qBottom<vert1<qTop,
  default is NA (no horizontal line is drawn)}

  \item{flowDuration}{logical variable if TRUE plot the
  flow duration lines, if FALSE do not plot them, default =
  TRUE}
}
\description{
  These plots are normally used for plotting changes in the
  estimated concentration surface (whatSurface=3) but can
  be used to explore the changes in estimated surfaces for
  the log of concentration or for the standard error (in
  log space) which is what determines the bias correction.
}
\examples{
year0<-2001
year1<-2009
qBottom<-0.2
qTop<-20
maxDiff<-0.5
surfaces <- exsurfaces
INFO <- exINFOEnd
Daily <- exDailyEnd
plotDiffContours(year0,year1,qBottom,qTop,maxDiff)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


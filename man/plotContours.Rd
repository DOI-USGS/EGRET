\name{plotContours}
\alias{plotContours}
\title{Color contour plot of the estimated surfaces as a function of discharge and time (surfaces include log concentration, standard error, and concentration)}
\usage{
  plotContours(yearStart, yearEnd, qBottom, qTop,
    whatSurface = 3, localsurfaces = surfaces,
    localINFO = INFO, localDaily = Daily, qUnit = 2,
    contourLevels = NA, span = 60, pval = 0.05,
    printTitle = TRUE, vert1 = NA, vert2 = NA, horiz = NA,
    flowDuration = TRUE)
}
\arguments{
  \item{yearStart}{numeric value for the starting date for
  the graph, expressed as decimal year (typically whole
  number such as 1989.0)}

  \item{yearEnd}{numeric value for the ending date for the
  graph, expressed as decimal year, (for example 1993.0)}

  \item{qBottom}{numeric value for the bottom edge of the
  graph, expressed in the units of discharge that are being
  used (as specified in qUnit)}

  \item{qTop}{numeric value for the top edge of the graph,
  expressed in the units of discharge that are being used
  (as specified in qUnit)}

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

  \item{contourLevels}{numeric vector containing the
  contour levels for the contour plot, arranged in
  ascending order, default is NA (which causes the contour
  levels to be set automatically, based on the data)}

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
  These plots are normally used for plotting the estimated
  concentration surface (whatSurface=3) but can be used to
  explore the estimated surfaces for the log of
  concentration or for the standard error (in log space)
  which is what determines the bias correction. The plots
  are often more interpretable when the time limits are
  only about 4 years apart. To explore changes over a long
  time period it is best to do this multiple times, for
  various time slices of 4 years (for example) or to use
  the function plotDiffContours.
}
\examples{
yearStart<-2001
yearEnd<-2010
qBottom<-0.2
qTop<-20
clevel<-seq(0,2,0.5)
plotContours(yearStart,yearEnd,qBottom,qTop, localsurfaces = exsurfaces, localINFO = exINFOEnd, localDaily = exDailyEnd, contourLevels = clevel)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


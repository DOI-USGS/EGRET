\name{plot1of15}
\alias{plot1of15}
\title{plots 1 of the 15 graphs of streamflow statistics on a single page}
\usage{
  plot1of15(yearStart, yearEnd, qf, istat,
    localAnnualSeries, localINFO, isBottom = FALSE)
}
\arguments{
  \item{yearStart}{A numeric value for the year in which
  the graph should start}

  \item{yearEnd}{A numeric value for the year in which the
  graph should end}

  \item{qf}{a scale factor to convert discharge in cubic
  feet per second to mm/day}

  \item{istat}{A numeric value selecting the flow statistic
  to be plotted, must be an integer from 1 to 8}

  \item{localAnnualSeries}{string indicating the name of
  the data frame that contains the annual series data for
  all 8 statistics for the given period of analysis}

  \item{localINFO}{string indicating the name of the data
  frame that contains the meta data}

  \item{isBottom}{logical, if TRUE the graph is from the
  bottom row and thus needs x axis labels, if FALSE it does
  not need labels}
}
\description{
  Part of the flowHistory system.  The 15 graphs include
  annual and four seasonal graphs for each of 3 flow
  statistics: 1-day maximum, mean, and 7-day minimum
}
\examples{
plot1of15(1990,2000,0.2938476,5,localAnnualSeries=exannualSeries,localINFO=exINFOEnd)
}
\keyword{graphics}
\keyword{streamflow}


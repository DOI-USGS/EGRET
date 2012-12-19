\name{estSurfaces}
\alias{estSurfaces}
\title{Estimate the three surfaces (for yHat, SE and ConcHat) as a function of DecYear and logQ and store in the three-dimensional object called surfaces}
\usage{
  estSurfaces(localDaily = Daily, localSample = Sample,
    windowY = 10, windowQ = 2, windowS = 0.5,
    minNumObs = 100, minNumUncen = 50)
}
\arguments{
  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}

  \item{localSample}{string specifying the name of the data
  frame containing the sample values, default is Sample}

  \item{windowY}{numeric specifying the half-window width
  in the time dimension, in units of years, default is 10}

  \item{windowQ}{numeric specifying the half-window width
  in the discharge dimension, units are natural log units,
  default is 2}

  \item{windowS}{numeric specifying the half-window with in
  the seasonal dimension, in units of years, default is
  0.5}

  \item{minNumObs}{numeric specifying the miniumum number
  of observations required to run the weighted regression,
  default is 100}

  \item{minNumUncen}{numeric specifying the minimum number
  of uncensored observations to run the weighted
  regression, default is 50}
}
\value{
  surfaces array containing the three surfaces estimated,
  array is 3 dimensional
}
\description{
  This function uses weighted survival regression to
  estimate three surfaces that cover the complete range of
  DecYear and log(Q) values in the Daily data set. These
  surfaces are: (1) is the estimated log concentration
  (yHat), (2) is the estimated standard error (SE), (3) is
  the estimated concentration (ConcHat). They are mapped as
  an array that covers the complete space of daily
  discharge and time. The first index is discharge, layed
  out in 14 equally spaced levels of log(Q). The second
  index is time, layed out as 16 increments of the calendar
  year, starting January 1.  It returns the 3 dimensional
  array called surfaces.  This array will be used to
  estimate these 3 quantities for any given day in the
  daily values record.
}
\examples{
estSurfaces(localDaily = exDailyStart, localSample = exSampleStart)
}
\keyword{statistics}
\keyword{water-quality}


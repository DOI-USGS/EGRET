\name{estCrossVal}
\alias{estCrossVal}
\title{Jack-Knife cross validation of the WRTDS (Weighted Regressions on Time, Discharge, and Season)}
\usage{
  estCrossVal(localSample = Sample, windowY = 10,
    windowQ = 2, windowS = 0.5, minNumObs = 100,
    minNumUncen = 50)
}
\arguments{
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
  SampleCrossV data frame containing the sample data
  augmented by the results of the cross-validation exercise
}
\description{
  This function fits the WRTDS model n times (where n is
  the number of observations). For each fit, the data value
  being estimated is eliminated from the record. This gives
  predictions that do not depend on knowing the actual
  result for that day. Thus it provides for a more "honest"
  estimate of model performance than a traditional error
  analysis that uses all the data.
}
\examples{
Sample <- exSample
SampleCrossV <- estCrossVal()
}
\keyword{statistics}
\keyword{water-quality}


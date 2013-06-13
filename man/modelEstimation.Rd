\name{modelEstimation}
\alias{modelEstimation}
\title{Estimation process for the WRTDS (Weighted Regressions on Time, Discharge, and Season)}
\usage{
  modelEstimation(localDaily = Daily, localSample = Sample,
    localINFO = INFO, windowY = 10, windowQ = 2,
    windowS = 0.5, minNumObs = 100, minNumUncen = 50,
    env = parent.frame())
}
\arguments{
  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}

  \item{localSample}{string specifying the name of the data
  frame containing the sample values, default is Sample}

  \item{localINFO}{string specifying the name of the data
  frame containing the metadata, default is INFO}

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

  \item{env}{environment to set variables in}
}
\description{
  This one function does a jack-knife cross-validation of a
  WRTDS model, fits the surface (concentration as a
  function of discharge and time), estimates daily values
  of concentration and flux, and flow normalized values. It
  returns several data frames or matrices (Daily, INFO,
  Sample, AnnualResults, and surfaces).
}
\examples{
Daily <- ChopDaily
Sample <- ChopSample
INFO <- ChopINFO
\dontrun{modelEstimation()}
}
\keyword{statistics}
\keyword{water-quality}


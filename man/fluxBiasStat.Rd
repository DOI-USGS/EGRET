\name{fluxBiasStat}
\alias{fluxBiasStat}
\title{Compute the flux bias statistic: (mean of estimated flux - mean of observed flux)  / mean of observed flux}
\usage{
  fluxBiasStat(localSample = Sample)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}
}
\value{
  fluxBias a vector of three numerical values, a lower
  bound, upper bound and an average estimate of the ratio
  of (mean estimated flux - mean observed flux) / mean
  estimated flux.  Typically one should use fluxBias[3]
}
\description{
  Computes three versions of the flux bias: The first where
  all censored values are set to their miniumum.  The
  second where all censored values are set to their
  maximum.  The third which is the average of the other
  two.  In practice there is rarely a noticable difference
  among them.
}
\examples{
fluxBiasStat(localSample = exSampleEnd)
}
\keyword{bias}
\keyword{statistics,}
\keyword{water-quality}


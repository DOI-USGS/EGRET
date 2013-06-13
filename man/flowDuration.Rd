\name{flowDuration}
\alias{flowDuration}
\title{Computes several values of the flow duration curve for streamflow centered on a specific date of the year}
\usage{
  flowDuration(centerDate = "09-30", localDaily = Daily,
    localINFO = INFO, qUnit = 2, span = 365)
}
\arguments{
  \item{centerDate}{string specifying the center date of
  the part of the year for which the flow duration is to be
  calculated, it is in the form "mm-dd" (it must be in
  quotes), default is "09-30"}

  \item{localDaily}{string specifying the name of the data
  frame that contains the daily discharge data, default
  name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{span}{number this is the half-width of the window
  over which the discharge values are to be used in
  constructing the flow-duration curve. If the full year is
  desired any value greater than 182 will provide serve.
  Note that for a window of about 2-months width, a span
  value shoud be about 30.}
}
\description{
  This function is useful for helping the analyst determine
  the empirical probability distribution of streamflow for
  a particular part of the year or for the whole year. This
  is particularly useful in setting up discharge scales for
  various other plots in this package.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
flowDuration("06-25", qUnit=1,span=30) # for a window of 30 days either side of June 25 expressed in units of cubic feet per second
flowDuration("01-01", qUnit=2) # for a flow-duration curve covering the whole year, expressed in units of cubic meters per second
}
\keyword{statistics}
\keyword{streamflow,}


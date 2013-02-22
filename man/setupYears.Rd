\name{setupYears}
\alias{setupYears}
\title{Creates the AnnualResults data frame from the Daily data frame}
\usage{
  setupYears(paLong = 12, paStart = 10, localDaily = Daily)
}
\arguments{
  \item{paLong}{numeric integer specifying the length of
  the period of analysis, in months, 1<=paLong<=12, default
  is 12}

  \item{paStart}{numeric integer specifying the starting
  month for the period of analysis, 1<=paStart<=12, default
  is 10}

  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}
}
\value{
  AnnualResults data frame with one row per year
}
\description{
  This function aggregates the results stored on a daily
  basis in the Daily data frame and stores the average
  values of these in the new data frame called
  AnnualResults.  The "annual values" can be a full 12
  months, or they can be shorter.  See manual to understand
  paLong and paStart arguments.  The simplest case, a Water
  Year (October through September), would have paLong=12,
  and paStart=10.  A calendar year would be paLong=12 and
  paStart=1.  A winter season of Dec, Jan, Feb would be
  paLong=3 and paStart=12
}
\examples{
Daily <- exDaily
setupYears()
}
\keyword{statistics}
\keyword{water-quality}


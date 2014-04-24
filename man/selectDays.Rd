\name{selectDays}
\alias{selectDays}
\title{Creates a subset of the Daily data frame that only contains data for the specified period of analysis}
\usage{
  selectDays(paLong, paStart, localDaily = Daily)
}
\arguments{
  \item{paLong}{a numeric value for the length of the
  period of Analysis, must be an integer from 1 to 12}

  \item{paStart}{a numeric value for the starting month of
  the period of analysis, must be an integer from 1 to 12}

  \item{localDaily}{data frame that contains the daily
  streamflow data, default is Daily}
}
\value{
  localDaily a data frame containing the daily data but
  only for the period of analysis (not all months)
}
\description{
  Function is not called by user, but is called by
  plotSDLogQ
}
\examples{
Daily <- ChopDaily
DailySubset <- selectDays(1,3)
}
\keyword{statistics}
\keyword{streamflow}


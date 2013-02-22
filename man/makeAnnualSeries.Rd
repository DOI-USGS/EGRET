\name{makeAnnualSeries}
\alias{makeAnnualSeries}
\title{Produces annual series of 8 streamflow statistics (and a lowess smooth of them) from daily streamflow data}
\usage{
  makeAnnualSeries(localDaily = Daily, localINFO = INFO)
}
\arguments{
  \item{localDaily}{string specifying the name of a data
  frame that contains the daily streamflow data}

  \item{localINFO}{string specifying the name of a data
  frame that contains the metadata}
}
\value{
  annualSeries data frame that contains the annual series
  of streamflow statistics
}
\description{
  Part of the flowHistory system.  The data come from Daily
  and INFO data frames. Note that the function setPA must
  be run before this to establish the period of analysis
  (e.g. water year).
}
\examples{
Daily <- exDaily
INFO <- exINFO
annualSeries <- makeAnnualSeries()
}
\keyword{statistics}
\keyword{streamflow}
\keyword{trends}


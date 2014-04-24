\name{blankTime}
\alias{blankTime}
\title{Deletes the computed values during periods of time when there is no sample data}
\usage{
blankTime(startBlank, endBlank, localDaily = Daily)
}
\arguments{
  \item{startBlank}{string specifying starting date of
  blank period, input in quotes in yyyy-mm-dd format}

  \item{endBlank}{string specifying the ending date of
  blank period, input in quotes in yyyy-mm-dd format}

  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}
}
\value{
localDaily Daily data frame returned with NA's in sample
gap
}
\description{
This function is used when the data analyst believes that a
gap in the sample data record is so long that estimates
during that period are not reliable.  This is only unsed
for periods of several years in duration.  For this period,
the values of Conc, Flux, FNConc and FNFlux are all
converted to NA.
}
\examples{
startBlank = "2004-10-01"
endBlank = "2006-09-30"
Daily <- ChopDaily
Daily <- blankTime(startBlank, endBlank)
}
\keyword{statistics}
\keyword{water-quality}


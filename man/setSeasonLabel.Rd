\name{setSeasonLabel}
\alias{setSeasonLabel}
\title{Create a character string that describes the period of analysis, when period of analysis has already been set in AnnualResults}
\usage{
  setSeasonLabel(localAnnualResults = AnnualResults)
}
\arguments{
  \item{localAnnualResults}{string specifying the name of
  the data frame that contains the annual results, default
  is AnnualResults}
}
\value{
  periodName string which describes the period of analysis
}
\description{
  The period of analysis can be of any length from 1 month
  to 12 months. The period of analysis can have any
  starting month from 1 (January) through 12 (December).
  This function produces a character string that describes
  this period of analysis. For example "water year",
  "calendar year", "year starting with April", or "Season
  consisting of April, May, June". There is an alternative
  version of this function for the case where AnnualResults
  does not exist. This might arise in a call from
  plotConcTime or plotLogConcTime. That function is called
  setSeasonLabelByUser.
}
\examples{
setSeasonLabel(localAnnualResults = exAnnualResults)
}
\keyword{graphics}
\keyword{quality}
\keyword{water}


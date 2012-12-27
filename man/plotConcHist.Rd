\name{plotConcHist}
\alias{plotConcHist}
\title{Graph of annual concentration and flow normalized concentration versus year}
\usage{
  plotConcHist(yearStart = NA, yearEnd = NA,
    localAnnualResults = AnnualResults, localINFO = INFO,
    concMax = NA, printTitle = TRUE, plotFlowNorm = TRUE)
}
\arguments{
  \item{yearStart}{numeric is the calendar year containing
  the first estimated annual value to be plotted, default
  is NA (which allows it to be set automatically by the
  data)}

  \item{yearEnd}{numeric is the calendar year just after
  the last estimated annual value to be plotted, default is
  NA (which allows it to be set automatically by the data)}

  \item{localAnnualResults}{string specifying the name of
  the data frame that contains the annual results, default
  name is AnnualResults}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}

  \item{plotFlowNorm}{logical variable if TRUE flow
  normalized line is plotted, if FALSE not plotted}
}
\description{
  Data come from a data frame named AnnualResults. The
  metadata come from a data frame named INFO. The annual
  concentrations are "time-weighted" mean concentrations
  (as opposed to "flow-weighted"). The annual results
  reported are for a specified "period of analysis" which
  can be an entire water year, a calendar, a season or even
  an individual month. User specifies this period of
  analysis in the call to setupYears.
}
\examples{
yearStart <- 2001
yearEnd <- 2010
INFO <- exINFOEnd
AnnualResults <- exAnnualResults
plotConcHist(yearStart, yearEnd)
}
\seealso{
  \code{\link{setupYears}}
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


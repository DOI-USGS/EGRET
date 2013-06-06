\name{plotFluxHist}
\alias{plotFluxHist}
\title{Graph of annual flux and flow normalized flux versus year}
\usage{
  plotFluxHist(yearStart = NA, yearEnd = NA, fluxUnit = 9,
    localAnnualResults = AnnualResults, localINFO = INFO,
    fluxMax = NA, printTitle = TRUE, plotFlowNorm = TRUE,
    tinyPlot = FALSE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, lwd = 2, ...)
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

  \item{fluxUnit}{number representing entry in pre-defined
  fluxUnit class array. \code{\link{fluxConst}}}

  \item{fluxMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}

  \item{plotFlowNorm}{logical variable if TRUE the flow
  normalized line is plotted, if FALSE not plotted}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{cex}{number}

  \item{cex.axis}{number}

  \item{cex.main}{number}

  \item{lwd}{number}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Data come from a data frame named AnnualResults. The
  metadata come from a data frame named INFO. The annual
  results reported are for a specified "period of analysis"
  which can be an entire water year, a calendar, a season
  or even an individual month. The user specifies this
  period of analysis in the call to setupYears.
}
\examples{
yearStart <- 2001
yearEnd <- 2010
AnnualResults <- exAnnualResults
INFO <- exINFO
plotFluxHist(yearStart, yearEnd, fluxUnit = 1)
plotFluxHist(yearStart, yearEnd, fluxUnit = 'kgDay')
}
\seealso{
  \code{\link{setupYears}}
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{plotConcHist}
\alias{plotConcHist}
\title{Graph of annual concentration and flow normalized concentration versus year}
\usage{
  plotConcHist(yearStart = NA, yearEnd = NA,
    localAnnualResults = AnnualResults, localINFO = INFO,
    concMax = NA, printTitle = TRUE, tinyPlot = FALSE,
    plotFlowNorm = TRUE, cex = 0.8, cex.axis = 1.1,
    cex.main = 1.1, lwd = 2, customPar = FALSE, ...)
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

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small, as a part of a multipart
  figure, default is FALSE}

  \item{cex}{numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{lwd}{number magnification of line width.}

  \item{customPar}{logical defaults to FALSE. If TRUE, par
  should be set by user, if FALSE, EGRET chooses best
  graphical parameters.}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
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
INFO <- ChopINFO
Daily <- ChopDaily
AnnualResults <- setupYears()
plotConcHist(yearStart, yearEnd)
}
\seealso{
  \code{\link{setupYears}}
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


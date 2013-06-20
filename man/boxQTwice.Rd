\name{boxQTwice}
\alias{boxQTwice}
\title{Two box plots side-by-side, discharge on sample days, and discharge on all days}
\usage{
  boxQTwice(localSample = Sample, localDaily = Daily,
    localINFO = INFO, printTitle = TRUE, qUnit = 2,
    font.main = 2, cex = 0.8, cex.main = 1.1,
    cex.axis = 1.1, tinyPlot = FALSE, customPar = FALSE,
    ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed (this is best for a
  multi-plot figure)}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{font.main}{font to be used for plot main titles}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multi-plot
  figure, default is FALSE.}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  This function is used to compare the distribution of
  discharges in the sample data set and the discharges in
  the full daily data set. Data come from three data frames
  created by the dataRetrieval package: Sample, Daily, and
  INFO. Note that discharge is plotted on a logarithmic
  axis. The data is logged before the statistics are
  performed to determine the output of the boxplot.
}
\examples{
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
boxQTwice()
boxQTwice(qUnit=1)
boxQTwice(qUnit='cfs')
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


\name{multiPlotDataOverview}
\alias{multiPlotDataOverview}
\title{Produces a 4 panel plot that gives an overview of the data set prior to any processing}
\usage{
  multiPlotDataOverview(localSample = Sample,
    localDaily = Daily, localINFO = INFO, qUnit = 2,
    cex.main = 1.2, logScaleConc = TRUE, logScaleQ = TRUE)
}
\arguments{
  \item{localSample}{data frame that contains the
  concentration data, default name is Sample}

  \item{localDaily}{data frame that contains the flow data,
  default name is Daily}

  \item{localINFO}{data frame that contains the metadata,
  default name is INFO}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{logScaleConc}{logical if TRUE y in concentration
  graphs plotted in log axis. Default is TRUE.}

  \item{logScaleQ}{logical if TRUE y in streamflow graphs
  plotted in log axis. Default is TRUE.}
}
\description{
  The four plots produced are 1) log concentration versus
  log discharge, 2) log concentration versus time 3) a
  boxplot of log concentration by month, and 4) a
  side-by-side boxplot of the sampled discharges and all
  daily discharges. To save space, the graphic is labeled
  only at the top of the 4 graph display.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO, Daily, and Sample dataframes,
  then the following R code will produce a plot:
  \code{multiPlotDataOverview()}
}
\examples{
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
# Water year:
INFO <- setPA()
multiPlotDataOverview(qUnit=1)
# Graphs consisting of Jun-Aug
INFO <- setPA(paStart=6,paLong=3)
multiPlotDataOverview(qUnit=1)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


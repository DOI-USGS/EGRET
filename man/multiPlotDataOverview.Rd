\name{multiPlotDataOverview}
\alias{multiPlotDataOverview}
\title{Produces a 4 panel plot that gives an overview of the data set prior to any processing}
\usage{
  multiPlotDataOverview(localSample = Sample,
    localDaily = Daily, localINFO = INFO, qUnit = 2)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}
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
multiPlotDataOverview(qUnit=1)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


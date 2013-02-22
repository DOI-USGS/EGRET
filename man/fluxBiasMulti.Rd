\name{fluxBiasMulti}
\alias{fluxBiasMulti}
\title{Produces a 6-panel plot that is useful for determining if there is a flux bias problem}
\usage{
  fluxBiasMulti(localSample = Sample, localDaily = Daily,
    localINFO = INFO, qUnit = 2, fluxUnit = 3,
    moreTitle = "")
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localDaily}{string specifying the name of the data
  frame that contains the flow data, default name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{fluxUnit}{object of fluxUnit class.
  \code{\link{fluxConst}}, or numeric represented the short
  code, or character representing the descriptive name.}

  \item{moreTitle}{string specifying some additional
  information to go in figure title, typically some
  information about the specific estimation method used,
  default is no additional information}
}
\description{
  These plots use the jack-knife estimates from WRTDS to
  investigate the potential flux bias problem. It can also
  be used for estimates constructed by other methods (such
  as LOADEST) if the results are stored in a data frame
  organized like the Sample data frame.  It allows
  additional label information to indicate what method is
  used. The 6 graphs are: Log Concentration versus Log
  Discharge, Residual verus Log Discharge, Log
  Concentration versus Log Estimated Concentration
  (estimates made prior to bias adjustment), Residuals
  versus Estimates (in log concentration space), Observed
  Flux versus Estimated Flux (2 plots, one in log space and
  the other in real space).
}
\examples{
Sample <- exSample
Daily <- exDaily
INFO <- exINFO

fluxBiasMulti(fluxUnit = 9,moreTitle="WRTDS")
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


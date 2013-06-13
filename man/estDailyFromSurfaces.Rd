\name{estDailyFromSurfaces}
\alias{estDailyFromSurfaces}
\title{Estimates all daily values of Concentration, Flux, Flow Normalized Concentration, and Flow Normalized Flux}
\usage{
  estDailyFromSurfaces(localDaily = Daily,
    localINFO = INFO, localsurfaces = surfaces)
}
\arguments{
  \item{localDaily}{string specifying the name of the data
  frame containing the daily values, default is Daily}

  \item{localINFO}{string specifying the name of the data
  frame containing the meta-data, default is INFO}

  \item{localsurfaces}{string specifying the name of the
  array containing the three surfaces, default is surfaces}
}
\value{
  localDaily string specifying the name of the data frame
  containing the daily values and these estimates
}
\description{
  Uses the surfaces estimated in estSurfaces to estimate
  these four time series in addition to the time series for
  standard error and yHat (estimated log concentration).
  The results are stored in an augmented version of the
  Daily data frame, which is returned.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
INFO <- setPA()
surfaces <- exsurfaces
#################################################
# This is usually done in modelEstimation:
surfaceIndexParameters<-surfaceIndex()
INFO$bottomLogQ<-surfaceIndexParameters[1]
INFO$stepLogQ<-surfaceIndexParameters[2]
INFO$nVectorLogQ<-surfaceIndexParameters[3]
INFO$bottomYear<-surfaceIndexParameters[4]
INFO$stepYear<-surfaceIndexParameters[5]
INFO$nVectorYear<-surfaceIndexParameters[6]
#################################################
Daily <- estDailyFromSurfaces()
}
\keyword{statistics}
\keyword{water-quality}


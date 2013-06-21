\name{boxConcThree}
\alias{boxConcThree}
\title{Three box plots side-by-side}
\usage{
  boxConcThree(localSample = Sample, localDaily = Daily,
    localINFO = INFO, tinyPlot = FALSE, printTitle = TRUE,
    moreTitle = "WRTDS", customPar = FALSE, font.main = 2,
    cex = 0.8, cex.main = 1.1, cex.axis = 1.1, ...)
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

  \item{font.main}{font to be used for plot main titles}

  \item{moreTitle}{string specifying some additional
  information to go in figure title, typically some
  information about the specific estimation method used,
  default is no additional information}

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
  concentration in the sample and predicted data set. Data
  come from three data frames created by the dataRetrieval
  package.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO, Daily, and Sample dataframes,
  then the following R code will produce a plot:
  \code{boxConcThree()}
}
\examples{
Sample <- ChopSample
Daily <- ChopDaily
INFO <- ChopINFO
boxConcThree()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


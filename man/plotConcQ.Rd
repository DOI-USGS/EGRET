\name{plotConcQ}
\alias{plotConcQ}
\title{Plot of Observed Concentration versus Discharge}
\usage{
  plotConcQ(localSample = Sample, localINFO = INFO,
    qUnit = 2, tinyPlot = FALSE, concMax = NA,
    printTitle = TRUE, ...)
}
\arguments{
  \item{localSample}{string specifying the name of the data
  frame that contains the concentration data, default name
  is Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{qUnit}{object of qUnit class \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{concMax}{number specifying the maximum value to be
  used on the vertical axis, default is NA (which allows it
  to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure)}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  Data come from a data frame named Sample which contains
  the sample data. The metadata come from a data frame
  named INFO. Discharge is plotted on a log scale.
}
\examples{
Sample <- exSample
INFO <- exINFO
plotConcQ()
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


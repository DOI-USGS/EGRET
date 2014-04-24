\name{tableChange}
\alias{tableChange}
\title{Create a table of the changes in flow-normalized values between various points in time in the record}
\usage{
  tableChange(localDaily = Daily, localINFO = INFO,
    fluxUnit = 9, yearPoints = NA)
}
\arguments{
  \item{localDaily}{data frame that contains the flow data,
  default name is Daily}

  \item{localINFO}{data frame that contains the metadata,
  default name is INFO}

  \item{fluxUnit}{object of fluxUnit class.
  \code{\link{fluxConst}}, or numeric represented the short
  code, or character representing the descriptive name.}

  \item{yearPoints}{numeric vector listing the years for
  which the change or slope computations are made, they
  need to be in chronological order.  For example
  yearPoints=c(1975,1985,1995,2005), default is NA (which
  allows the program to set yearPoints automatically)}
}
\description{
  These tables describe trends in flow-normalized
  concentration and in flow-normalized flux. They are
  described as changes in real units or in percent and als
  as slopes in real units per year or in percent per year.
  They are computed over pairs of time points.  These time
  points can be user-defined or they can be set by the
  program to be the final year of the record and a set of
  years that are multiple of 5 years prior to that.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
# Water Year:
tableChange(fluxUnit=6,yearPoints=c(2001,2005,2008,2009))
tableChange(fluxUnit=9)
# Winter:
INFO <- setPA(paStart=12,paLong=3)
tableChange(fluxUnit=6,yearPoints=c(2001,2005,2008,2009))
}
\keyword{statistics}
\keyword{water-quality}


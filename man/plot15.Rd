\name{plot15}
\alias{plot15}
\title{Makes 15 graphs of streamflow statistics on a single page}
\usage{
  plot15(yearStart, yearEnd, localDaily = Daily,
    localINFO = INFO)
}
\arguments{
  \item{yearStart}{A numeric value for year in which the
  graph should start, default is NA, which indicates that
  the graph should start with first annual value}

  \item{yearEnd}{A numeric value for year in which the
  graph should end, default is NA, which indicates that the
  graph should end with last annual value}

  \item{localDaily}{string specifying the name of the data
  frame that contains the daily data, default is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default is INFO}
}
\description{
  Part of flowHistory system.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
pdf("plot15.pdf",heigh=10,width=8)
plot15(yearStart=1990,yearEnd=2000)
dev.off()
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


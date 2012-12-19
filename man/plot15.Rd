\name{plot15}
\alias{plot15}
\title{Makes 15 graphs of streamflow statistics on a single page}
\usage{
  plot15(savePath, yearStart, yearEnd, localDaily = Daily,
    localINFO = INFO)
}
\arguments{
  \item{savePath}{string specifying the full pathname of
  the folder where the file is to be saved ending with the
  final slash}

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
  Part of flowHistory system.  This function saves a
  graphic as a postscript file.  It assigns the file a name
  using the abbreviations for station.
}
\examples{
\dontrun{plot15("",yearStart=1990,yearEnd=2000,localDaily=exDailyStart,localINFO=exINFOStart)}
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


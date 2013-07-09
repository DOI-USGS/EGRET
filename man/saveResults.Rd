\name{saveResults}
\alias{saveResults}
\title{A utility program for saving the contents of the workspace}
\usage{
  saveResults(savePath, localINFO = INFO)
}
\arguments{
  \item{savePath}{string specifying the full pathname of
  the folder where the file is to be saved ending with the
  final slash}

  \item{localINFO}{string specifying the name of the INFO
  database}
}
\description{
  This function saves the workspace.  It assigns the file a
  name using the abbreviations for station and constituent.
}
\examples{
INFO <- ChopINFO
INFO$staAbbrev <- "Chop"
INFO$constitAbbrev <- "nitrogen"
Daily <- ChopDaily
Sample <- ChopSample
surfaces <- exsurfaces
savePath <- "C:/RData/"
saveResults(savePath)

#To load:
load(paste(savePath,"Chop.nitrogen.RData",sep=""))
}
\keyword{statistics}
\keyword{water-quality}


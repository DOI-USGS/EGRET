\name{tableChangeSingle}
\alias{tableChangeSingle}
\title{Create a table of the changes in flow-normalized concentration or flux values between various points in time in the record}
\usage{
tableChangeSingle(localDaily = Daily, localINFO = INFO, fluxUnit = 9,
  yearPoints = NA, returnDataFrame = FALSE, flux = FALSE)
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

  \item{flux}{logical if TRUE results are returned in flux,
  if FALSE concentration. Default is set to FALSE.}

  \item{returnDataFrame}{logical, if a dataframe is
  required to be returned set this to TRUE.  Otherwise, the
  default is FALSE}
}
\value{
resultsDF dataframe describing trends in flow-normalized
concentration or flux if returnDataFrame is TRUE

dataframe with Year1, Year2, change[mg/L], slope[mg/L],
change[percent], slope[percent] columns. The data in each
row is the change or slope calculated from Year1 to Year2
}
\description{
This table describe trends in flow-normalized concentration
or flux depending on if flux is defined as TRUE or FALSE.
The results are described as changes in real units or in
percent and als as slopes in real units per year or in
percent per year. They are computed over pairs of time
points (Year1 to Year2).  These time points can be
user-defined or they can be set by the program to be the
final year of the record and a set of years that are
multiple of 5 years prior to that.
}
\examples{
Daily <- ChopDaily
INFO <- ChopINFO
# Water Year:
tableChangeSingle(fluxUnit=6,yearPoints=c(2001,2005,2008,2009), flux=FALSE)  #This returns concentration ASCII table in the console
tableChangeSingle(fluxUnit=6,yearPoints=c(2001,2005,2008,2009), flux=TRUE)  #This returns flux values ASCII table in the console
tableChangeConc <-tableChangeSingle(fluxUnit=9, returnDataFrame = TRUE, flux=FALSE)    #This returns concentration values in a dataframe
tableChangeFlux <-tableChangeSingle(fluxUnit=9, returnDataFrame = TRUE, flux=TRUE)  #This returns flux values in a dataframe
# Winter:
INFO <- setPA(paStart=12,paLong=3)
tableChangeSingle(fluxUnit=6,yearPoints=c(2001,2005,2008,2009), flux=FALSE)
}
\keyword{statistics}
\keyword{water-quality}


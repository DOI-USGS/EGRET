\name{plotSDLogQ}
\alias{plotSDLogQ}
\title{Graph of the standard deviation of the log of daily discharge versus year}
\usage{
  plotSDLogQ(yearStart = NA, yearEnd = NA, window = 15,
    localDaily = Daily, localINFO = INFO, sdMax = NA,
    printTitle = TRUE, tinyPlot = FALSE,
    printStaName = TRUE, printPA = TRUE)
}
\arguments{
  \item{yearStart}{numeric is the calendar year of the
  first value to be included in graph, default is NA, which
  plots from the start of the period of record}

  \item{yearEnd}{numeric is the calendar year of the last
  value to be included in graph, default is NA, which plots
  to the end of the period of record}

  \item{window}{numeric which is the full width, in years,
  of the time window over which the standard deviation is
  computed, default = 15}

  \item{localDaily}{string specifying the name of the data
  frame that contains the daily streamflow data, default
  name is Daily}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{sdMax}{numeric is the maximum value to be used on
  the vertical axis of the graph, default is NA (which
  allows it to be set automatically by the data)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE title is not printed (this is best for
  a multi-plot figure), default is TRUE}

  \item{tinyPlot}{logical variable if TRUE plot is designed
  to be small, if FALSE it is designed for page size,
  default is FALSE (not fully implemented yet)}

  \item{printStaName}{logical variable, if TRUE print the
  station name, if FALSE do not, default is TRUE}

  \item{printPA}{logical variable, if TRUE print the period
  of analysis information in the plot title, if FALSE leave
  it out, default is TRUE}
}
\description{
  Data come from data frame named Daily. The metadata come
  from a data frame named INFO. User must have already run
  the function, INFO<-setPA() Can be used to analyze a
  specific period of analysis by Running
  INFO<-setPA(paStart,paLong)
}
\examples{
Daily <- exDaily
INFO <- exINFO
plotSDLogQ(window=3,printTitle=FALSE)
}
\keyword{graphics}
\keyword{statistics}
\keyword{streamflow}


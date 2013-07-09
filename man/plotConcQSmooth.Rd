\name{plotConcQSmooth}
\alias{plotConcQSmooth}
\title{Plot up to three curves representing the concentration versus discharge relationship. Each curve is a different point in time.}
\usage{
  plotConcQSmooth(date1, date2, date3, qLow, qHigh,
    qUnit = 2, legendLeft = 0, legendTop = 0, concMax = NA,
    concMin = NA, bw = FALSE, printTitle = TRUE,
    printValues = FALSE, localSample = Sample,
    localINFO = INFO, colors = c("black", "red", "green"),
    printLegend = TRUE, windowY = 10, windowQ = 2,
    windowS = 0.5, tinyPlot = FALSE, customPar = FALSE,
    lwd = 2, cex = 0.8, cex.axis = 1.1, cex.main = 1.1,
    cex.legend = 1.2, lineVal = c(1, 1, 1),
    logScale = FALSE, ...)
}
\arguments{
  \item{date1}{string specifying the date for the first
  curve on the graph, it is in the form "yyyy-mm-dd" (must
  be in quotes)}

  \item{date2}{string specifying the date for the second
  curve on the graph, it is in the form "yyyy-mm-dd" (must
  be in quotes).  If only one curve is wanted this should
  be NA}

  \item{date3}{string specifying the date for the third
  curve on the graph, it is in the form "yyyy-mm-dd" (must
  be in quotes).  If a third curve is not wanted this
  should be NA}

  \item{qLow}{numeric value for the lowest discharge to be
  considered, expressed in the units of discharge that are
  being used (as specified in qUnit)}

  \item{qHigh}{numeric value for the highest discharge to
  be considered, expressed in the units of discharge that
  are being used (as specified in qUnit)}

  \item{qUnit}{object of qUnit class. \code{\link{qConst}},
  or numeric represented the short code, or character
  representing the descriptive name.}

  \item{legendLeft}{numeric which represents the left edge
  of the legend in the units of the plot.}

  \item{legendTop}{numeric which represents the top edge of
  the legend in the units of the plot.}

  \item{printLegend}{logicalif TRUE, legend is included}

  \item{concMax}{numeric value for upper limit on
  concentration shown on the graph, default = NA (which
  causes the upper limit to be set automatically, based on
  the data)}

  \item{concMin}{numeric value for lower limit on
  concentration shown on the graph, default = NA (which
  causes the lower limit to be set automatically, based on
  the data)}

  \item{bw}{logical if TRUE graph is produced in black and
  white, default is FALSE (which means it will use color)}

  \item{printTitle}{logical variable if TRUE title is
  printed, if FALSE not printed}

  \item{printValues}{logical variable if TRUE the results
  shown on the graph are also printed to the console and
  returned in a dataframe (this can be useful for
  quantifying the changes seen visually in the graph),
  default is FALSE (not printed)}

  \item{localSample}{string specifying the name of the data
  frame that contains the Sample data, default name is
  Sample}

  \item{localINFO}{string specifying the name of the data
  frame that contains the metadata, default name is INFO}

  \item{windowY}{numeric specifying the half-window width
  in the time dimension, in units of years, default is 10}

  \item{windowQ}{numeric specifying the half-window width
  in the discharge dimension, units are natural log units,
  default is 2}

  \item{windowS}{numeric specifying the half-window with in
  the seasonal dimension, in units of years, default is
  0.5}

  \item{cex}{numerical value giving the amount by which
  plotting symbols should be magnified}

  \item{cex.main}{magnification to be used for main titles
  relative to the current setting of cex}

  \item{cex.axis}{magnification to be used for axis
  annotation relative to the current setting of cex}

  \item{lwd}{number line width}

  \item{cex.legend}{magnification to be used for legend
  annotation relative to the current setting of cex}

  \item{tinyPlot}{logical variable, if TRUE plot is
  designed to be plotted small as part of a multipart
  figure, default is FALSE.}

  \item{logScale}{logical whether or not to use a log scale
  in the y axis.}

  \item{customPar}{logical defaults to FALSE. If TRUE,
  par() should be set by user before calling this function
  (for example, adjusting margins with
  par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses
  the best margins depending on tinyPlot.}

  \item{colors}{color vector of lines on plot, see ?par
  'Color Specification'. Defaults to
  c("black","red","green")}

  \item{lineVal}{vector of line types. Defaults to c(1,1,1)
  which is a solid line for each line. Options: 0=blank,
  1=solid (default), 2=dashed, 3=dotted, 4=dotdash,
  5=longdash, 6=twodash}

  \item{\dots}{arbitrary graphical parameters that will be
  passed to genericEGRETDotPlot function (see ?par for
  options)}
}
\description{
  These plots are like a vertical slice of the estimated
  concentration surface that is seen in the plotContours
  function. These plots show how the
  concentration-discharge relationship is changing over
  time. Typically the time points selected would be in
  three years at the same time of year spaced out over the
  period of record.  But that is not necessary. Another
  possibility is to use this to explore seasonal
  differences.  In this case the three dates would be in
  the same year but different times during the year.

  Although there are a lot of optional arguments to this
  function, most are set to a logical default. If your
  workspace contains an INFO and Sample dataframes, 3
  dates, and the discharge lower and upper limits, then the
  following R code will produce a plot:
  \code{plotConcQSmooth(date1,date2,date3,qLow,qHigh)}
}
\examples{
date1<-"2001-06-01"
date2<-"2005-06-01"
date3<-"2010-06-01"
qLow<-1
qHigh<-100
Sample <- ChopSample
INFO <- ChopINFO
plotConcQSmooth(date1,date2,date3,qLow,qHigh)
}
\keyword{graphics}
\keyword{statistics}
\keyword{water-quality}


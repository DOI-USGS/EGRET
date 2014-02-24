#' Two box plots side-by-side, discharge on sample days, and discharge on all days
#'
#' @description
#' This function is used to compare the distribution of discharges in the sample data set 
#' and the discharges in the full daily data set.
#' Data come from three data frames created by the dataRetrieval package: Sample, Daily, and INFO.
#' Note that discharge is plotted on a logarithmic axis. The data is logged before the statistics are performed
#' to determine the output of the boxplot.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#' contains an INFO, Daily, and Sample dataframes, then the following R code will produce a plot:
#' \code{boxQTwice()}
#'
#' @param localSample data frame that contains the concentration data, default name is Sample
#' @param localDaily data frame that contains the flow data, default name is Daily 
#' @param localINFO data frame that contains the metadata, default name is INFO
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' # Water year:
#' INFO <- setPA()
#' boxQTwice()
#' boxQTwice(qUnit=1)
#' boxQTwice(qUnit='cfs')
#' # Graphs consisting of Jun-Aug
#' INFO <- setPA(paStart=6,paLong=3)
#' boxQTwice()
boxQTwice<-function(localSample = Sample, localDaily = Daily, localINFO = INFO, 
                    printTitle = TRUE, qUnit = 2, cex=0.8,cex.main=1.1, 
                    cex.axis=1.1, tinyPlot = FALSE, customPar=FALSE,...){
  # This function does two boxplots side by side
  # The first is for the discharges on the sampled days
  # The second is for the discharges on all of the days  

  paLong <- localINFO$paLong
  paStart <- localINFO$paStart  
  
  localSample <- if(paLong == 12) localSample else selectDays(paLong,paStart,localDaily=localSample)
  localDaily <- if(paLong == 12) localDaily else selectDays(paLong,paStart,localDaily=localDaily)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  ################################################################################
  # I plan to make this a method, so we don't have to repeat it in every funciton:
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  ################################################################################
  
  qFactor<-qUnit@qUnitFactor
  bigQ<-c(localSample$Q*qFactor,localDaily$Q*qFactor)
  nS<-length(localSample$Q)
  nD<-length(localDaily$Q)
  index1<-rep(1,nS)
  index2<-rep(2,nD)
  index<-c(index1,index2)

  
  plotTitle<-if(printTitle) paste(localINFO$shortName,",",localINFO$paramShortName,"\nComparison of distribution of\nSampled Discharges and All Daily Discharges") else ""
 
  yMin <- 0.99 * min(bigQ)
  yMax <- 1.01 * max(bigQ)
  
  if (tinyPlot) {
    yLabel <- qUnit@qUnitTiny
    if (!customPar) par(mar=c(4,5,1,0.1),tcl=0.5,cex.lab=cex.axis)
    groupNames<-c("Sampled","All")
    yTicks <- logPretty1(yMin,yMax)
  } else {
    yLabel <- qUnit@qUnitExpress
    if (!customPar) par(mar=c(5,6,4,2)+0.1,tcl=0.5,cex.lab=cex.axis)
    groupNames<-c("Sampled Days","All Days")
    yTicks <- logPretty1(yMin,yMax)
  }
    
  numYTicks <- length(yTicks)
  yBottom <- yTicks[1]
  yTop <- yTicks[numYTicks]
  
  boxplot(log(bigQ,10)~index,varwidth=TRUE,
          names=groupNames,xlab="",ylab=yLabel,
          ylim=c(log(yBottom,10),log(yTop,10)),
          main=plotTitle,cex=cex,
          cex.main=cex.main,
          cex.axis=cex.axis, las=1,yaxt = "n",yaxs="i",
          ...)
  axis(2, tcl = 0.5, las = 1, at = log(yTicks,10), labels = yTicks, cex.axis=cex.axis, cex=cex.axis)
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  
}
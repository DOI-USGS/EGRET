#' Two box plots side-by-side, discharge on sample days, and discharge on all days
#'
#' This function is used to compare the distribution of discharges in the sample data set 
#' and the discharges in the full daily data set.
#' Data come from three data frames created by the dataRetrieval package. 
#' These are Sample, Daily, and INFO.
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param font.main font to be used for plot main titles
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' boxQTwice()
#' boxQTwice(qUnit=1)
#' boxQTwice(qUnit='cfs')
boxQTwice<-function(localSample = Sample, localDaily = Daily, localINFO = INFO, 
                    printTitle = TRUE, qUnit = 2, font.main=2, cex=0.8,cex.main=1.1, 
                    cex.axis=1.1, tinyPlot = FALSE,...){
  # This function does two boxplots side by side
  # The first is for the discharges on the sampled days
  # The second is for the discharges on all of the days  

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
    yLabel <- paste("Discharge (",gsub(" ","",qUnit@qShortName),")",sep="")
    par(mar=c(4,5,1,0.1),tcl=0.5,cex.lab=cex.axis)
    groupNames<-c("Sampled","All")
    yTicks <- logPretty1(yMin,yMax)
  } else {
    yLabel <- paste("Discharge in ",qUnit@qUnitName,sep="")
    par(mar=c(5,6,4,2)+0.1,tcl=0.5,cex.lab=cex.axis)
    groupNames<-c("Sampled Days","All Days")
    yTicks <- logPretty1(yMin,yMax)
  }
    
  numYTicks <- length(yTicks)
  yBottom <- yTicks[1]
  yTop <- yTicks[numYTicks]
  
  boxplot(log(bigQ,10)~index,varwidth=TRUE,
          names=groupNames,xlab="",ylab=yLabel,
          ylim=c(log(yBottom,10),log(yTop,10)),
          main=plotTitle,font.main=font.main,cex=cex,
          cex.main=cex.main,
          cex.axis=cex.axis, las=1,yaxt = "n",yaxs="i",
          ...)
  axis(2, tcl = 0.5, las = 1, at = log(yTicks,10), labels = yTicks, cex.axis=cex.axis, cex=cex.axis)
  
}
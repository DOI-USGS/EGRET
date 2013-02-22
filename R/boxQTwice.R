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
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' Daily <- exDaily
#' INFO <- exINFO
#' boxQTwice()
#' boxQTwice(qUnit=1)
#' boxQTwice(qUnit='cfs')
boxQTwice<-function(localSample = Sample, localDaily = Daily, localINFO = INFO, printTitle = TRUE, qUnit = 2){
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
  yLab<-paste("Discharge in",qUnit@qUnitName)
  groupNames<-c("Sampled Days","All Days")
  plotTitle<-if(printTitle) paste(localINFO$shortName,",",localINFO$paramShortName,"\nComparison of distribution of\nSampled Discharges and All Daily Discharges") else ""
  boxplot(bigQ~index,log="y",varwidth=TRUE,names=groupNames,xlab="",ylab=yLab,main=plotTitle,font.main=2,cex.main=1.0,cex.axis=0.8)
}
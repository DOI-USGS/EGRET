#' Three box plots side-by-side
#'
#' @description
#' This function is used to compare the distribution of concentration in the sample and predicted data set.
#' Data come from three data frames created by the dataRetrieval package. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#' contains an INFO, Daily, and Sample dataframes, then the following R code will produce a plot:
#' \code{boxConcThree()}
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localDaily string specifying the name of the data frame that contains the flow data, default name is Daily 
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param font.main font to be used for plot main titles
#' @param moreTitle string specifying some additional information to go in figure title, typically some information about the specific estimation method used, default is no additional information
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
#' boxConcThree()
boxConcThree<-function (localSample = Sample, localDaily = Daily, localINFO = INFO, tinyPlot=FALSE,
                        printTitle = TRUE, moreTitle = "WRTDS",customPar=FALSE,
                        font.main=2,cex=0.8,cex.main = 1.1, cex.axis = 1.1,...) 
{
  
  nS <- length(localSample$ConcAve)
  nD <- length(localDaily$ConcDay)
  index1 <- rep(1, nS)
  index2 <- rep(2, nS)
  index3 <- rep(3, nD)
  index <- c(index1, index2,index3)
  concV <- c(localSample$ConcAve,localSample$ConcHat,localDaily$ConcDay)
  
  plotTitle <- if (printTitle) {
    paste(localINFO$shortName, ",", localINFO$paramShortName, 
          "\nComparison of distribution of sampled concentrations\nwith estimates on sampled days and on all days using ",moreTitle)
  } else {
    ""
  }
  
  yMax<-max(concV,na.rm=TRUE)
  yTicks<-yPretty(yMax)
  yTop<-yTicks[length(yTicks)]
  
  if (tinyPlot) {
    yLab <- paste("Conc. (mg/L)")
    if (!customPar) par(mar=c(4,5,1,0.1),tcl=0.5,cex.lab=cex.axis)  

  } else {
    yLab <- paste("Concentration in mg/L")
    if (!customPar) par(mar=c(5,6,4,2)+0.1,tcl=0.5,cex.lab=cex.axis)

  }
  name1 <- "Sampled day\nvalues"
  name2 <- "Sampled day\nestimates"
  name3 <- "All day\nestimates"
  groupNames <- c(name1,name2,name3)
  
  boxplot(concV ~ index,varwidth=TRUE,
          names=groupNames,xlab="",ylab=yLab,
          ylim=c(0,yTop),axes=FALSE,
          main=plotTitle,font.main=font.main,cex=cex,
          cex.main=cex.main,
          las=1,yaxs="i",
          ...)
  
  axis(1,tcl=0.5,at=c(1,2,3),labels=groupNames,cex.axis=cex.axis*0.5454)
  axis(2,tcl=0.5,las=1,at=yTicks,cex.axis=cex.axis)
  axis(3,tcl=0.5,at=c(1,2,3),labels=FALSE)
  axis(4,tcl=0.5,at=yTicks,labels=FALSE)
  box()

}
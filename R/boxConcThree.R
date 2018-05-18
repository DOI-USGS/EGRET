#' Three box plots side-by-side
#'
#' @description
#' This function is used to compare the distribution of concentration in the 
#' sample and predicted data set. It shows three boxplots.  One for the sample, 
#' one for the predictions on days with sample values, and one for all days 
#' (whether or not they had sample values).
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param font.main font to be used for plot main titles
#' @param moreTitle character specifying some additional information to go in figure title, typically some information about the specific estimation method used, default is no additional information
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multi-plot figure, default is FALSE.
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' boxConcThree(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' boxConcThree(eList)
boxConcThree<-function (eList, tinyPlot=FALSE,
                        printTitle = TRUE, moreTitle = "WRTDS",customPar=FALSE,
                        font.main=2,cex=0.8,cex.main = 1.1, cex.axis = 1.1,...){
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample,paLong,paStart)
  localDaily <- if(paLong == 12) localDaily else selectDays(localDaily, paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  nS <- length(localSample$ConcAve)
  nD <- length(localDaily$ConcDay)
  index1 <- rep(1, nS)
  index2 <- rep(2, nS)
  index3 <- rep(3, nD)
  index <- c(index1, index2,index3)
  
  plotTitle <- if (printTitle) {
    paste(localINFO$shortName, ",", localINFO$paramShortName, 
          "\nComparison of distribution of sampled concentrations\nwith estimates on sampled days and on all days using ",moreTitle)
  } else {
    ""
  }
  
  if (tinyPlot) {
    yLab <- paste("Conc. (",localINFO$param.units,")",sep="")
    if (!customPar) par(mar=c(4,5,1,0.1),tcl=0.5,cex.lab=cex.axis)  

  } else {
    yLab <- paste("Concentration in",localINFO$param.units)
    if (!customPar) par(mar=c(5,6,4,2)+0.1,tcl=0.5,cex.lab=cex.axis)

  }
  name1 <- "Sampled\nvalues"
  name2 <- "Sampled\nestimates"
  name3 <- "All\nestimates"
  groupNames <- c(name1,name2,name3)
  
  # if(!rResid){
    concV <- c(localSample$ConcAve,localSample$ConcHat,localDaily$ConcDay)
  
  # } else {
  #   if(!("rObserved" %in% names(localSample))){
  #     eList <- makeAugmentedSample(eList)
  #     localSample <- eList$Sample
  #   }
  #   concV <- c(localSample$rObserved,localSample$ConcHat,localDaily$ConcDay)
  # }
  
  yMax<-max(concV,na.rm=TRUE)
  yTicks<-yPretty(yMax)
  yTop<-yTicks[length(yTicks)]
  
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
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  invisible(eList)
}
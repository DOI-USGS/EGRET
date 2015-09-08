#' A box plot of WRTDS residuals by month
#'
#' @description
#' This function produces a boxplot of the residuals from WRTDS, expressed in natural log concentration units. 
#' It provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error. 
#' The monthly boxplot widths are proportional to the square root of the sample size. 
#' The residuals for a censored value are determined as the difference between the natural log of the average of the upper and lower. 
#'   bounds on the sample value, minus the log space estimate of concentration. 
#'   
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param font.main font to be used for plot main titles
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' @param las numeric in {0,1,2,3}; the style of axis labels
#' @param rResid logical option to plot randomized residuals.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' boxResidMonth(eList)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' boxResidMonth(eList)
boxResidMonth<-function(eList, stdResid = FALSE, las=1,
                        printTitle = TRUE, cex=0.8, cex.axis=1.1, cex.main=1.1,
                        font.main=2, tinyPlot=FALSE, customPar=FALSE,rResid=FALSE,...) {
  
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample, paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  if (tinyPlot){
    if (!customPar) par(mar=c(4,5,1,0.1),cex.lab=cex.axis,mgp=c(2.5,0.5,0),tcl=0.5)
    yLab<-if(stdResid) "Standardized Residuals" else "Residuals"
    names <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  } else {
    if (!customPar) par(mar=c(5,6,4,2) + 0.1,cex.lab=cex.axis,mgp=c(3,1,0),tcl=0.5)
    yLab<-if(stdResid) "Standardized Residuals in natural log units" else "Residuals in natural log units"    
    names <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  }
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of residuals by month") else ""
  
  if(!rResid){
    resid<-log(localSample$ConcAve) - localSample$yHat
  } else {
    if(!("rResid" %in% names(localSample))){
      eList <- makeAugmentedSample(eList)
      localSample <- eList$Sample
    }
    resid <- localSample$rResid
  }
  
  if(stdResid) {
    resid<- resid/localSample$SE
  }
  
  singleMonthList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  
  namesListFactor <- factor(singleMonthList, levels=singleMonthList)
  monthList <- as.character(apply(localSample, 1, function(x)  monthInfo[[as.numeric(x[["Month"]])]]@monthAbbrev))
  monthList <- factor(monthList, namesListFactor)

  boxplot(resid ~ monthList,
          varwidth=TRUE,
          xlab="Month",ylab=yLab,
          main=plotTitle,
          names=names,
          cex=cex,
          cex.main=cex.main,
          cex.axis=cex.axis,
          las=las,
          ...)
  abline(h=0)  
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  invisible(eList)
}
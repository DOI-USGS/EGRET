#' A box plot of WRTDS residuals by month
#'
#' This function produces a boxplot of the residuals from WRTDS, expressed in natural log concentration units. 
#' It provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error. 
#' The monthly boxplot widths are proportional to the square root of the sample size. 
#' The residuals for a censored value are determined as the difference between the natural log of the average of the upper and lower. 
#'   bounds on the sample value, minus the log space estimate of concentration. 
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param las numeric in {0,1,2,3}; the style of axis labels
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param font.main font to be used for plot main titles
#' @param customPar logical defaults to FALSE. If TRUE, par should be set by user, if FALSE, EGRET chooses best graphical parameters.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- ChopSample
#' INFO <- ChopINFO
#' boxResidMonth()
boxResidMonth<-function(localSample = Sample, localINFO = INFO, stdResid = FALSE, 
                        printTitle = TRUE, las=2, cex=0.8, cex.axis=1.1, cex.main=1.1,
                        font.main=2, tinyPlot=FALSE, customPar=FALSE,...) {
  #This function makes a boxplot of Residual by month
  #  if stdResid=TRUE, they will be standardized residuals
  #Box width is proportional to the square root of the sample size
  if (tinyPlot){
    if (!customPar) par(mar=c(4,5,1,0.1),cex.lab=cex.axis,mgp=c(2.5,0.5,0))
    yLab<-if(stdResid) "Standardized Residuals" else "Residuals"
    names <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  } else {
    if (!customPar) par(mar=c(5,6,4,2) + 0.1,cex.lab=cex.axis,mgp=c(3,1,0))
    yLab<-if(stdResid) "Standardized Residuals in natural log units" else "Residuals in natural log units"    
    names <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  }
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of residuals by month") else ""
  resid<-log(localSample$ConcAve) - localSample$yHat
  resid<-if(stdResid) resid/localSample$SE else resid
  
  singleMonthList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  
  namesListFactor <- factor(singleMonthList, levels=singleMonthList)
  monthList <- as.character(apply(localSample, 1, function(x)  monthInfo[[as.numeric(x[["Month"]])]]@monthAbbrev))
  monthList <- factor(monthList, namesListFactor)
  
  tempDF <- data.frame(month=monthList, resid=resid)  
  
  boxplot(tempDF$resid ~ tempDF$month,
          varwidth=TRUE,
          xlab="Month",ylab=yLab,
          main=plotTitle,
          names=names,
          cex=cex,
          cex.main=cex.main,
          cex.axis=cex.axis,
          ...)
  abline(h=0)  
}
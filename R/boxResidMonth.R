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
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' boxResidMonth()
boxResidMonth<-function(localSample = Sample, localINFO = INFO, stdResid = FALSE, printTitle = TRUE) {
  #This function makes a boxplot of Residual by month
  #  if stdResid=TRUE, they will be standardized residuals
  #Box width is proportional to the square root of the sample size
  yLab<-if(stdResid) "Standardized Residuals in natural log units" else "Residuals in natural log units"
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\nBoxplots of residuals by month") else ""
  resid<-log(localSample$ConcAve) - localSample$yHat
  resid<-if(stdResid) resid/localSample$SE else resid
  
  singleMonthList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthAbbrev})
  #   singleMonthList <- sapply(c(1:12),function(x){monthInfo[[x]]@monthSingle})
  
  namesListFactor <- factor(singleMonthList, levels=singleMonthList)
  monthList <- as.character(apply(localSample, 1, function(x)  monthInfo[[as.numeric(x[["Month"]])]]@monthAbbrev))
  monthList <- factor(monthList, namesListFactor)
  
  tempDF <- data.frame(month=monthList, resid=resid)  
  
  boxplot(tempDF$resid ~ tempDF$month,
          #           resid~localSample$Month,
          varwidth=TRUE,
          #           names=singleMonthList,
          xlab="Month",ylab=yLab,
          main=plotTitle,
          las=2,
          cex=0.8,cex.axis=0.9,cex.main=1.0,font.main=2)
  abline(h=0)  
}
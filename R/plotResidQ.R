#' Plot of the residuals from WRTDS (in log concentration units) versus the discharge 
#'
#' This function produces a plot of the residuals from WRTDS, expressed in natural log concentration units
#' versus the discharge shown on a log scale. 
#' The function also provides an alternative for viewing the standardized residuals, where the each residual is divided by its estimated standard error
#'
#' @param localSample string specifying the name of the data frame that contains the concentration data, default name is Sample
#' @param localINFO string specifying the name of the data frame that contains the metadata, default name is INFO
#' @param qUnit object of qUnit class \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small as part of a multipart figure, default is FALSE.
#' @param stdResid logical variable, if TRUE it uses the standardized residual, if FALSE it uses the actual, default is FALSE
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param rmSciX logical defaults to FALSE, changes x label from scientific to fixed
#' @param rmSciY logical defaults to FALSE, changes y label from scientific to fixed
#' @param cex numerical value giving the amount by which plotting text and symbols should be magnified relative to the default
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotResidQ(qUnit=1)
plotResidQ<-function (localSample = Sample, localINFO = INFO, qUnit = 2, 
                      tinyPlot = FALSE, stdResid = FALSE, printTitle = TRUE,
                      cex=0.8, cex.axis=1.1,cex.main=1.1,rmSciX=FALSE,rmSciY=FALSE,...) 
{  
   
   if (is.numeric(qUnit)) {
     qUnit <- qConst[shortCode = qUnit][[1]]
   } else if (is.character(qUnit)) {
     qUnit <- qConst[qUnit][[1]]
   }
   
   qFactor <- qUnit@qUnitFactor
   x <- localSample$Q * qFactor
   
   yLow <- log(localSample$ConcLow) - localSample$yHat
   yHigh <- log(localSample$ConcHigh) - localSample$yHat
   
   yLow <- if(stdResid){
     yLow/localSample$SE
   } else {
       yLow
   }
   
   yHigh <- if(stdResid){
     yHigh/localSample$SE
   } else {
       yHigh
   }
   
   Uncen <- localSample$Uncen

   if (tinyPlot){
     xLab <- qUnit@qUnitTiny
     yLab <- ifelse(stdResid, "Standardized Residual", "Residual")
  } else {
     xLab <- qUnit@qUnitExpress
     yLab <- ifelse(stdResid, "Standardized Residual in natural log units", "Residual in natural log units")
  }
  
  plotTitle <- ifelse (printTitle,  paste(localINFO$shortName, "\n", localINFO$paramShortName, 
           "\n", "Residual versus Discharge"), "")
   
   #######################
   
   xInfo <- generalAxis(x=x, minVal=NA, maxVal=NA, logScale=TRUE, tinyPlot=tinyPlot,padPercent=5)   
   yInfo <- generalAxis(x=yHigh, minVal=(min(yLow, na.rm = TRUE) - 0.5), maxVal=(max(yHigh) + 0.1), tinyPlot=tinyPlot)

   genericEGRETDotPlot(x=x, y=yHigh,
                       xTicks=xInfo$ticks, yTicks=yInfo$ticks,hLine=TRUE,
                       xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, yInfo$top),
                       xlab = xLab, ylab = yLab, plotTitle=plotTitle,cex=cex,
                       log = "x", cex.axis=cex.axis,cex.main=cex.main, 
                       tinyPlot=tinyPlot,rmSciX=rmSciX,rmSciY=rmSciY,...
     )

   censoredSegments(yInfo$bottom, yLow, yHigh, x, Uncen )
   
}

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
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' Sample <- exSample
#' INFO <- exINFO
#' plotResidQ(qUnit=1)
plotResidQ<-function (localSample = Sample, localINFO = INFO, qUnit = 2, 
                      tinyPlot = FALSE, stdResid = FALSE, printTitle = TRUE, ...) 
{  
   if(tinyPlot) {
     par(mar=c(5,4,1,1)) 
   }else {
     par(mar=c(5,4,4,2)+0.1)
   }
   
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
   xMin <- 0.95 * min(x)
   xMax <- 1.05 * max(x)
   maxYHigh <- max(yHigh) + 0.1
   minYLow <- min(yLow, na.rm = TRUE) - 0.5
   #xTicks <- logPretty3(xMin, xMax)
   #numXTicks <- length(xTicks)
   #xLeft <- xTicks[1]
   #xRight <- xTicks[numXTicks]
   #ySpan <- c(minYLow, maxYHigh)
   #yTicks <- pretty(ySpan, n = 5)
   #numYTicks <- length(yTicks)
   #yBottom <- yTicks[1]
   #yTop <- yTicks[numYTicks]
   xLab <- qUnit@qUnitExpress
   yLab <- ifelse (stdResid, "Standardized Residual in natural log units", "Residual in natural log units")
   plotTitle <- ifelse (printTitle,  paste(localINFO$shortName, "\n", localINFO$paramShortName, 
           "\n", "Residual versus Discharge"), "")
   
   #######################
   
   xInfo <- generalAxis(x=x, min=xMin, max=xMax, log=TRUE, tinyPlot=tinyPlot)
   
   yInfo <- generalAxis(x=yHigh, min=minYLow, max=maxYHigh, tinyPlot=tinyPlot)

   genericEGRETDotPlot(x=x, y=yHigh,
                       xTicks=xInfo$ticks, yTicks=yInfo$ticks,hLine=TRUE,
                       xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, yInfo$top),
                       xlab = xLab, ylab = yLab, plotTitle=plotTitle,
                       log = "x", ...
     )
   # Laura took out cex.lab = 1.0, cex = 0.4, 

   censoredSegments(yBottom, yLow, yHigh, x, Uncen )

   par(mar=c(5,4,4,2)+0.1)
}
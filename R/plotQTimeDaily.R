#' Plot of the discharge time series
#'
#' @description
#' Part of flowHistory component.
#' Allows discharge record to only show those discharges above a given threshold
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default. If your workspace
#'  contains an INFO and Sample dataframes, then the following R code will produce a plot:
#'  \code{plotQTimeDaily()}
#'
#' @param startYear numeric indicating the starting year for the graph
#' @param endYear numeric indicating the ending year for the graph (should be a time in decimal years that is after the last observations to be plotted)
#' @param localDaily data frame that contains the flow data, default name is Daily 
#' @param localINFO data frame that contains the metadata, default name is INFO
#' @param qLower numeric specifying the lower bound on discharges that are to be plotted, must be in the units specified by qUnit, default is NA (lower bound is zero)
#' @param qUnit object of qUnit class. \code{\link{qConst}}, or numeric represented the short code, or character representing the descriptive name.  Default is qUnit=1 (cubic feet per second)
#' @param tinyPlot logical variable, if TRUE plot is designed to be short and wide, default is FALSE.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @param lwd line width, a positive number, defaulting to 1
#' @param col specification for the default plotting color
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.lab magnification to be used for x and y labels relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow
#' @export
#' @examples
#' Daily <- ChopDaily
#' INFO <- ChopINFO
#' # Water year:
#' INFO <- setPA()
#' plotQTimeDaily()
#' plotQTimeDaily(startYear=1990, endYear=2000,qLower=1000)
#' # Graphs consisting of Jun-Aug
#' INFO <- setPA(paStart=6,paLong=3)
#' plotQTimeDaily()
plotQTimeDaily<-function (startYear=NA, endYear=NA, localDaily = Daily, 
                          localINFO = INFO, qLower = NA, qUnit = 1, 
                          tinyPlot = FALSE, printTitle = TRUE, lwd = 3, col="red", 
                          cex.main = 1.2, cex.lab = 1.2, customPar=FALSE,...){
  
  paLong <- localINFO$paLong
  paStart <- localINFO$paStart  

  localDaily <- if(paLong == 12) localDaily else selectDays(paLong,paStart,localDaily=localDaily)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  #########################################################
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  #############################################################
  qFactor<-qUnit@qUnitFactor

  if (tinyPlot){
    yLab <- qUnit@qUnitTiny
  } else {
    yLab <- qUnit@qUnitExpress
  }
  
  startYear <- if (is.na(startYear)) as.integer(min(localDaily$DecYear,na.rm=TRUE)) else startYear
  endYear <- if (is.na(endYear)) as.integer(max(localDaily$DecYear,na.rm=TRUE)) else endYear
  
  subDaily <- subset(localDaily, DecYear >= startYear)
  subDaily <- subset(subDaily, DecYear <= endYear)
  xDaily <- subDaily$DecYear
  
  yDaily <- qFactor * subDaily$Q
  yMin <- if(is.na(qLower)) 0 else qLower

  line2 <- if(is.na(qLower)) "Daily Discharge" else paste("Daily discharge above a threshold of\n",qLower," ",qUnit@qUnitName,sep="")
  line1 <- localINFO$shortName
  
  plotTitle <- if (printTitle) {
    paste(line1, "\n", line2)
  } else {
    ""
  }

  qBottom <- if(is.na(qLower)) 0 else qLower
  
  xInfo <- generalAxis(x=xDaily, minVal=startYear, maxVal=endYear, tinyPlot=tinyPlot)
  yInfo <- generalAxis(x=yDaily, minVal=yMin, maxVal=1.05*max(yDaily), tinyPlot=tinyPlot,padPercent=0)

  genericEGRETDotPlot(x=xDaily, y=yDaily, 
                      xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                      xlab="", ylab=yLab, customPar=customPar,
                      xTicks=xInfo$ticks, yTicks=yInfo$ticks, tinyPlot=tinyPlot,
                      plotTitle=plotTitle, cex.main=cex.main,cex.lab=cex.lab,
                      type="l",col=col,lwd=lwd, xDate=TRUE,...
  )
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)

}
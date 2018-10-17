#' Color contour plot of the estimated surfaces as a function of discharge and time (surfaces include log concentration, standard error, and concentration)
#'
#' @description
#' These plots are normally used for plotting the estimated concentration surface (whatSurface=3) but can be used to explore the 
#' estimated surfaces for the log of concentration or for the standard error (in log space) which is what determines the bias correction. 
#' The plots are often more interpretable when the time limits are only about 4 years apart.
#' To explore changes over a long time period it is best to do this multiple times, for various time slices of 4 years (for example) or to use the function \code{plotDiffContours}.
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#' 
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Daily and INFO dataframes, and surfaces matrix
#' @param yearStart numeric value for the starting date for the graph, expressed as decimal year (typically whole number such as 1989.0)
#' @param yearEnd numeric value for the ending date for the graph, expressed as decimal year, (for example 1993.0)
#' @param qBottom numeric value for the bottom edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit). NA will choose a "pretty" lower limit nearest to the 5\% of discharge.  If yTicks are specified, then the first value of yTicks becomes the lowest discharge shown on the figure.
#' @param qTop numeric value for the top edge of the graph, expressed in the units of discharge that are being used (as specified in qUnit). NA will choose a "pretty" upper limit nearest to the 95\% of discharge. If yTicks are specified, then the last value of yTicks becomes the highest discharge shown on the figure.
#' @param whatSurface numeric value, can only accept 1, 2, or 3;  whatSurface=1 is yHat (log concentration), whatSurface=2 is SE (standard error of log concentration), and whatSurface=3 is ConcHat (unbiased estimate of concentration), default = 3.
#' @param qUnit object of qUnit class. \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name. 
#' @param contourLevels numeric vector containing the contour levels for the contour plot, arranged in ascending order, default is NA (which causes the contour levels to be set automatically, based on the data)
#' @param span numeric, it is the half-width (in days) of the smoothing window for computing the flow duration information, default = 60
#' @param pval numeric, the probability value for the lower flow frequency line on the graph
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed 
#' @param vert1 numeric, the location in time for a black vertical line on the figure, yearStart<vert1<yearEnd, default is NA (vertical line is not drawn) 
#' @param vert2 numeric, the location in time for a black vertical line on the figure, yearStart<vert2<yearEnd, default is NA (vertical line is not drawn)
#' @param horiz numeric, the location in discharge for a black horizontal line on the figure, qBottom<vert1<qTop, default is NA (no horizontal line is drawn)
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins.
#' @param yTicks vector of yTick labels and marks that will be plotted in log space. (for example yTicks = c(3, 5, 10, 20, 50, 100, 200, 400). The first and last values determine the range of the y axis. If NA, the tick marks will be automatically generated. 
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param tick.lwd line width for axis ticks, default is 1
#' @param lwd numeric, line width of flowDuration curve, default is 2
#' @param tcl numeric, length of tick marks in inches, default is 0.03
#' @param color.palette a function that creates a color palette for the contour plot. Default goes from white to gray to blue to red 
#' using the function \code{colorRampPalette(c("white","gray","blue","red"))}. A few preset options are heat.colors, topo.colors, and terrain.colors.
#' @param \dots arbitrary functions sent to the generic plotting function.  See ?par for details on possible parameters
#' @param flowDuration logical variable if TRUE plot the flow duration lines (5 and 95 flow percentiles), if FALSE do not plot them, default = TRUE
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS complience. It will only change automatically
#' generated labels. 
#' @keywords water-quality statistics graphics
#' @export
#' @examples 
#' yearStart <- 2001
#' yearEnd <- 2010
#' qBottom <- 0.5
#' qTop<- 22
#' clevel <- seq(0,3.5,0.5)
#' eList <- Choptank_eList
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop, contourLevels = clevel)  
#' plotContours(eList, yearStart,yearEnd,qBottom=0.1,qTop=NA, contourLevels = clevel) 
#' yTicksModified <- c(.1,1,10,25)
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
#'       contourLevels = clevel,yTicks=yTicksModified,flowDuration=FALSE)  
#' colors <- colorRampPalette(c("white","red"))
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
#'        contourLevels = clevel,yTicks=yTicksModified,
#'        color.palette=colors,flowDuration=FALSE)
#' colors2 <- heat.colors # Some other options: topo.colors, terrain.colors, cm.colors
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop,
#'        contourLevels = clevel,color.palette=colors2,lwd=2,flowDuration=FALSE)
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
#'        contourLevels = clevel,cex.axis=2,flowDuration=FALSE)
#' par(mar=c(5,8,5,8))
#' plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
#'        contourLevels = clevel,customPar=TRUE,printTitle=FALSE,flowDuration=FALSE)
plotContours<-function(eList, yearStart, yearEnd, qBottom=NA, qTop=NA, whatSurface = 3, 
                       qUnit = 2, contourLevels = NA, span = 60, pval = 0.05,
                       printTitle = TRUE, vert1 = NA, vert2 = NA, horiz = NA, tcl=0.03,
                       flowDuration = TRUE, customPar=FALSE, yTicks=NA,tick.lwd=1,usgsStyle = FALSE,
                       lwd=2,cex.main=1,cex.axis=1,color.palette=colorRampPalette(c("white","gray","blue","red")),...) {
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  nVectorYear <- localINFO$nVectorYear
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }

  if(!customPar){
    par(mgp=c(2.5,0.5,0))
  }
  surfaceName<-c("log of Concentration","Standard Error of log(C)","Concentration")
  j<-3
  j<-if(whatSurface==1) 1 else j
  j<-if(whatSurface==2) 2 else j
  surf<-localsurfaces
  surfaceMin<-min(surf[,,j])
  surfaceMax<-max(surf[,,j])
  surfaceSpan<-c(surfaceMin,surfaceMax)
  contourLevels<-if(is.na(contourLevels[1])) pretty(surfaceSpan,n=5) else contourLevels
  # computing the indexing of the surface, the whole thing, not just the part being plotted
  if(all(c("Year","LogQ") %in% names(attributes(localsurfaces)))){
    x <- attr(localsurfaces, "Year")
    y <- attr(localsurfaces, "LogQ")
  } else {
    x <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    y <- ((1:nVectorLogQ)*stepLogQ) + (bottomLogQ - stepLogQ)
  }
  
  yLQ<-y
  qFactor<-qUnit@qUnitFactor
  y<-exp(y)*qFactor
  numX<-length(x)
  numY<-length(y)
  
  qBottomT <- ifelse(is.na(qBottom), quantile(localDaily$Q, probs = 0.05)*qFactor, qBottom)

  qTopT <- ifelse(is.na(qTop), quantile(localDaily$Q, probs = 0.95)*qFactor, qTop)
  
  if(any(is.na(yTicks))){
    
    if(is.na(qBottom)){
      qBottom <- max(0.9*y[1],qBottomT)
    }
    if(is.na(qTop)){
      qTop <- min(1.1*y[numY],qTopT)
    }
    yTicks <- logPretty3(qBottom,qTop)
  }

  if(yearEnd-yearStart >= 4){
    xSpan<-c(yearStart,yearEnd)
    xTicks<-pretty(xSpan,n=5)
    xlabels <- xTicks
  } else {
    xlabels <- c(as.Date(paste(yearStart,"-01-01",sep="")), as.Date(paste(yearEnd,"-01-01",sep="")))
    xlabels <- pretty(xlabels,n=5)
    xTicksDates <- as.POSIXlt(xlabels)
    years <- xTicksDates$year + 1900 
    day <- xTicksDates$yday
    xTicks <- years + day/365
    xlabels <- format(xlabels, "%Y-%b")
  }
  
  nYTicks<-length(yTicks)
  surfj<-surf[,,j]
  surft<-t(surfj)
  # the next section does the flow duration information, using the whole period of record in Daily, not just the graph period
  plotTitle<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color") else NULL
  if(flowDuration) {
    numDays<-length(localDaily$Day)
    freq<-rep(0,nVectorLogQ)
    durSurf<-rep(0,length(x)*length(y))
    dim(durSurf)<-c(length(x),length(y))
    centerDays<-seq(1,365,22.9)
    centerDays<-floor(centerDays)
    for (ix in 1:16) {
      startDay<-centerDays[ix]-span
      endDay<-centerDays[ix]+span
      goodDays<-seq(startDay,endDay,1)
      goodDays<-ifelse(goodDays>0,goodDays,goodDays+365)
      goodDays<-ifelse(goodDays<366,goodDays,goodDays-365)
      numDays<-length(localDaily$Day)
      isGood <- localDaily$Day %in% goodDays 

      spanDaily<-data.frame(localDaily,isGood)
      spanDaily<-subset(spanDaily,isGood)
      n<-length(spanDaily$Day)
      LogQ<-spanDaily$LogQ
      for(jQ in 1:length(y)) {
        ind<-ifelse(LogQ < yLQ[jQ],1,0)
        freq[jQ]<-sum(ind)/n
      }
      xInd<-seq(ix,numX,16)
      numXind<-length(xInd)
      for(ii in 1:numXind) {
        iX<-xInd[ii]
        durSurf[iX,]<-freq
      }
    }
    plevels<-c(pval,1-pval)
    pct1<-format(plevels[1]*100,digits=2)
    pct2<-format(plevels[2]*100,digits=2)
    plotTitle<- plotTitle<-if(printTitle)paste(localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color\nBlack lines are",pct1,"and",pct2,"flow percentiles")
  }
  # setting up for the possible 3 straight lines to go on the graph
  # if the lines aren't being plotted they are just located outside the plot area
  vectorNone<-c(yearStart,log(yTicks[1],10)-1,yearEnd,log(yTicks[1],10)-1)
  v1<-if(is.na(vert1)) vectorNone else c(vert1,log(yTicks[1],10),vert1,log(yTicks[nYTicks],10))
  v2<-if(is.na(vert2)) vectorNone else c(vert2,log(yTicks[1],10),vert2,log(yTicks[nYTicks],10))
  h1<-if(is.na(horiz)) vectorNone else c(yearStart,log(horiz,10),yearEnd,log(horiz,10))
  
  deltaY <- (log(yTicks[length(yTicks)],10)-log(yTicks[1],10))/25
  deltaX <- (yearEnd-yearStart)/25
  
  yLab<-ifelse(usgsStyle,qUnit@unitUSGS,qUnit@qUnitExpress)
  
  logY <- log(y,10)
  filled.contour(x,log(y,10),surft,levels=contourLevels,xlim=c(yearStart,yearEnd),
                 ylim=c(log(yTicks[1],10),log(yTicks[nYTicks],10)),
                 xaxs="i",yaxs="i",
                 color.palette=color.palette, # ...,
                 plot.axes={
                   
                   width <- grconvertX(par("usr")[2],from="user",to="inches") - grconvertX(par("usr")[1],from="user",to="inches")
                   height <- grconvertY(par("usr")[4],from="user",to="inches") - grconvertY(par("usr")[3],from="user",to="inches")
                   
                   axis(1,tcl=0,at=xTicks,labels=xlabels,cex.axis=cex.axis)
                   axis(2,tcl=0,las=1,at=log(yTicks,10),labels=yTicks,cex.axis=cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels =FALSE,cex.axis=cex.axis)
                   axis(4, tcl = 0, at = log(yTicks, 10), labels=FALSE,cex.axis=cex.axis)
                   if(flowDuration) contour(x,log(y,10),durSurf,add=TRUE,drawlabels=FALSE,levels=plevels,lwd=lwd)
                   segments(v1[1],v1[2],v1[3],v1[4])
                   segments(v2[1],v2[2],v2[3],v2[4])
                   segments(h1[1],h1[2],h1[3],h1[4])
                   
                   segments(xTicks, rep(log(yTicks[1],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[3],from="user",to="inches")+tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[4],from="user",to="inches")-tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(rep(yearStart,length(yTicks)), log(yTicks,10), rep(grconvertX(grconvertX(par("usr")[1],from="user",to="inches")+tcl,from="inches",to="user"),length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   segments(rep(grconvertX(grconvertX(par("usr")[2],from="user",to="inches")-tcl,from="inches",to="user"),length(yTicks)), log(yTicks,10), rep(yearEnd,length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                 },
                  plot.title = {
                    if(printTitle) {
                      title(main = plotTitle, ylab = yLab,cex.main = cex.main)
                    } else {
                      title(main = NULL, ylab = yLab)
                    }
                 }
        )

}
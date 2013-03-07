dischargeYAxis <- function(qMax,qActual,tinyPlot,qUnit, runoff=FALSE, 
                           yPlotMin=0,padPercent=5){
  
  nTicks<-if(tinyPlot) 5 else 8
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  yTop<-if(is.na(qMax)) upperMagnification*max(qActual,na.rm=TRUE) else qMax
  yLow<-if(is.na(yPlotMin)) lowerMagnification*min(qActual,na.rm=TRUE) else yPlotMin
  
  ySpan<-c(yLow,yTop)
  yTicks<-pretty(ySpan,n=nTicks)
  numYTicks<-length(yTicks)
  yBottom<-yTicks[1]
  yTop<-yTicks[numYTicks]
  yLab<-if(runoff) "mm/day" else qUnit@qUnitExpress
  
  return(list(yLab=yLab,yTop=yTop,yBottom=yBottom,yTicks=yTicks))
}
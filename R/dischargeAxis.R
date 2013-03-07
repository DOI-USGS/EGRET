dischargeAxis <- function(){
  xMin<-0.95*min(x)
  xMax<-1.05*max(x)
  xTicks<-if(tinyPlot) logPretty1(xMin,xMax) else logPretty3(xMin,xMax)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  return(list(xMin=xMin,xMax=xMax,xTicks=xTicks,xLeft=xLeft,xLab=xLab))
}